library(tidyverse)
library(readxl)
library(tidytext)
library(nntools)
library(janitor)
library(scales)
library(googleway)
library(progressr)
library(leaflet)
library(viridis)
library(lubridate)
library(hms)

raw_responses = read_excel(
  path = paste0(get_sharepoint_dir(),
                "/Wash Co OR Transit Study - Documents/Shared/02 Engagement/OOH/SurveyMonkey/Data_All_230322/Excel/Washington County Transit Study.xlsx"),
  skip = 2,
  col_names = FALSE
) %>%
  clean_names()

schema_path <- paste0(get_sharepoint_dir(),
                      "/Wash Co OR Transit Study - Documents/Shared/02 Engagement/OOH/wash-co-survey-analysis-schema.xlsx")

respondent_home_zip_ref <- read_rds("data/output/respondent_home_zip_ref.rds")

keep_respondent_home_zip_ref <- respondent_home_zip_ref %>%
  filter(home_zip_zone != "Other States")

email_removal_ref <- read_excel(paste0(
  get_sharepoint_dir(),
  "/Wash Co OR Transit Study - Documents/Shared/02 Engagement/OOH/wash-co-respondent-emails.xlsx"),
  sheet= "r-output-emails") %>%
  clean_names() %>%
  filter(remove == 1)

#excel_sheets(schema_path)
col_reference <- read_excel(path = schema_path, sheet = "col_ref")
questions <- read_excel(path = schema_path, sheet = "questions")
answers <- read_excel(path = schema_path, sheet = "answers")

clean_responses_pre <- raw_responses %>%
  select(x1, x3, x4, x5, x11:last_col()) %>%
  rename(respondent_id = x1,
         start_timestamp = x3, 
         end_timestamp = x4,
         ip_address = x5) %>%
  mutate(across(.cols = x11:last_col(), as.character)) %>%
  pivot_longer(cols = x11:last_col()) %>%
  rename(answer_text = value) %>%
  mutate(col_num = as.numeric(str_replace(name,"x",""))) %>%
  left_join(col_reference) %>%
  filter(!is.na(answer_text)) %>%
  select(-name) %>%
  left_join(answers) %>%
  filter(!(respondent_id %in% c(114260690815)))

# identifying duplicate responses -------------------
dup_detection_data <- clean_responses_pre %>%
  filter(is.na(answer_id)) %>%
  select(respondent_id, 
         start_timestamp, end_timestamp, 
         ip_address, question_id, 
         question_text, answer_text)

timestamp_duplicates <- dup_detection_data %>%
  mutate(start_timestamp = str_sub(as.character(start_timestamp),1,16),
         end_timestamp = str_sub(as.character(end_timestamp),1,16)) %>%
  distinct(respondent_id,start_timestamp,end_timestamp) %>%
  group_by(start_timestamp,end_timestamp) %>%
  mutate(timestamp_count = seq_along(respondent_id)) %>%
  ungroup() 

filtered_timestamp_duplicates <- timestamp_duplicates %>%
  filter(timestamp_count == 1)

open_ended_comment_duplicates <- dup_detection_data %>%
  filter(question_id == 30) %>%
  filter(respondent_id %in% filtered_timestamp_duplicates$respondent_id) %>%
  select(respondent_id,start_timestamp,answer_text) %>%
  mutate(start_timestamp = ymd_hms(start_timestamp)) %>%
  mutate(rounded_timestamp = round_date(start_timestamp,"15 minutes")) 

identified_oc_dups <- open_ended_comment_duplicates %>%
  group_by(answer_text,rounded_timestamp) %>%
  mutate(response_count = seq_along(respondent_id)) %>%
  ungroup() %>%
  filter(response_count > 1)

clean_responses <- clean_responses_pre %>%
  filter(respondent_id %in% filtered_timestamp_duplicates$respondent_id) %>%
  filter(!(respondent_id %in% identified_oc_dups$respondent_id)) %>%
  filter(respondent_id %in% keep_respondent_home_zip_ref$respondent_id) %>%
  filter(!(respondent_id %in% email_removal_ref$respondent_id))

count_all_responses <- length(unique(raw_responses$x1))
count_clean_responses <- length(unique(clean_responses$respondent_id))
percent_kept <- count_clean_responses/count_all_responses

dirty_emails <- raw_responses %>%
  filter(!(x1 %in% clean_responses$respondent_id)) %>%
  select(x1, x153) %>%
  filter(!is.na(x153))

write_csv(dirty_emails,"data/output/invalid-emails-list.csv")

#Extracting Open Ended Responses ----------

open_ended_responses <- clean_responses %>%
  filter(is.na(answer_id)) %>%
  filter(answer_text != "NA",
         answer_text != "No disability") %>%
  filter(!(question_id %in% c(14, 27, 31, 28, 29, 26))) %>%
  select(respondent_id, question_id, question_text,
         header_level_1, header_level_2,
         answer_text) %>%
  arrange(question_id,respondent_id)

write_csv(open_ended_responses, "data/output/open-ended-responses.csv")

# Preparing clean responses --------------

transit_rider_ref <- clean_responses %>%
  filter(question_id == 1) %>%
  mutate(transit_rider_cat = case_when(
    answer_text == "Yes" ~ "Transit Riders",
    answer_text == "No" ~ "Non Transit Riders"
  )) %>%
  select(respondent_id, transit_rider_cat)

rider_color_vec <- c("Transit Riders" = nn_colors("NN Blue"),
                     "Non Transit Riders" = nn_colors("NN Red"))

answer_wrap_width = 90

# Multiple Choice -------------
sub_questions <- questions %>%
  filter(question_type == "Multiple Choice, Single Selection" |
           question_type == "Multiple Choice, Multiple Selection")

for(i in 1:nrow(sub_questions)){
  
  qid = sub_questions$question_id[i]
  qtext = sub_questions$question_text[i]
  qordinal = sub_questions$ordinal[i]
  
  sub_responses <- clean_responses %>%
    filter(question_id == qid) %>%
    filter(other_text == FALSE | is.na(other_text)) %>%
    mutate(answer_text = case_when(
      is.na(other_text) ~ header_level_2,
      TRUE ~ answer_text
    )) %>%
    left_join(transit_rider_ref)
  
  total_num_respondents = n_distinct(sub_responses$respondent_id)
  
  rider_cat_respondent_ref <- sub_responses %>%
    group_by(transit_rider_cat) %>%
    summarise(num_respondents = n_distinct(respondent_id)) %>%
    mutate(legend_label = paste0(transit_rider_cat," (N=",comma(num_respondents),")"))
  
  if(qid == 1){
    ordered_answers <- sub_responses %>%
      group_by(answer_text) %>%
      summarise(num_responses = n()) %>%
      arrange(num_responses) %>%
      ungroup() %>%
      mutate(answer_text = str_wrap(answer_text,answer_wrap_width)) %>%
      pull(answer_text)
    
    summ_responses <- sub_responses %>%
      group_by(answer_text, transit_rider_cat) %>%
      summarise(num_responses = n()) %>%
      ungroup() %>%
      left_join(rider_cat_respondent_ref) %>%
      mutate(prop_respondents = num_responses/total_num_respondents) %>%
      arrange(prop_respondents) %>%
      mutate(answer_text = str_wrap(answer_text,answer_wrap_width)) %>%
      mutate(answer_text = factor(answer_text, ordered = TRUE, levels = unique(answer_text)))
  }else{
    if(qordinal == TRUE){
      ordered_answers <- answers %>%
        filter(question_id == qid) %>%
        select(answer_text) %>%
        pull(answer_text) %>%
        str_wrap(width = answer_wrap_width) %>%
        unique()
      
      summ_responses <- sub_responses %>%
        group_by(answer_text, transit_rider_cat) %>%
        summarise(num_responses = n()) %>%
        ungroup() %>%
        left_join(rider_cat_respondent_ref) %>%
        mutate(prop_respondents = num_responses/num_respondents) %>%
        arrange(prop_respondents) %>%
        mutate(answer_text = str_wrap(answer_text,answer_wrap_width)) %>%
        mutate(answer_text = factor(answer_text, ordered = TRUE, levels = ordered_answers))
    }else{
      ordered_answers <- sub_responses %>%
        group_by(answer_text) %>%
        summarise(num_responses = n()) %>%
        arrange(num_responses) %>%
        ungroup() %>%
        mutate(answer_text = str_wrap(answer_text,answer_wrap_width)) %>%
        pull(answer_text)
      
      summ_responses <- sub_responses %>%
        group_by(answer_text, transit_rider_cat) %>%
        summarise(num_responses = n()) %>%
        ungroup() %>%
        left_join(rider_cat_respondent_ref) %>%
        mutate(prop_respondents = num_responses/num_respondents) %>%
        arrange(prop_respondents) %>%
        mutate(answer_text = str_wrap(answer_text,answer_wrap_width)) %>%
        mutate(answer_text = factor(answer_text, ordered = TRUE, levels = unique(answer_text)))
    }
  }
  
  num_answers = nrow(summ_responses)
  
  ggplot(summ_responses, aes(x = answer_text, y = prop_respondents,
                             fill = transit_rider_cat)) + 
    geom_col(position = position_dodge2(width = 0.9, preserve = "single")) + 
    scale_fill_manual(values = rider_color_vec,
                      labels = rider_cat_respondent_ref$legend_label) +
    geom_label(aes(label = percent(prop_respondents,accuracy = 0.1)),
               alpha = 0.75, color = nn_colors("NN White"),
               show.legend = FALSE,
               position = position_dodge2(width = 0.9, preserve = "single"),
               fontface="bold",
               size=5)+
    coord_flip() +
    nn_basic_theme(base_size = 18, grey_background = FALSE) +
    scale_y_continuous(labels = percent) + 
    labs(
      x = "Answer",
      y = "Proportion of Respondents",
      title = paste0(qtext," (N=", comma(total_num_respondents),")"),
      fill = "Respondent Category"
    )+
    theme(legend.key.height =  unit(0.33, 'cm'),
          legend.key.width = unit(0.33, "cm"))
  
  ggsave(filename = paste0("viz/survey-analysis/q",str_pad(qid,width = 2,side = "left",pad = "0"),".png"),
         height = 1.5 + 0.2*(num_answers+0.5*(nrow(rider_cat_respondent_ref)-1)),
         width = 7, dpi = 300)
  
  print(i)
  
}

# Other text -----------
other_text_responses <- clean_responses %>%
  filter(header_level_2 == "Other (please specify)") %>%
  select(respondent_id,question_id,question_text,header_level_1,header_level_2,
         answer_text)

clipr::write_clip(other_text_responses)

#Open-Ended Comments ----------

sub_responses <- clean_responses %>%
  filter(question_id == 30) %>%
  filter(str_to_lower(answer_text)!="no") %>%
  select(respondent_id,answer_text)

clipr::write_clip(sub_responses)

single_tokens <- sub_responses %>%
  select(respondent_id, answer_text) %>%
  unnest_tokens(word, answer_text) %>%
  anti_join(stop_words)

single_token_summ <- single_tokens %>%
  group_by(word) %>%
  summarise(num_respondents = n_distinct(respondent_id)) %>%
  arrange(desc(num_respondents))

clipr::write_clip(single_token_summ)

double_tokens <- sub_responses %>%
  select(respondent_id, answer_text) %>%
  unnest_tokens(token, answer_text, token = "ngrams",n=2) %>%
  separate(col = token, into = c("word1","word2"),remove = FALSE,sep = " ") %>%
  filter(!(word1 %in% stop_words$word),
         !(word2 %in% stop_words$word))

double_token_summ <- double_tokens %>%
  group_by(token) %>%
  summarise(num_respondents = n_distinct(respondent_id)) %>%
  arrange(desc(num_respondents)) %>%
  filter(!is.na(token))

clipr::write_clip(double_token_summ)

triple_tokens <- sub_responses %>%
  select(respondent_id, answer_text) %>%
  unnest_tokens(token, answer_text, token = "ngrams",n=3) %>%
  separate(col = token, into = c("word1","word2","word3"),remove = FALSE,sep = " ") %>%
  filter(!(word1 %in% stop_words$word),
         !(word2 %in% stop_words$word))

triple_token_summ <- triple_tokens %>%
  group_by(token) %>%
  summarise(num_respondents = n_distinct(respondent_id)) %>%
  arrange(desc(num_respondents)) %>%
  filter(!is.na(token))

clipr::write_clip(triple_token_summ)

# Zip Code Geocoding -------------

# Geocoding home zip codes -----------

# set_key("AIzaSyAoJtdIZ0Bms9_4w6trMp9hyoWVk5-LK4s")
# 
# zip_responses <- clean_responses %>%
#   filter(question_id %in% c(14,26,27,28,29))
# 
# uq_zip_codes <- zip_responses %>%
#   group_by(answer_text) %>%
#   summarise(num_respondents = n_distinct(respondent_id)) %>%
#   ungroup() %>%
#   rename(zip_code = answer_text) %>%
#   arrange(desc(num_respondents))
# 
# with_progress({
#   
#   p <- progressor(steps = nrow(uq_zip_codes))
#   
#   geocode_result_tibble <- uq_zip_codes %>%
#     mutate(geocode_results = map(zip_code,function(zc_text, p){
#       
#       p()
#       
#       google_geocode(zc_text)
#       
#     }, p = p))
# })
# 
# 
# zip_code_ref = geocode_result_tibble %>%
#   mutate(cleaned_geocode_results = map(geocode_results, function(grs){
#     
#     if(grs$status == "OK"){
#       
#       results = grs$results
#       
#       components = results$address_components[[1]] %>%
#         unnest(types) %>%
#         select(types, long_name) %>%
#         rename(geog_type = types) %>%
#         filter(geog_type != "political") %>%
#         distinct() %>%
#         pivot_wider(names_from = geog_type,
#                     values_from = long_name)
#       
#       loc = results$geometry$location
#       
#       res_tibble = bind_cols(components,loc)
#       
#       return(res_tibble)
#     }else{
#       return(tibble())
#     }
#     
#   })) %>%
#   select(-geocode_results) %>%
#   unnest(cleaned_geocode_results) %>%
#   select(zip_code:lng) %>%
#   rename(zip_code_raw_string = zip_code) %>%
#   arrange(postal_code)
# 
# # clipr::write_clip(zip_code_ref)
# 
# survey_zip_ref_cats <- read_excel(
#   paste0(get_sharepoint_dir(),
#          "/Wash Co OR Transit Study - Documents/Shared/02 Engagement/OOH/zipcode_groups.xlsx"),
#   sheet = "survey-zips-to-date"
# )
# 
# respondent_home_zip_ref <- zip_responses %>%
#   filter(question_id == 14) %>%
#   select(respondent_id, answer_text) %>%
#   left_join(survey_zip_ref_cats %>%
#               select(zip_code_raw_string, Zone) %>%
#               rename(answer_text = zip_code_raw_string,
#                      home_zip_zone = Zone)) %>%
#   mutate(home_zip_zone = replace_na(home_zip_zone,"No answer"))
# 
# write_rds(respondent_home_zip_ref,"data/output/respondent_home_zip_ref.rds")

respondent_home_zip_tally <- respondent_home_zip_ref %>%
  group_by(home_zip_zone) %>%
  summarise(num_respondents = n_distinct(respondent_id)) %>%
  mutate(prop_respondents = num_respondents/sum(num_respondents)) %>%
  arrange(desc(prop_respondents))

check <- respondent_home_zip_ref  %>%
  filter(!(home_zip_zone %in% c("No answer","Other States"))) %>%
  group_by(answer_text) %>%
  summarise(n = n())

# Emails
respondent_emails <- clean_responses %>%
  filter(question_id == 31) %>%
  select(respondent_id,header_level_2,answer_text) %>%
  pivot_wider(names_from = header_level_2,
              values_from = answer_text) %>%
  left_join(respondent_home_zip_ref) %>%
  filter(!(home_zip_zone %in% c("No answer","Other States")))

clipr::write_clip(respondent_emails)

#Cross-Tabulations ---------

## Plot function -------

crosstab_function <- function(demog_ref, compare_qid, plot_title){
  
  answer_wrap_width = 60
  
  sub_responses <- clean_responses %>%
    filter(question_id == compare_qid) %>%
    filter(other_text == FALSE | is.na(other_text)) %>%
    mutate(answer_text = case_when(
      is.na(other_text) ~ header_level_2,
      TRUE ~ answer_text
    )) %>%
    left_join(transit_rider_ref) %>%
    left_join(demog_ref %>% select(-question_id)) %>%
    filter(!is.na(demo_group)) %>%
    filter(demo_group != "No answer") %>%
    filter(!(answer_text %in% c("Prefer not to answer")))
  
  demo_qid <- unique(demog_ref$question_id)[1]
  
  demo_group = case_when(
    demo_qid == 14 ~ "Home Location",
    demo_qid == 15 ~ "Household Income Group",
    demo_qid == 17 ~ "Age Group",
    demo_qid == 18 ~ "Racial Group",
    demo_qid == 20 ~ "Gender", 
    demo_qid == 21 ~ "Disability Status"
  )
  
  qtext = questions$question_text[questions$question_id == compare_qid]
  qordinal = questions$ordinal[questions$question_id == compare_qid]
  
  rider_cat_respondent_ref <- sub_responses %>%
    group_by(transit_rider_cat) %>%
    summarise(num_respondents = n_distinct(respondent_id)) %>%
    mutate(legend_label = paste0(transit_rider_cat," (N=",comma(num_respondents),")"))
  
  demo_group_respondent_ref <- sub_responses %>%
    group_by(demo_group)  %>%
    summarise(num_respondents = n_distinct(respondent_id)) %>%
    ungroup() %>%
    arrange(demo_group) %>%
    mutate(demo_group_label = paste0(demo_group," (N=",comma(num_respondents),")")) %>%
    mutate(demo_group_label = factor(demo_group_label, ordered=TRUE, levels = demo_group_label))
  
  if(qordinal == TRUE){
    ordered_answers <- answers %>%
      filter(question_id == qid) %>%
      select(answer_text) %>%
      pull(answer_text) %>%
      str_wrap(width = answer_wrap_width) %>%
      unique()
  }else{
    ordered_answers <- sub_responses %>%
      group_by(answer_text) %>%
      summarise(num_responses = n()) %>%
      arrange(num_responses) %>%
      ungroup() %>%
      mutate(answer_text = str_wrap(answer_text,answer_wrap_width)) %>%
      pull(answer_text)
  }
  
  respondent_counts <- sub_responses %>%
    group_by(transit_rider_cat,demo_group) %>%
    summarise(num_respondents = n_distinct(respondent_id))
  
  summ_responses <- sub_responses %>%
    group_by(transit_rider_cat, demo_group, answer_text) %>%
    summarise(num_responses = n())  %>%
    ungroup() %>%
    left_join(respondent_counts) %>%
    mutate(prop_responses = num_responses/num_respondents) %>%
    mutate(answer_text = str_wrap(answer_text,answer_wrap_width)) %>%
    mutate(answer_text = factor(answer_text,ordered=TRUE, levels = ordered_answers)) %>%
    arrange(answer_text) %>%
    group_by(answer_text) %>%
    mutate(num_answer_responses = sum(num_responses)) %>%
    ungroup() %>%
    mutate(answer_label = paste0(as.character(answer_text)," (N=",num_answer_responses,")")) %>%
    mutate(answer_label = factor(answer_label, ordered=TRUE, levels = unique(answer_label))) %>%
    left_join(demo_group_respondent_ref)
  
  plt <- ggplot(summ_responses, aes(y = answer_label, x = demo_group_label, 
                             fill = prop_responses, 
                             label=percent(prop_responses,accuracy = 0.1))) +
    geom_tile() +
    geom_label(fill = "white", alpha=0.6, size = 5) +
    #facet_wrap(~transit_rider_cat)+
    scale_fill_viridis_c(option = "rocket",direction = -1,
                         labels = percent) +
    labs(
      fill = "Proportion of Respondents\nwithin Demographic Group",
      x = demo_group,
      y = "Answer Text",
      title = plot_title
    ) +
    nn_basic_theme(base_size = 18, grey_background = FALSE) +
    theme(legend.key.height =  unit(0.33, 'cm'),
          axis.text.y = element_text(lineheight = 0.4),
          axis.text.x = element_text(lineheight = 0.4),
          legend.title = element_text(lineheight = 0.4)) 
  
  if(demo_qid == 14){
    plt <- plt +
      guides(x = guide_axis(n.dodge = 2))
  }
  
  num_answers = length(ordered_answers)
  
  ggsave(paste0("viz/survey-analysis/crosstabs/q",
                str_pad(compare_qid,width = 2,side = "left",pad = "0"),"_vs_",
                str_pad(demo_qid,width = 2,side = "left",pad = "0"),".png"),
         width = 6,
         height = 2 + num_answers*0.25,
         dpi = 300,
         plot = plt)
}

## Household Income ----------
demog_ref <- clean_responses %>%
  filter(question_id == 15) %>%
  mutate(demo_group = case_when(
    answer_text == "Less than $10,000" ~ "Less than $50k",
    answer_text == "$10,000 to $19,999" ~ "Less than $50k",
    answer_text == "$20,000 to $29,999" ~ "Less than $50k",
    answer_text == "$30,000 to $39,999" ~ "Less than $50k",
    answer_text == "$40,000 to $49,999" ~ "Less than $50k",
    answer_text == "$50,000 to $74,999" ~ "$50k to less than $100k",
    answer_text == "$75,000 to $99,999" ~ "$50k to less than $100k",
    answer_text == "$100,000 to $149,999" ~ "$100k or more",
    answer_text == "$150,000 or more" ~ "$100k or more",
    answer_text == "Don't know / Prefer not to answer" ~ "No answer"
  )) %>%
  mutate(demo_group = factor(demo_group, ordered = TRUE,
                             levels = c("Less than $50k","$50k to less than $100k",
                                        "$100k or more","No answer"))) %>%
  select(respondent_id, question_id, demo_group)

crosstab_function(demog_ref, compare_qid = 8, "Transit Barriers (for riders) by Household Income Group")
crosstab_function(demog_ref, compare_qid = 9, "Potential Transit Improvements (for riders) by Household Income Group")
crosstab_function(demog_ref, compare_qid = 11, "Transit Barriers (for non-riders) by Household Income Group")
crosstab_function(demog_ref, compare_qid = 12, "Potential Transit Improvements (for non-riders) by Household Income Group")

## Age Group ------------
demog_ref <- clean_responses %>%
  filter(question_id == 17) %>%
  mutate(demo_group = case_when(
    answer_text == "Under 18" ~ "Younger than 35",
    answer_text == "18-24" ~ "Younger than 35",
    answer_text == "25-34" ~ "Younger than 35",
    answer_text == "35-44" ~ "35 - 64",
    answer_text == "45-54" ~ "35 - 64",
    answer_text == "55-64" ~ "35 - 64",
    answer_text == "65-74" ~ "65 and older",
    answer_text == "75 or older" ~ "65 and older",
    answer_text == "Prefer not to answer" ~ "No answer"
  ))  %>%
  mutate(demo_group = factor(demo_group, ordered = TRUE,
                             levels = c("Younger than 35","35 - 64",
                                        "65 and older","No answer"))) %>%
  select(respondent_id, question_id, demo_group)

crosstab_function(demog_ref, compare_qid = 8, "Transit Barriers (for riders) by Age Group")
crosstab_function(demog_ref, compare_qid = 9, "Potential Transit Improvements (for riders) by Age Group")
crosstab_function(demog_ref, compare_qid = 11, "Transit Barriers (for non-riders) by Age Group")
crosstab_function(demog_ref, compare_qid = 12, "Potential Transit Improvements (for non-riders) by Age Group")

## Disability vs. non-Disability ----------
demog_ref <- clean_responses %>%
  filter(question_id == 21) %>%
  mutate(demo_group = case_when(
    answer_text == "Prefer not to answer" ~ "No answer",
    answer_text == "No disability" ~ "Not Disabled",
    TRUE ~ "Disabled"
  ))  %>%
  mutate(demo_group = factor(demo_group, ordered = TRUE,
                             levels = c("Not Disabled","Disabled","No answer"))) %>%
  select(respondent_id, question_id, demo_group)

crosstab_function(demog_ref, compare_qid = 8, "Transit Barriers (for riders) by Disability Status")
crosstab_function(demog_ref, compare_qid = 9, "Potential Transit Improvements (for riders) by Disability Status")
crosstab_function(demog_ref, compare_qid = 11, "Transit Barriers (for non-riders) by Disability Status")
crosstab_function(demog_ref, compare_qid = 12, "Potential Transit Improvements (for non-riders) by Disability Status")

## People of Color vs. White Alone ---------
demog_ref <- clean_responses %>%
  filter(question_id == 18) %>%
  mutate(demo_group = case_when(
    answer_text == "Prefer not to answer" ~ "No answer",
    answer_text == "White" ~ "White",
    TRUE ~ "Person of Color"
  ))  %>%
  mutate(demo_group = factor(demo_group, ordered = TRUE,
                             levels = c("White","Person of Color","No answer"))) %>%
  select(respondent_id, question_id, demo_group)

crosstab_function(demog_ref, compare_qid = 8, "Transit Barriers (for riders) by Racial Group")
crosstab_function(demog_ref, compare_qid = 9, "Potential Transit Improvements (for riders) by Racial Group")
crosstab_function(demog_ref, compare_qid = 11, "Transit Barriers (for non-riders) by Racial Group")
crosstab_function(demog_ref, compare_qid = 12, "Potential Transit Improvements (for non-riders) by Racial Group")

## Gender ----------------
demog_ref <- clean_responses %>%
  filter(question_id == 20) %>%
  mutate(demo_group = case_when(
    answer_text == "Man" ~ "Man",
    answer_text == "Woman" ~ "Woman",
    answer_text == "Prefer not to answer" ~ "No answer",
    TRUE ~ str_wrap("Transgender, Non-binary, genderqueer or third gender, or gender not listed",30)
  ))  %>%
  mutate(demo_group = factor(demo_group, ordered = TRUE,
                             levels = c("Man","Woman",
                                        str_wrap("Transgender, Non-binary, genderqueer or third gender, or gender not listed",30),
                                        "No answer"))) %>%
  select(respondent_id, question_id, demo_group)

crosstab_function(demog_ref, compare_qid = 8, "Transit Barriers (for riders) by Gender Group")
crosstab_function(demog_ref, compare_qid = 9, "Potential Transit Improvements (for riders) by Gender Group")
crosstab_function(demog_ref, compare_qid = 11, "Transit Barriers (for non-riders) by Gender Group")
crosstab_function(demog_ref, compare_qid = 12, "Potential Transit Improvements (for non-riders) by Gender Group")


## Home Location ---------
demog_ref <- respondent_home_zip_ref %>%
  select(respondent_id, home_zip_zone) %>%
  rename(demo_group = home_zip_zone) %>%
  mutate(question_id = 14) %>%
  mutate(demo_group = factor(demo_group,ordered = TRUE, levels = respondent_home_zip_tally$home_zip_zone))

crosstab_function(demog_ref, compare_qid = 8, plot_title = "Transit Barriers (for riders) by Home Location")
crosstab_function(demog_ref, compare_qid = 9, "Potential Transit Improvements (for riders) by Home Location")
crosstab_function(demog_ref, compare_qid = 11, "Transit Barriers (for non-riders) by Home Location")
crosstab_function(demog_ref, compare_qid = 12, "Potential Transit Improvements (for non-riders) by Home Location")
