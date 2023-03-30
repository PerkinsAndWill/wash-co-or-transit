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

map_comment_path <- paste0(get_sharepoint_dir(),
                           "/Wash Co OR Transit Study - Documents/Shared/02 Engagement/OOH/Map_Comments")

map_comment_files = tibble(
  file_name = list.files(map_comment_path)
  ) %>%
  mutate(comment_type = file_name %>%
           str_replace("PublicInput_","") %>%
           str_replace(".csv","")) %>%
  mutate(raw_comments = map(file_name, 
                            ~ read_csv(paste0(map_comment_path,"/",.x))))

map_comments <- map_comment_files %>%
  select(comment_type, raw_comments) %>%
  unnest(raw_comments) %>%
  clean_names() %>%
  select(comment_type, oid, comments) %>%
  filter(!is.na(comments)) %>%
  mutate(map_comment_id = seq_along(oid))

clipr::write_clip(map_comments)

map_single_tokens <- map_comments %>%
  select(map_comment_id, comments) %>%
  unnest_tokens(word, comments) %>%
  anti_join(stop_words) 

map_single_token_summ <- map_single_tokens %>%
  group_by(word) %>%
  summarise(num_comments = n()) %>%
  arrange(desc(num_comments))

clipr::write_clip(map_single_token_summ)
