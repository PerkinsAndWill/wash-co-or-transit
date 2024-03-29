library(tidytransit)
library(tidyverse)
library(janitor)
library(DBI)
library(dbplyr)
library(jsonlite)
library(geojsonsf)
library(sf)
library(httr)
library(lubridate)
library(rgeos)
library(furrr)
library(leaflet)
library(readxl)
library(hms)
library(aws.s3)
library(aws.signature)
library(aws.iam)
library(nntools)
library(readxl)
library(nngeo)
library(leaflet.extras2)
library(viridis)
library(nntools)
library(clipr)
library(tigris)

future::plan(multisession)

# Database Extracts ---------------------------
app_agency_id <- "TriMet"
app_time_period <- "Fall 2019"

reconnect_db = function(){
  con <<- dbConnect(RPostgres::Postgres(),
                    dbname = "trimet-bdat",
                    host="nn-sandbox.cwux9kotq1gg.us-west-2.rds.amazonaws.com",
                    port = 5432,
                    user = "trimet-bdat-admin",
                    password = "xm.iFZF8keDfNy7")
}
reconnect_db()

analysis_segments <- read_sf(con,"analysis_segments")

segment_directions <- tbl(con,"segment_directions_simp") %>%
  collect()

seg_network_defs <- tbl(con,"seg_network_defs") %>% 
  filter(time_period == app_time_period) %>%
  collect() %>%
  mutate(cardinal_direction = paste0(cardinal_direction,"bound")) %>%
  left_join(segment_directions) %>%
  fill(fb_direction,.direction = "downup")

processed_seg_data <- tbl(con,"processed_seg_data") %>%
  filter(time_period == "Fall 2019") %>%
  collect() %>%
  mutate(pass_delay = delay_per_trip*total_load)

# USER INPUTS ---------------------

#Folder for saving outputs to
folder_name = "trimet"

#Coordinate systems
coord_global = 4326
coord_local = 2269 #see here: https://nelsonnygaard.shinyapps.io/coord-system-reference/

#Path to GTFS feed you will use
feed_path = "data/input/gtfs/trimet/2022-09-22.zip"

#A character string to identify the time period of the GTFS feed you are using
app_time_period = "Fall 2022"
file_ending = str_replace_all(app_time_period," ","_")

# Project Setup -----------
if(!dir.exists("viz/frequency-span-charts")){dir.create("viz/frequency-span-charts")}
if(!dir.exists(paste0("viz/frequency-span-charts/",folder_name))){
  dir.create(paste0("viz/frequency-span-charts/",folder_name))
}

full_output_folder = paste0("viz/frequency-span-charts/",folder_name)

# GTFS Table Setup ------------
bus_gtfs = read_gtfs(feed_path)

trips            <- bus_gtfs$trips  
stop_times       <- bus_gtfs$stop_times 
shapes           <- bus_gtfs$shapes 
routes           <- bus_gtfs$routes %>%
  mutate(route_label = case_when(
    str_length(route_short_name) == 0 ~ route_long_name,
    #TRUE ~ paste0(route_short_name,": ",route_long_name)
    TRUE ~ paste0(str_pad(route_short_name,width=3,side="left",pad="0"),": ",route_long_name)
  ))
stops            <- bus_gtfs$stops %>%
  filter(!is.na(stop_lon),!is.na(stop_lat)) %>%
  st_as_sf(coords = c("stop_lon", "stop_lat"),
           crs = coord_global) %>%
  st_transform(crs = coord_local)

gtfs_table_names <- names(bus_gtfs)

#This if statement will build a cross reference of date and service ID via calendar files
if("calendar" %in% gtfs_table_names & "calendar_dates" %in% gtfs_table_names){
  calendar_reg<- bus_gtfs$calendar %>% 
    mutate(date_range = map2(start_date,end_date,function(sd,ed){
      seq.Date(sd,ed,by='1 day')
    })) %>%
    unnest(date_range) %>%
    rename(date = date_range) %>%
    pivot_longer(cols = monday:sunday) %>%
    rename(weekday_operating = name) %>%
    mutate(weekday_operating = str_to_title(weekday_operating)) %>%
    mutate(weekday = weekdays(date)) %>%
    filter(value == 1, weekday==weekday_operating) %>%
    select(service_id,date,weekday) %>%
    arrange(service_id,date) %>%
    left_join(
      tibble(
        weekday = c("Monday","Tuesday","Wednesday","Thursday",
                    "Friday","Saturday","Sunday"),
        day_cat = c("Weekday","Weekday","Weekday","Weekday",
                    "Weekday","Saturday","Sunday")
      )
    )
  
  calendar_exceptions <- bus_gtfs$calendar_dates %>%
    filter(exception_type == 1) %>%
    mutate(weekday = weekdays(date)) %>%
    select(service_id,date,weekday) %>%
    arrange(service_id,date) %>%
    left_join(
      tibble(
        weekday = c("Monday","Tuesday","Wednesday","Thursday",
                    "Friday","Saturday","Sunday"),
        day_cat = c("Weekday","Weekday","Weekday","Weekday",
                    "Weekday","Saturday","Sunday")
      )
    )
  
  calendar = bind_rows(calendar_reg,calendar_exceptions) %>%
    distinct() %>%
    arrange(service_id,date)
}else if ("calendar" %in% gtfs_table_names & !("calendar_dates" %in% gtfs_table_names)){
  calendar <- bus_gtfs$calendar %>% 
    mutate(date_range = map2(start_date,end_date,function(sd,ed){
      seq.Date(sd,ed,by='1 day')
    })) %>%
    unnest(date_range) %>%
    rename(date = date_range) %>%
    pivot_longer(cols = monday:sunday) %>%
    rename(weekday_operating = name) %>%
    mutate(weekday_operating = str_to_title(weekday_operating)) %>%
    mutate(weekday = weekdays(date)) %>%
    filter(value == 1, weekday==weekday_operating) %>%
    select(service_id,date,weekday) %>%
    arrange(service_id,date) %>%
    left_join(
      tibble(
        weekday = c("Monday","Tuesday","Wednesday","Thursday",
                    "Friday","Saturday","Sunday"),
        day_cat = c("Weekday","Weekday","Weekday","Weekday",
                    "Weekday","Saturday","Sunday")
      )
    )
}else if (!("calendar" %in% gtfs_table_names) & "calendar_dates" %in% gtfs_table_names){
  calendar <- bus_gtfs$calendar_dates %>%
    filter(exception_type == 1) %>%
    mutate(weekday = weekdays(date)) %>%
    select(service_id,date,weekday) %>%
    arrange(service_id,date) %>%
    left_join(
      tibble(
        weekday = c("Monday","Tuesday","Wednesday","Thursday",
                    "Friday","Saturday","Sunday"),
        day_cat = c("Weekday","Weekday","Weekday","Weekday",
                    "Weekday","Saturday","Sunday")
      )
    )
}

most_freq_stop_screen = stop_times %>%
  left_join(trips %>% select(trip_id,route_id,direction_id,shape_id,service_id)) %>%
  left_join(calendar %>% distinct(service_id,day_cat)) %>%
  group_by(day_cat,route_id,direction_id,stop_id) %>%
  summarise(stop_count = n()) %>%
  arrange(day_cat,route_id,direction_id,desc(stop_count)) %>%
  do(head(.,n=1)) %>%
  ungroup() %>%
  select(-stop_count)

#Subset to most frequent stop times
most_freq_stop_times = stop_times %>%
  left_join(trips %>% select(trip_id,route_id,direction_id,shape_id,service_id)) %>%
  right_join(most_freq_stop_screen) %>%
  group_by(service_id,day_cat,route_id,direction_id,trip_id) %>%
  arrange(service_id,day_cat,route_id,direction_id,trip_id,stop_sequence) %>%
  do(tail(.,n=1)) %>%
  ungroup() %>%
  mutate(departure_time = str_pad(departure_time,width=8,side='left',pad='0'),
         arrival_time = str_pad(arrival_time,width=8,side='left',pad='0')) %>%
  mutate(trip_depart_hour = as.numeric(str_sub(departure_time,1,2))+
           as.numeric(str_sub(departure_time,4,5))/60 +
           as.numeric(str_sub(departure_time,7,8))/3600) %>%
  mutate(floor_hour = floor(trip_depart_hour)) %>%
  group_by(service_id,day_cat,route_id,direction_id,stop_id) %>%
  arrange(service_id,day_cat,route_id,direction_id,stop_id,trip_depart_hour) %>%
  mutate(headway_observed = (trip_depart_hour-lag(trip_depart_hour))*60) %>%
  ungroup() 

# Plot Assembly -------------------------------------------------------------------------

headway_summary <- most_freq_stop_times %>%
  select(service_id,day_cat,route_id,direction_id,floor_hour,
         headway_observed,trip_id, trip_depart_hour) %>%
  left_join(calendar) %>%
  group_by(day_cat,route_id,floor_hour) %>%
  summarise(mean_headway_observed = mean(headway_observed, na.rm=TRUE),
            num_dates_observed = n_distinct(date),
            num_uq_trip_ids = n_distinct(trip_id),
            min_trip_depart_hour = min(trip_depart_hour),
            max_trip_depart_hour = max(trip_depart_hour)) %>%
  ungroup() %>%
  left_join(routes %>% select(route_id,route_type,route_label)) %>%
  mutate(span_depart_hour = max_trip_depart_hour - min_trip_depart_hour) %>%
  mutate(mean_headway_observed = ifelse(mean_headway_observed>60,60,mean_headway_observed),
         mean_headway_observed = ifelse(is.na(mean_headway_observed),60,mean_headway_observed)) %>%
  filter(num_dates_observed >5)

xl_db_path <- paste0(get_sharepoint_dir(),
                     "/Wash Co OR Transit Study - Documents/Shared/04 Transit Market/Performance/transit-analysis-worksheet.xlsx")

xl_sheets <- excel_sheets(xl_db_path)

wash_co_route_ref <- read_excel(xl_db_path,sheet = "wash_co_route_ref")

uq_day_types <- c("Weekday", "Saturday", "Sunday")

route_level_ref <- read_excel(xl_db_path, sheet = "ft_headways_wkd", skip = 2) %>%
  clean_names() %>%
  select(plot_order, route_label) %>% 
  arrange(desc(plot_order)) %>%
  mutate(route_label = case_when(
    str_sub(route_label,1,1) == "0" ~ str_sub(route_label,2,-1),
    TRUE ~ route_label
  ))

## Existing Frequency -----------

num_uq_routes <- length(unique(route_level_ref$route_label))
route_levels = unique(route_level_ref$route_label)

weekday_headway_summary <- headway_summary %>%
  filter(route_id %in% wash_co_route_ref$route_id) %>%
  filter(day_cat == "Weekday") %>%
  group_by(route_id) %>%
  mutate(sum_uq_trip_ids = sum(num_uq_trip_ids)) %>%
  ungroup() %>%
  arrange(desc(route_type),sum_uq_trip_ids,route_label) %>%
  mutate(route_label = case_when(
    str_sub(route_label,1,1) == "0" ~ str_sub(route_label,2,-1),
    TRUE ~ route_label
  )) 

sub_route_levels <- route_levels[route_levels %in% weekday_headway_summary$route_label]
sub_num_uq_routes <- length(sub_route_levels)

pdf("viz/frequency-span-charts/trimet/frequency-span-by-day-type.pdf",height = 8.5,width = 11,onefile = TRUE)
for(i in 1:length(uq_day_types)){
  dc = uq_day_types[[i]]
  
  filt_headway_summary <- headway_summary %>%
    filter(route_id %in% wash_co_route_ref$route_id) %>%
    filter(day_cat == dc) %>%
    group_by(route_id) %>%
    mutate(sum_uq_trip_ids = sum(num_uq_trip_ids)) %>%
    ungroup() %>%
    arrange(desc(route_type),sum_uq_trip_ids,route_label) %>%
    mutate(route_label = case_when(
      str_sub(route_label,1,1) == "0" ~ str_sub(route_label,2,-1),
      TRUE ~ route_label
    )) 
  
  sub_headway_summary <-  filt_headway_summary %>%
    mutate(route_label = factor(route_label, ordered = TRUE, levels = sub_route_levels))
  
  plt <- ggplot(sub_headway_summary, aes(xmin=floor_hour, xmax = floor_hour +1, 
                                  ymin = as.numeric(route_label)-1,
                                  ymax = as.numeric(route_label), 
                                  fill = mean_headway_observed)) +
    geom_rect()+
    scale_fill_viridis(option = "F", limits = c(0,60),alpha = 0.8,
                       name = "Avg.\nHeadway")+
    theme_light() +
    scale_x_continuous(breaks = seq(0,26,2),
                       labels = c("12 am","2 am","4 am","6 am",
                                  "8 am","10 am","12 pm","2 pm",
                                  "4 pm","6 pm","8 pm","10 pm",
                                  "12 am","2 am"),
                       name = "Hour of Day",
                       expand = expansion(0))+
    scale_y_continuous(breaks = seq(0.5,sub_num_uq_routes-0.5),
                       limits = c(0,sub_num_uq_routes),
                       labels = sub_route_levels,
                       expand = expansion(0))+
    labs(
      y = "Route",
      title = "Frequency and Span for Washington County, OR Routes",
      subtitle = paste0(dc," Service, Fall 2022")
    )+
    theme(panel.grid.major.y = element_blank())
  
  print(plt)
}
dev.off()

## Future Frequency -------------

future_freq_dt_ref <- tribble(
  ~day_cat, ~sheet_name,
  "Weekday", "ft_headways_wkd",
  "Saturday", "ft_headways_sat_sun",
  "Sunday", "ft_headways_sat_sun"
)

pdf("viz/frequency-span-charts/trimet/future-frequency-span-by-day-type.pdf",height = 8.5,width = 11,onefile = TRUE)
for(i in 1:nrow(future_freq_dt_ref)){
  
  sn <- future_freq_dt_ref$sheet_name[i]
  dc <- future_freq_dt_ref$day_cat[i]
  
  if(dc == "Weekday"){
    raw <- read_excel(xl_db_path, sheet = sn, skip = 2) %>%
      clean_names()
    
    sub_headway_summary <- bind_rows(
      headway_summary %>%
        filter(route_id %in% wash_co_route_ref$route_id) %>%
        filter(route_type !=3, day_cat == dc) %>%
        select(route_label, floor_hour, mean_headway_observed),
      raw %>%
        filter(route_type == 3) %>%
        select(1:x27_30) %>%
        pivot_longer(cols = `x4_7`:`x27_30`, names_to = "temp",
                     values_to = "mean_headway_observed") %>%
        mutate(floor_hour = str_replace(temp,"x","") %>%
                 str_extract(".+(?<=_)") %>%
                 str_replace("_","") %>%
                 as.numeric()) %>%
        select(-temp)
    ) %>%
      filter(!is.na(mean_headway_observed)) %>%
      mutate(num_trips = round(60/mean_headway_observed)) %>%
      group_by(route_label) %>%
      mutate(daily_trips = sum(num_trips)) %>%
      ungroup() %>%
      mutate(route_label = case_when(
        str_sub(route_label,1,1) == "0" ~ str_sub(route_label,2,-1),
        TRUE ~ route_label
      )) %>%
      mutate(route_label = factor(route_label, ordered = TRUE, levels = route_levels))
  }else{
    raw <- read_excel(xl_db_path, sheet = sn, skip = 1) 
    
    sub_headway_summary <- bind_rows(
      headway_summary %>%
        filter(route_id %in% wash_co_route_ref$route_id) %>%
        filter(route_type !=3, day_cat == dc) %>%
        select(route_label, floor_hour, mean_headway_observed),
      raw %>%
        filter(route_type == 3) %>%
        pivot_longer(cols = `4`:`27`, names_to = "floor_hour",
                     values_to = "mean_headway_observed") %>%
        mutate(floor_hour = as.numeric(floor_hour))
    ) %>%
      filter(!is.na(mean_headway_observed)) %>%
      mutate(num_trips = round(60/mean_headway_observed)) %>%
      group_by(route_label) %>%
      mutate(daily_trips = sum(num_trips)) %>%
      ungroup() %>%
      mutate(route_label = case_when(
        str_sub(route_label,1,1) == "0" ~ str_sub(route_label,2,-1),
        TRUE ~ route_label
      )) %>%
      mutate(route_label = factor(route_label, ordered = TRUE, levels = route_levels))
  }
  
  missing_route_labels <- route_levels[!(route_levels %in% sub_headway_summary$route_label)]
  
  missing_route_frame <-expand_grid(
    route_label = missing_route_labels,
    floor_hour = 3:27
  ) %>%
    filter(!(route_label %in% c("WES Commuter Rail"))) %>%
    mutate(route_label = factor(route_label, ordered = TRUE, levels = route_levels)) 
  
  # pf_headway_summary <- sub_headway_summary %>%
  #   select(floor_hour,route_label,mean_headway_observed) %>%
  #   full_join(
  #     expand_grid(
  #       route_label = route_levels,
  #       floor_hour = 3:26
  #     ) %>%
  #       mutate(route_label = factor(route_label, ordered = TRUE, levels = route_levels))
  #   ) %>%
  #   arrange(route_label,floor_hour)
  
  plt <- ggplot() +
    geom_rect(data = sub_headway_summary, aes(xmin=floor_hour, xmax = floor_hour +1, 
                                              ymin = as.numeric(route_label)-1,
                                              ymax = as.numeric(route_label), 
                                              fill = mean_headway_observed))+
    geom_line(data = missing_route_frame, aes(x = floor_hour, y = as.numeric(route_label)-0.5, group =route_label,
                                              color="Discontinued Routes"),
              size=3) +
    scale_color_manual(values = c("dark grey"),name="") +
    scale_fill_viridis(option = "F", limits = c(0,60),alpha = 0.8,
                       name = "Avg.\nHeadway")+
    theme_light() +
    scale_x_continuous(breaks = seq(0,26,2),
                       labels = c("12 am","2 am","4 am","6 am",
                                  "8 am","10 am","12 pm","2 pm",
                                  "4 pm","6 pm","8 pm","10 pm",
                                  "12 am","2 am"),
                       name = "Hour of Day",
                       expand = expansion(0))+
    scale_y_continuous(breaks = seq(0.5,num_uq_routes-0.5),
                       limits = c(0,num_uq_routes),
                       labels = route_levels,
                       expand = expansion(0))+
    labs(
      y = "Route",
      title = "Frequency and Span for Washington County, OR Routes",
      subtitle = paste0(dc," Service, Forward Together Service Plan")
    )+
    theme(panel.grid.major.y = element_blank())
  
  print(plt)
  
}
dev.off()

## change in number of daily trips ---------

dt_list = list()

for(i in 1:nrow(future_freq_dt_ref)){
  
  sn <- future_freq_dt_ref$sheet_name[i]
  dc <- future_freq_dt_ref$day_cat[i]
  
  if(dc == "Weekday"){
    raw <- read_excel(xl_db_path, sheet = sn, skip = 2) %>%
      clean_names()
    
    ft_headway_summary <- bind_rows(
      headway_summary %>%
        filter(route_id %in% wash_co_route_ref$route_id) %>%
        filter(route_type !=3, day_cat == dc) %>%
        select(route_label, floor_hour, mean_headway_observed),
      raw %>%
        filter(route_type == 3) %>%
        select(1:x27_30) %>%
        pivot_longer(cols = `x4_7`:`x27_30`, names_to = "temp",
                     values_to = "mean_headway_observed") %>%
        mutate(floor_hour = str_replace(temp,"x","") %>%
                 str_extract(".+(?<=_)") %>%
                 str_replace("_","") %>%
                 as.numeric()) %>%
        select(-temp)
    ) %>%
      filter(!is.na(mean_headway_observed)) %>%
      mutate(num_trips = round(60/mean_headway_observed)) %>%
      group_by(route_label) %>%
      mutate(daily_trips = sum(num_trips)) %>%
      ungroup() %>%
      mutate(route_label = case_when(
        str_sub(route_label,1,1) == "0" ~ str_sub(route_label,2,-1),
        TRUE ~ route_label
      )) %>%
      mutate(route_label = factor(route_label, ordered = TRUE, levels = route_levels))
  }else{
    raw <- read_excel(xl_db_path, sheet = sn, skip = 1) 
    
    ft_headway_summary <- bind_rows(
      headway_summary %>%
        filter(route_id %in% wash_co_route_ref$route_id) %>%
        filter(route_type !=3, day_cat == dc) %>%
        select(route_label, floor_hour, mean_headway_observed),
      raw %>%
        filter(route_type == 3) %>%
        pivot_longer(cols = `4`:`27`, names_to = "floor_hour",
                     values_to = "mean_headway_observed") %>%
        mutate(floor_hour = as.numeric(floor_hour))
    ) %>%
      filter(!is.na(mean_headway_observed)) %>%
      mutate(num_trips = round(60/mean_headway_observed)) %>%
      group_by(route_label) %>%
      mutate(daily_trips = sum(num_trips)) %>%
      ungroup() %>%
      mutate(route_label = case_when(
        str_sub(route_label,1,1) == "0" ~ str_sub(route_label,2,-1),
        TRUE ~ route_label
      )) %>%
      mutate(route_label = factor(route_label, ordered = TRUE, levels = route_levels))
  }
  
  ex_headway_summary <- headway_summary %>%
    filter(route_id %in% wash_co_route_ref$route_id) %>%
    filter(day_cat == dc) %>%
    group_by(route_id) %>%
    mutate(sum_uq_trip_ids = sum(num_uq_trip_ids)) %>%
    ungroup() %>%
    arrange(desc(route_type),sum_uq_trip_ids,route_label) %>%
    mutate(route_label = case_when(
      str_sub(route_label,1,1) == "0" ~ str_sub(route_label,2,-1),
      TRUE ~ route_label
    )) 
  
  ft_daily_trips <- ft_headway_summary %>%
    group_by(route_label) %>%
    summarise(daily_trips = mean(daily_trips)) %>%
    ungroup() %>%
    mutate(day_cat = dc)
  
  ex_daily_trips <- ex_headway_summary %>%
    mutate(num_trips = 60/mean_headway_observed) %>%
    group_by(route_label) %>%
    summarise(daily_trips = sum(num_trips) %>% round(0)) %>%
    ungroup() %>%
    mutate(day_cat = dc)
  
  joined_daily_trips = ex_daily_trips %>%
    rename(ex_daily_trips = daily_trips) %>%
    full_join(ft_daily_trips %>%
                rename(ft_daily_trips = daily_trips)) %>%
    mutate(across(.cols = c(ex_daily_trips,ft_daily_trips),
                  replace_na, 0)) %>%
    mutate(diff_daily_trips = ft_daily_trips - ex_daily_trips)
  
  dt_list[[i]] = joined_daily_trips
  
  print(i)
}

dt_bound <- bind_rows(dt_list) %>%
  group_by(route_label) %>%
  mutate(weekday_diff = diff_daily_trips[day_cat=="Weekday"]) %>%
  ungroup() %>%
  arrange(weekday_diff) %>%
  mutate(route_label = factor(route_label, ordered=TRUE, levels = unique(route_label))) %>%
  mutate(day_cat = factor(day_cat,ordered=TRUE,levels=c("Weekday","Saturday","Sunday")))

ggplot(dt_bound,aes(x=diff_daily_trips,y=route_label))+
  geom_col(fill = nn_colors("NN Blue"), alpha=0.9) + facet_wrap(~day_cat) +
  nn_basic_theme(grey_background = FALSE) +
  scale_x_continuous(limits = c(-60,60), breaks = seq(-60,60,20)) +
  theme(panel.grid.major.x = element_line(colour = "#002934", linewidth = 0.1))+
  labs(
    x = "Difference in Daily Trips Scheduled",
    y = "Route",
    title = str_wrap("Difference in Daily Trips Scheduled for Forward Together Service Plan Relative to Fall 2022 Service",width=100)
  )
ggsave("viz/frequency-span-charts/trimet/diff-daily-trips-scheduled.pdf",height = 8.5,width = 11)


## Delay by Route and Hour ------------

or_counties <- counties(state = "OR", year = 2020)
wash_county_geom <- or_counties %>%
  filter(NAME=="Washington") %>%
  st_transform(coord_local)

wash_co_segments <- analysis_segments %>%
  st_transform(coord_local) %>%
  filter(st_intersects(., wash_county_geom, sparse = FALSE))

rt_hr_delay_summ <- processed_seg_data %>%
  filter(segment_id %in% wash_co_segments$segment_id) %>%
  filter(route_id %in% wash_co_route_ref$route_id) %>%
  group_by(agency_id,time_period,route_id,cardinal_direction,in_out_direction,
           direction_id,segment_id,hour) %>%
  summarise(seg_delay = sum(delay,na.rm = TRUE)/60,
            pass_delay = sum(pass_delay,na.rm = TRUE)/60,
            seg_length_miles = mean(seg_length_miles)) %>%
  group_by(agency_id,time_period,route_id,hour) %>%
  summarise(seg_delay = sum(seg_delay),
            pass_delay = sum(pass_delay),
            seg_length_miles = sum(seg_length_miles)) %>%
  mutate(sdpm = seg_delay/seg_length_miles,
         pdpm = pass_delay/seg_length_miles) %>%
  ungroup() %>%
  filter(!is.na(hour)) %>%
  mutate(route_id = as.character(route_id)) %>%
  left_join(routes %>% select(route_id,route_label))  %>%
  group_by(route_id) %>%
  mutate(pass_delay_route = sum(pass_delay),
         seg_delay_route = sum(seg_delay)) %>%
  ungroup() %>%
  arrange(desc(route_id)) %>%
  mutate(route_label = factor(route_label, ordered=TRUE, levels = unique(route_label)))

route_levels <- rt_hr_delay_summ %>% 
  arrange(route_label) %>%
  distinct(route_label) %>%
  mutate(route_label = as.character(route_label)) %>%
  mutate(route_label = case_when(
    str_sub(route_label,1,1) == "0" ~ str_sub(route_label,2,-1),
    TRUE ~ route_label
  )) %>%
  pull(route_label)

num_uq_routes <- length(route_levels)

#Passenger Delay
ggplot(rt_hr_delay_summ, aes(xmin=hour, xmax = hour +1, 
                                ymin = as.numeric(route_label)-1,
                                ymax = as.numeric(route_label), 
                                fill = pdpm*60)) +
  geom_rect()+
  scale_fill_viridis(option = "A",alpha = 0.8, direction = -1,
                     name = "Passenger Delay\n(pax-minutes per mile)")+
  theme_light() +
  scale_x_continuous(breaks = seq(0,26,2),
                     labels = c("12 am","2 am","4 am","6 am",
                                "8 am","10 am","12 pm","2 pm",
                                "4 pm","6 pm","8 pm","10 pm",
                                "12 am","2 am"),
                     name = "Hour of Day",
                     expand = expansion(0))+
  scale_y_continuous(breaks = seq(0.5,num_uq_routes-0.5),
                     limits = c(0,num_uq_routes),
                     labels = route_levels,
                     expand = expansion(0))+
  labs(
    y = "Route",
    title = "Passenger Delay per Mile for Washington County, OR Routes",
    subtitle = "Weekday Service, Fall 2019, Only Includes Route Segments within Washington County"
  )+
  theme(panel.grid.major.y = element_blank())

ggsave("viz/frequency-span-charts/trimet/passenger-delay-weekday.pdf",height = 8.5,width = 11)

#Segment Delay
ggplot(rt_hr_delay_summ, aes(xmin=hour, xmax = hour +1, 
                             ymin = as.numeric(route_label)-1,
                             ymax = as.numeric(route_label), 
                             fill = sdpm*60)) +
  geom_rect()+
  scale_fill_viridis(option = "A",alpha = 0.7, direction = -1,
                     name = "Bus Delay\n(minutes per mile)",
                     limits = c(0,15))+
  theme_light() +
  scale_x_continuous(breaks = seq(0,26,2),
                     labels = c("12 am","2 am","4 am","6 am",
                                "8 am","10 am","12 pm","2 pm",
                                "4 pm","6 pm","8 pm","10 pm",
                                "12 am","2 am"),
                     name = "Hour of Day",
                     expand = expansion(0))+
  scale_y_continuous(breaks = seq(0.5,num_uq_routes-0.5),
                     limits = c(0,num_uq_routes),
                     labels = route_levels,
                     expand = expansion(0))+
  labs(
    y = "Route",
    title = "Bus Delay per Mile for Washington County, OR Routes",
    subtitle = "Weekday Service, Fall 2019, Only Includes Route Segments within Washington County"
  )+
  theme(panel.grid.major.y = element_blank())

ggsave("viz/frequency-span-charts/trimet/bus-delay-weekday.pdf",height = 8.5,width = 11)

all_day_wash_co_seg_summary <- processed_seg_data %>%
  filter(segment_id %in% wash_co_segments$segment_id) %>%
  filter(route_id %in% wash_co_route_ref$route_id) %>%
  group_by(agency_id,time_period,segment_id) %>%
  summarise(seg_delay = sum(delay,na.rm = TRUE)/60,
            pass_delay = sum(pass_delay,na.rm = TRUE)/60,
            seg_length_miles = mean(seg_length_miles),
            total_load = sum(total_load,na.rm = TRUE),
            num_delay_trips = sum(num_delay_trips,na.rm = TRUE),
            num_load_trips = sum(num_load_trips,na.rm = TRUE)) %>%
  mutate(pdpm = pass_delay/seg_length_miles,
         sdpm = seg_delay/seg_length_miles,
         pdpmpt = 60*(pdpm/num_delay_trips),
         sdpmpt = 60*(sdpm/num_delay_trips),
         delay_per_trip = (seg_delay/num_delay_trips)*60,
         service_miles = num_load_trips*seg_length_miles)

am_peak_wash_co_seg_summary <- processed_seg_data %>%
  filter(segment_id %in% wash_co_segments$segment_id) %>%
  filter(route_id %in% wash_co_route_ref$route_id) %>%
  filter(hour %in% c(7,8)) %>%
  group_by(agency_id,time_period,segment_id) %>%
  summarise(seg_delay = sum(delay,na.rm = TRUE)/60,
            pass_delay = sum(pass_delay,na.rm = TRUE)/60,
            seg_length_miles = mean(seg_length_miles),
            total_load = sum(total_load,na.rm = TRUE),
            num_delay_trips = sum(num_delay_trips,na.rm = TRUE),
            num_load_trips = sum(num_load_trips,na.rm = TRUE)) %>%
  mutate(pdpm = pass_delay/seg_length_miles,
         sdpm = seg_delay/seg_length_miles,
         pdpmpt = 60*(pdpm/num_delay_trips),
         sdpmpt = 60*(sdpm/num_delay_trips),
         delay_per_trip = (seg_delay/num_delay_trips)*60,
         service_miles = num_load_trips*seg_length_miles)

pm_peak_wash_co_seg_summary <- processed_seg_data %>%
  filter(segment_id %in% wash_co_segments$segment_id) %>%
  filter(route_id %in% wash_co_route_ref$route_id) %>%
  filter(hour %in% c(16,17)) %>%
  group_by(agency_id,time_period,segment_id) %>%
  summarise(seg_delay = sum(delay,na.rm = TRUE)/60,
            pass_delay = sum(pass_delay,na.rm = TRUE)/60,
            seg_length_miles = mean(seg_length_miles),
            total_load = sum(total_load,na.rm = TRUE),
            num_delay_trips = sum(num_delay_trips,na.rm = TRUE),
            num_load_trips = sum(num_load_trips,na.rm = TRUE)) %>%
  mutate(pdpm = pass_delay/seg_length_miles,
         sdpm = seg_delay/seg_length_miles,
         pdpmpt = 60*(pdpm/num_delay_trips),
         sdpmpt = 60*(sdpm/num_delay_trips),
         delay_per_trip = (seg_delay/num_delay_trips)*60,
         service_miles = num_load_trips*seg_length_miles)

write_sf(wash_co_segments,
         "G:/Current/WASHINGTON_CO_OR_Transit_Study_2020_1005/Analysis/z_Original/TriMet/wash-co-segment-delay/wash-co-analysis-segments.geojson")

write_csv(all_day_wash_co_seg_summary,
          "G:/Current/WASHINGTON_CO_OR_Transit_Study_2020_1005/Analysis/z_Original/TriMet/wash-co-segment-delay/all-day-segment-summary.csv")

write_csv(am_peak_wash_co_seg_summary,
          "G:/Current/WASHINGTON_CO_OR_Transit_Study_2020_1005/Analysis/z_Original/TriMet/wash-co-segment-delay/am-peak-segment-summary.csv")

write_csv(pm_peak_wash_co_seg_summary,
          "G:/Current/WASHINGTON_CO_OR_Transit_Study_2020_1005/Analysis/z_Original/TriMet/wash-co-segment-delay/pm-peak-segment-summary.csv")

