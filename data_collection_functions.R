# Functions used to collect data for my table

library(tidyverse)
library(baseballr)
library(janitor)
library(forestmangr)

startyear_and_id_finder <- function(name) {
  
  player_name <- str_to_title(name)
  player_firstname <- strsplit(player_name, "\\s+")[[1]][1]
  player_lastname <- strsplit(player_name, "\\s+")[[1]][2]
  
  id_and_startyear <- playerid_lookup(last_name = player_lastname, first_name = player_firstname) %>%
    arrange(desc(mlb_played_first)) %>%
    top_n(1) %>%
    select(mlbam_id, mlb_played_first)
  
  start_year <- id_and_startyear %>%
    select(mlb_played_first) %>%
    deframe()
  
  id <- id_and_startyear %>%
    select(mlbam_id) %>%
    deframe()
  
  return(c(start_year, id))
}


statcast_data_finder <- function(batter_name) {
  
  start_year <- startyear_and_id_finder(batter_name)[1]
  if(start_year > 2015){
    seasons <- (start_year:2020)
  } else{
    seasons <- (2015:2020)
  }
  
  id <- startyear_and_id_finder(batter_name)[2]  
  player_data <- map_df(seasons, function(season) {
    scrape_statcast_savant(start_date = glue::glue("{season}-04-01"),
                           end_date   = glue::glue("{season}-10-01"),
                           playerid   = id)
  }) %>%
    arrange(desc(game_date))
  
  return(player_data)
}


get_table_data <- function(name){
  
  all_batter_data <- statcast_data_finder(name)
  
  table_data <- all_batter_data %>% 
    filter(type == "X") %>%
    filter(!estimated_woba_using_speedangle == "null") %>%
    filter(!events == "sac_fly") %>%
    select(game_year, events, estimated_woba_using_speedangle, woba_value, estimated_ba_using_speedangle) %>%
    mutate(game_year = as.character(game_year)) %>%
    mutate(ba = ifelse(events %in% c("single", "double", "triple", "home_run"), 1, 0)) %>%
    group_by(game_year) %>%
    summarise(ba = sum(ba)/ n(),
              expected_ba = sum(as.double(estimated_ba_using_speedangle))/ n(),
              woba = sum(as.double(woba_value))/ n(),
              expected_woba = sum(as.double(estimated_woba_using_speedangle))/ n()) %>%
    mutate(ba_above_expected = ba - expected_ba,
           woba_above_expected = woba - expected_woba) %>%
    select(1,2,3,6,4,5,7) %>%
    round_df(digits = 3)
  
  return(table_data)
}

