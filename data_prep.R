library(tidyverse)

process_player_summary = function(){
  (player_summary_names = list.files(path = "./raw_data", pattern = "Player Summary", full.names = TRUE))
  player_summary_list = purrr::map(player_summary_names, readr::read_csv)
  names(player_summary_list) = player_summary_names
  
  # player_summary_list
  
  player_summary_list %>% map_int(ncol)
  # gplots::venn(player_summary_list %>% map(colnames))
  
  player_summary = player_summary_list %>% bind_rows(.id = "file_name") %>% 
    dplyr::mutate(MATCH_ID = MATCH_ID %>% as.character() %>% as.factor())
  
  message("Missing values: ", sum(is.na(player_summary)))
  message("Saving player_summary data...")
  saveRDS(object = player_summary, file = "./clean_data/player_summary.rds")
}


process_team_summary = function(){
  (team_summary_names = list.files(path = "./raw_data", pattern = "Team Summary", full.names = TRUE))
  team_summary_list = purrr::map(team_summary_names, readr::read_csv)
  names(team_summary_list) = team_summary_names
  
  # team_summary_list
  
  team_summary_list %>% map_int(ncol)
  # gplots::venn(team_summary_list %>% map(colnames))
  
  team_summary = team_summary_list %>% bind_rows(.id = "file_name") %>% 
    dplyr::mutate(MATCH_ID = MATCH_ID %>% as.character() %>% as.factor())
  
  message("Missing values: ", sum(is.na(team_summary)))
  message("Saving team_summary data...")
  saveRDS(object = team_summary, file = "./clean_data/team_summary.rds")
}

process_all_players = function(){
  all_players = readr::read_csv("raw_data/Players 2018-2020.csv") %>% 
    dplyr::mutate(PERSON_ID = PERSON_ID %>% as.character() %>% as.factor())
  
  message("Saving all_player data...")
  saveRDS(object = all_players, file = "./clean_data/all_players.rds")
}

process_fixtures = function(){
  fixtures = readr::read_csv("raw_data/Fixtures 2018-2020.csv") %>% 
    dplyr::mutate(MATCH_ID = MATCH_ID  %>% as.character() %>% as.factor(),
                  HOME_SQUAD_ID  = HOME_SQUAD_ID %>% as.character() %>% as.factor())
  
  message("Saving fixtures data...")
  saveRDS(object = fixtures, file = "./clean_data/fixtures.rds")
}

process_player_summary()
process_team_summary()
process_all_players()
process_fixtures()
