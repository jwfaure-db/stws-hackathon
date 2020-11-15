library(tidyverse)

process_all_players = function(){
  all_players <<- readr::read_csv("raw_data/Players 2018-2020.csv") %>% 
    dplyr::mutate(PERSON_ID = PERSON_ID %>% as.character() %>% as.factor())
  
  brownlow_winners <<- readxl::read_excel("raw_data/brownlow_winners.xlsx")
    
  all_players <<- all_players %>% dplyr::mutate(
    BROWNLOW_WINNER = ifelse(FULLNAME %in% brownlow_winners$Player, 1, 0),
    AGE_EOS = SEASON_ID - year(DOB)
  )
  
  team_player_summary <<- all_players %>% group_by(SQUAD_NAME, SEASON_ID) %>% 
    dplyr::summarise(
      AVG_PLAYER_AGE = mean(AGE_EOS, na.rm = TRUE),
      AVG_PLAYER_HT = mean(CURRENT_HEIGHT, na.rm = TRUE),
      AVG_PLAYER_WT = mean(CURRENT_WEIGHT, na.rm = TRUE),
      BROWNLOW_MEDALISTS = sum(BROWNLOW_WINNER, na.rm = TRUE),
      AVG_CAREER_MATCHES = mean(CAREER_MATCHES_PRE_2018, na.rm = TRUE)
    ) %>% ungroup()
  
  message("Saving all_player data...")
  message("Missing values: ", sum(is.na(all_players)))
  message("Variables with highest proportion of missing values: ")
  print(all_players %>% is.na %>% colMeans %>% sort %>% rev() %>% head())
  saveRDS(object = all_players, file = "./clean_data/all_players.rds")
  saveRDS(object = team_player_summary, file = "./clean_data/team_player_summary.rds")
}

process_fixtures = function(){
  fixtures <<- readr::read_csv("raw_data/Fixtures 2018-2020.csv") %>% 
    dplyr::mutate(MATCH_ID = MATCH_ID  %>% as.character() %>% as.factor(),
                  HOME_SQUAD_ID  = HOME_SQUAD_ID %>% as.character() %>% as.factor())
  
  message("Saving fixtures data...")
  message("Missing values: ", sum(is.na(fixtures)))
  message("Variables with highest proportion of missing values: ")
  print(fixtures %>% is.na %>% colMeans %>% sort %>% rev() %>% head())
  saveRDS(object = fixtures, file = "./clean_data/fixtures.rds")
}

process_all_players()
process_fixtures()

process_player_summary = function(){
  (player_summary_names = list.files(path = "./raw_data", pattern = "Player Summary", full.names = TRUE))
  player_summary_list = purrr::map(player_summary_names, readr::read_csv)
  names(player_summary_list) = player_summary_names
  
  # player_summary_list
  
  # player_summary_list %>% map_int(ncol)
  # gplots::venn(player_summary_list %>% map(colnames))
  
  player_summary = player_summary_list %>% bind_rows(.id = "file_name") %>% 
    dplyr::mutate(MATCH_ID = MATCH_ID %>% as.character() %>% as.factor(),
                  PERSON_ID = PERSON_ID %>% as.character() %>% as.factor()) %>% 
    left_join(fixtures, by = c("MATCH_ID", "SEASON_ID", "GROUP_ROUND_NO", "VENUE_NAME")) %>%
    left_join(all_players, by = c("PERSON_ID", "FULLNAME", "SEASON_ID", "SQUAD_NAME"))
  
  message("Missing values: ", sum(is.na(player_summary)))
  message("Saving player_summary data...")
  
  message("Variables with highest proportion of missing values: ")
  print(player_summary %>% is.na %>% colMeans %>% sort %>% rev() %>% head())
  
  saveRDS(object = player_summary, file = "./clean_data/player_summary.rds")
}


process_team_summary = function(){
  (team_summary_names = list.files(path = "./raw_data", pattern = "Team Summary", full.names = TRUE))
  team_summary_list = purrr::map(team_summary_names, readr::read_csv)
  names(team_summary_list) = team_summary_names
  
  # team_summary_list
  
  # team_summary_list %>% map_int(ncol)
  # gplots::venn(team_summary_list %>% map(colnames))
  
  team_summary = team_summary_list %>% bind_rows(.id = "file_name") %>% 
    dplyr::mutate(MATCH_ID = MATCH_ID %>% as.character() %>% as.factor()) %>% 
    left_join(fixtures, by = c("MATCH_ID", "SEASON_ID", "GROUP_ROUND_NO", "VENUE_NAME"))
  
  opp_team_player_summary = team_player_summary %>% dplyr::rename_all(function(x) paste0("OPP_", x))
  
  team_summary = team_summary %>% dplyr::left_join(team_player_summary, by = c("SEASON_ID" = "SEASON_ID", "SQUAD_NAME" = "SQUAD_NAME")) %>% 
    dplyr::left_join(opp_team_player_summary, by = c("SEASON_ID" = "OPP_SEASON_ID", "OPP_SQUAD_NAME" = "OPP_SQUAD_NAME"))
  
  message("Missing values: ", sum(is.na(team_summary)))
  message("Saving team_summary data...")
  
  message("Variables with highest proportion of missing values: ")
  print(team_summary %>% is.na %>% colMeans %>% sort %>% rev() %>% head())
  
  saveRDS(object = team_summary, file = "./clean_data/team_summary.rds")
}

process_player_summary()
process_team_summary()
