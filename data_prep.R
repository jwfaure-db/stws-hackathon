library(tidyverse)

process_all_players = function(){
  all_players <<- readr::read_csv("raw_data/Players 2018-2020.csv") %>% 
    dplyr::mutate(PERSON_ID = PERSON_ID %>% as.character() %>% as.factor())
  
  brownlow_winners <<- readxl::read_excel("raw_data/brownlow_winners.xlsx")
    
  all_players <<- all_players %>% dplyr::mutate(
    BROWNLOW_WINNER = ifelse(FULLNAME %in% brownlow_winners$Player, 1, 0),
    AGE_EOS = SEASON_ID - lubridate::year(DOB)
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
  
  team_summary <<- team_summary %>% dplyr::left_join(team_player_summary, by = c("SEASON_ID" = "SEASON_ID", "SQUAD_NAME" = "SQUAD_NAME")) %>% 
    dplyr::left_join(opp_team_player_summary, by = c("SEASON_ID" = "OPP_SEASON_ID", "OPP_SQUAD_NAME" = "OPP_SQUAD_NAME"))
  
  message("Missing values: ", sum(is.na(team_summary)))
  message("Saving team_summary data...")
  
  message("Variables with highest proportion of missing values: ")
  print(team_summary %>% is.na %>% colMeans %>% sort %>% rev() %>% head())
  
  saveRDS(object = team_summary, file = "./clean_data/team_summary.rds")
}

process_player_summary()
process_team_summary()


process_match_summary = function(){
  (team_summary_names = list.files(path = "./raw_data", pattern = "Team Summary", full.names = TRUE))
  team_summary_list = purrr::map(team_summary_names, readr::read_csv)
  names(team_summary_list) = team_summary_names
  
  # team_summary_list
  
  # team_summary_list %>% map_int(ncol)
  # gplots::venn(team_summary_list %>% map(colnames))
  
  match_summary = team_summary_list %>% bind_rows(.id = "file_name") %>% 
    dplyr::mutate(MATCH_ID = MATCH_ID %>% as.character() %>% as.factor()) %>% 
    left_join(fixtures, by = c("MATCH_ID", "SEASON_ID", "GROUP_ROUND_NO", "VENUE_NAME")) %>% 
    group_by(MATCH_ID, SEASON_ID, GROUP_ROUND_NO, VENUE_NAME, SQUAD_NAME, OPP_SQUAD_NAME, 
             SQUAD_MARGIN, ZONE_LOGICAL_AFL, HOME_SQUAD, AWAY_SQUAD,
             HOME_SQUAD_TRAVEL, AWAY_SQUAD_TRAVEL) %>% 
    dplyr::summarise(
      across(where(is.numeric), sum)
    ) %>% select(-c(PERIOD, AWAY_SQUAD_ID, HOME_SCORE, AWAY_SCORE, ATTENDANCE)) %>% ungroup()
  
  opp_team_player_summary = team_player_summary %>% dplyr::rename_all(function(x) paste0("OPP_", x))
  
  match_summary = match_summary %>% dplyr::left_join(team_player_summary, by = c("SEASON_ID" = "SEASON_ID", "SQUAD_NAME" = "SQUAD_NAME")) %>% 
    dplyr::left_join(opp_team_player_summary, by = c("SEASON_ID" = "OPP_SEASON_ID", "OPP_SQUAD_NAME" = "OPP_SQUAD_NAME"))
  
  message("Missing values: ", sum(is.na(match_summary)))
  message("Saving match_summary data...")
  
  message("Variables with highest proportion of missing values: ")
  print(team_summary %>% is.na %>% colMeans %>% sort %>% rev() %>% head())
  
  saveRDS(object = match_summary, file = "./clean_data/match_summary.rds")
}

process_match_summary()


process_team_rates = function(){
  team_rates = team_summary %>% dplyr::transmute(
    HOME_AWAY = ifelse(SQUAD_NAME == HOME_SQUAD, "home", "away"), # whether squad of interest is home team
    RESULT = ifelse(SQUAD_MARGIN == 0, "draw", ifelse(SQUAD_MARGIN > 0, "won", "lost")), # our outcome variable
    PERIOD = PERIOD %>% as.factor(),
    MATCH_ID = MATCH_ID,
    GROUP_ROUND_NO = GROUP_ROUND_NO,
    SQUAD_NAME = SQUAD_NAME,
    OPP_SQUAD_NAME = OPP_SQUAD_NAME,
    PERIOD = PERIOD,
    BU_RESULT_SCORE = BU_SCORE_LAUNCH / BALL_UP,
    BU_RESULT_IN50 = BU_IN50_LAUNCH / BALL_UP,
    CB_RESULT_SCORE = CB_SCORE_LAUNCH / CENTRE_BOUNCE,
    CB_RESULT_IN50 = CB_IN50_LAUNCH / CENTRE_BOUNCE,
    CM_NET_PG = CHAIN_METRES_NET_PG / CHAIN_METRES_NET,
    CM_NET_STOPPAGES = CHAIN_METRES_NET_ST / CHAIN_METRES_NET,
    CLANGER = CLANGER / POSSESSION,
    NON_DISPOSAL_CLANGERS = sum(CLANGER_GROUND_KICK, CLANGER_HANDBALL, CLANGER_KICK, na.rm = TRUE) / CLANGER,
    CP_PROP = CONTESTED_POSSESSION / POSSESSION,
    CRUMB_RATE = CRUMB / MARK_DROPPED,
    D50_REB_SCORE = D50_REB_SCORE / sum(D50_REB_IN50, D50_REBOUND, na.rm = TRUE),
    EFFECTIVE_DISPOSAL = EFFECTIVE_DISPOSAL / DISPOSAL,
    FIRST_POSSESSION_TO_CLEAR = FIRST_POSSESSION_TO_CLEAR / FIRST_POSSESSION,
    FREE_FOR_AGAINST_RATIO = FREE_FOR / FREE_AGAINST,
    GATHER_RATE = GATHER / KNOCK_ON,
    GROUND_BALL_GET = GROUND_BALL_GET,
    HANBALL_GAIN_METRES_PER_HANDBALL = HANDBALL_GAIN_METRES / HANDBALL,
    INEFFECTIVE_HANDBALL_RATE = INEFFECTIVE_HANDBALL / HANDBALL,
    IN50_KICK_EFFECTIVE = IN50_KICK_RETAIN / IN50_KICK,
    IN50_TARGET_RESULT_SCORE = IN50_TARGET_TEAM_SCORE / IN50_TARGET,
    INEFFECTIVE_KICK_RATE = INEFFECTIVE_KICK / KICK,
    KI_RESULT_SCORE = KI_SCORE_LAUNCH / KICK_IN,
    KI_RESULT_IN50 = KI_IN50_LAUNCH / KICK_IN,
    HARD_BALL_GET_RATE = HARD_BALL_GET / sum(HARD_BALL_GET, LOOSE_BALL_GET, na.rm = TRUE),
    HIT_OUT_TO_ADVANTAGE_RATE = HIT_OUT_TO_ADVANTAGE / HITOUT,
    DROPPED_MARK_RATE = MARK_DROPPED / MARK,
    MISSED_SHOT_RATE = MISSED_SHOT / SHOT_AT_GOAL,
    TACKLE_RATE = TACKLE / sum(MISSED_TACKLE, TACKLE, na.rm = TRUE),
    OUT_ON_FULL_RATE = OUT_ON_FULL / KICK,
    INTERCEPT_SCORES = sum(PG_GOAL_LAUNCH, PG_BEHIND_LAUNCH, na.rm = TRUE) / INTERCEPT,
    INTERCEPT_RESULT_IN50 = PG_IN50_LAUNCH / INTERCEPT,
    RETAIN_DISPOSAL = RETAIN_DISPOSAL / DISPOSAL,
    R50_TEAMMATE = sum(RE50_RESULT_TEAM_CP, RE50_RESULT_TEAM_UP, na.rm = TRUE) / REBOUND_50,
    RUNDOWN_TACKLE_RATE = RUNDOWN_TACKLE / TACKLE,
    THROW_IN_CLEARANCE = THROW_IN_CLEARANCE / THROW_IN,
    SCORE_FROM_STOPPAGE = ST_SCORE_LAUNCH / STOPPAGE,
    SPOIL_GAINING_POSSESSION = SPOIL_GAINING_POSSESSION / SPOIL,
    TI_SCORE = TI_SCORE_LAUNCH / THROW_IN,
    TI_IN50 = TI_IN50_LAUNCH / THROW_IN,
    TIME_IN_POSS_SQUAD_RATE = TIME_IN_POSS_SQUAD / TIME_IN_POSS_TOTAL,
    TURNOVER_RATE = TURNOVER / POSSESSION,
    SQUAD_PRESSURE_RATE = SQUAD_PRESSURE_ACTS / SQUAD_PRESSURE_CHANCE,
    STOPPAGE_SCORE_RATE = ST_SCORE_LAUNCH / STOPPAGE,
    EFFECTIVE_CLEARANCE_RATE = EFFECTIVE_CLEARANCE / CLEARANCE,
    RATING = RATING,
    STOP_EXIT_KICK_RATE = STOP_EXIT_K / STOP_EXIT,
    SWITCH_SCORE_RATE = SWITCH_SCORE / SWITCH_COMPLETE,
    SWITCH_IN50_RATE = SWITCH_IN50 / SWITCH_COMPLETE,
    SWITCH_BATTLE = SWITCH_COMPLETE / sum(SWITCH_COMPLETE, SWITCH_OPP, na.rm = TRUE),
    HOME_AWAY = HOME_AWAY,
    RESULT = RESULT,
    MATCH_TYPE = MATCH_TYPE,
    HOME_SQUAD_TRAVEL = HOME_SQUAD_TRAVEL,
    AWAY_SQUAD_TRAVEL = AWAY_SQUAD_TRAVEL,
    ZONE_LOGICAL_AFL = ZONE_LOGICAL_AFL,
    AVG_PLAYER_AGE = AVG_PLAYER_AGE,
    AVG_PLAYER_HT = AVG_PLAYER_HT,
    AVG_PLAYER_WT = AVG_PLAYER_WT,
    BROWNLOW_MEDALISTS = BROWNLOW_MEDALISTS, 
    AVG_CAREER_MATCHES = AVG_CAREER_MATCHES,
    OPP_AVG_PLAYER_AGE = OPP_AVG_PLAYER_AGE,
    OPP_AVG_PLAYER_HT = OPP_AVG_PLAYER_HT,         
    OPP_AVG_PLAYER_WT = OPP_AVG_PLAYER_WT,        
    OPP_BROWNLOW_MEDALISTS = OPP_BROWNLOW_MEDALISTS,
    OPP_AVG_CAREER_MATCHES = OPP_AVG_CAREER_MATCHES 
  )
  message("Saving team_rates data...")
  saveRDS(team_rates, file = "./clean_data/team_rates.rds")
}

process_team_rates()
