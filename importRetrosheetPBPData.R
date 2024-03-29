importRetrosheetPBPData <- function(filePath)
{
  #reads in data with the default output from retrosheet's bevent DOS tool
  
  #read in the file
  df = read.csv(filePath, header = FALSE)

  #compile a vector of header names
  headerNames = c(
   "game_id",
   "visiting_team",
   "inning",
   "batting_team",
   "out",
   "balls",
   "strikes",
   "vis_score",
   "home_score",
   "res_batter",
   "res_batter_hand",
   "res_pitcher",
   "res_pitcher_hand",
   "first_runner",
   "second_runner",
   "third_runner",
   "event_text",
   "leadoff_flag",
   "pinchhit_flag",
   "defensive_position",
   "lineup_position",
   "event_type",
   "batter_event_flag",
   "ab_flag",
   "hit_value",
   "SH_flag",
   "SF_flag",
   "outs_on_play",
   "RBI_on_play",
   "wild_pitch_flag",
   "passed_ball_flag",
   "num_errors",
   "batter_dest",
   "runner_on_1st_dest",
   "runner_on_2nd_dest",
   "runner_on_3rd_dest"
  )
  
  
  #add header names to the data
  colnames(df) <- headerNames
  return(df)
}


