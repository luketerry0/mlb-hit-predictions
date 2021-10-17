jamesianSimilarityScores <- function(pID1, pID2, startingYear = 1776, endingYear = 3000, batting = TRUE)
{
  #calculates a similarity score based on Bill James's balance of player statistics
  #inputs two playerIDs in Lahman's Format, and a year range (inclusive) for what stats to use
  #if batting is FALSE, the score is found between pitchers
  
  #use the Lahman database for batting data
  library(Lahman)
  
  if (batting){ #calculate scores for batters
    bt <- Lahman::Batting
    p1bt <- subset(bt, playerID == pID1)
    p2bt <- subset(bt, playerID == pID2)

    #start at 1000 and subtract to the correct score
    score = 1000
    
    #add batting avg. and slugging percentage to table
    p1 <- battingStats(p1bt)
    p2 <- battingStats(p2bt)
    
    #define each attribute, and how much of a difference needs to exist between the two players to subtract a point
    columns <- c("G", "AB", "R", "H", "X2B", "X3B", "HR", "RBI", "BB", "SO", "SB", "BA", "SlugPct") #stat column names in p1/p2 data frames
    weights <- c(20, 75, 10, 15, 5, 4, 2, 10, 25, 150, 20, .001, .002) #the amount of difference that corresponds to one point subtracted (20 games played diff = 1 pt)

    #subtract the appropriate amount based on the differences in each statistic
    for (i in 1:length(columns)){
      weightCoef = 1/weights[i]
      key = columns[i]
      score = score - weightCoef*(mean(unlist(p1[key])) - mean(unlist(p2[key])))
      
    }
  }
  return(score)
}
jamesianSimilarityScores("abercda01", "addybo01")

