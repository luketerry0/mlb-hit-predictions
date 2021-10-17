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
      score = score - weightCoef*abs(mean(unlist(p1[key]), na.rm = TRUE) - mean(unlist(p2[key]), na.rm = TRUE))
    }
    
  }else{
    #calculate similarity score for pitchers, not batters
    
    pt <- Lahman::Pitching
    p1 <- subset(pt, playerID == pID1)
    p2 <- subset(pt, playerID == pID2)
    
    #start at 1000 and subtract to the correct score
    score = 1000
    
    #add winning percentage column
    p1$WinP <- p1$W/p1$G
    p2$WinP <- p2$W/p2$G
    
    #define each attribute, and how much of a difference needs to exist between the two players to subtract a point
    columns <- c("W", "L", "WinP", "ERA", "G", "GS", "CG", "IPouts", "H", "SO", "BB", "SHO", "SV") #stat column names in p1/p2 data frames
    weights <- c(1, 2, .002, .02, 10, 20, 20, 150, 50, 30, 10, 5, 3) #the amount of difference that corresponds to one point subtracted
    
    #subtract the appropriate amount based on the differences in each statistic
    for (i in 1:length(columns)){
      weightCoef = 1/weights[i]
      key = columns[i]
      diff = weightCoef*abs(mean(unlist(p1[key]), na.rm = TRUE) - mean(unlist(p2[key]), na.rm = TRUE))
      
      if ((key != "ERA") && (key != "WinP")){ #check that the key isn't ERA or WinP, which are capped at a difference of score of 100
      score = score - diff
      }else if (diff > 100){
        score = score - 100
      }
    }
  }
  return(score)
}

