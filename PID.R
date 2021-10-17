PID <- function(first, last, debutDate = "", idType = "lahman"){
  #find the playerID of a player given their first and last name
  #debut is only used if there are multiple people with the same name. format "2013-07-23"
  
  ppl = Lahman::People
    player <- subset(ppl, nameFirst == first)
    player <- subset(player, nameLast == last)
    
    if (length(player$playerID) != 1){
      if (debutDate != ""){
        player <- subset(player, debut == debutDate)
      }else{
      warning("Debut date parameter necessary to discern correct player")
      }
    }
    
    if(idType == "lahman"){
      return(player$playerID)
    }else if(idType == "retrosheet"){
      return(player$retroID)
    }
}
