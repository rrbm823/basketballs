#setwd("./R/basketballs/")
#load(".RData")


##function to calculate a team's RPI
RPI <- function(team, season){
  .25 * getWP(team, season) + .50 * getOWP(team, season) + .25 * getOOWP(team, season)
}

getWP <- function(team, season, rem.team = NULL, base = F) {
  wins <- a[[2]][intersect(which(a[[2]]$wteam == team), which(a[[2]]$season == season)),]
  losses <- a[[2]][intersect(which(a[[2]]$lteam == team), which(a[[2]]$season == season)),]
  if(!is.null(rem.team)){
    winsTargetRemd <- wins[-which(wins$lteam == rem.team),]
    lossesTargetRemd <- losses[-which(wins$wteam == rem.team),]
    if(nrow(winsTargetRemd) != 0) wins <- winsTargetRemd
    if(nrow(lossesTargetRemd) != 0) losses <- lossesTargetRemd
  }
  baseWP <- nrow(wins) / (nrow(wins) + nrow(losses))
  if(base) return(baseWP)
  away.wins <- nrow(wins[which(wins$wloc == "A"),])
  away.losses <- nrow(losses[which(losses$wloc == "H"),])
  home.wins <- nrow(wins[which(wins$wloc == "H"),])
  home.losses <- nrow(losses[which(losses$wloc == "A"),])
  neutral.wins <- nrow(wins[which(wins$wloc == "N"),])
  neutral.losses <- nrow(losses[which(losses$wloc == "N"),])
  total.wins <- 1.4*away.wins + 0.6*home.wins + neutral.wins
  total.losses <- 1.4*away.losses + 0.6*home.losses + neutral.losses
  weightedWP <- total.wins/(total.wins + total.losses)
  return(weightedWP)
}
  
getOWP <- function(team, season, remTarget = T){
  wins <- a[[2]][intersect(which(a[[2]]$wteam == team), which(a[[2]]$season == season)),]
  losses <- a[[2]][intersect(which(a[[2]]$lteam == team), which(a[[2]]$season == season)),]
  opponents <- c(wins$lteam, losses$wteam)
  if(remTarget == T){
    return(mean(laply(opponents, function(i) getWP(i, season, team, base = T))))
  }else return(mean(laply(opponents, function(i) getWP(i, season, base = T))))
}

getOOWP <- function(team, season){
  wins <- a[[2]][intersect(which(a[[2]]$wteam == team), which(a[[2]]$season == season)),]
  losses <- a[[2]][intersect(which(a[[2]]$lteam == team), which(a[[2]]$season == season)),]
  opponents <- c(wins$lteam, losses$wteam)
  return(mean(laply(opponents, function(i) getOWP(i, season, remTarget = F))))
}


##function to extract team RPI/SAGP from included spreadsheets
getRPI <- function(season, day, team) {
  a[[3]]$RPI[intersect(intersect(which(a[[3]]$season == get.alphabet(season)), which(a[[3]]$rating_day_num == as.character(day))), which(a[[3]]$team == get.team(as.character(team))))]
}

get.alphabet <- function(year){
  alpha <- c("A", "B", "C", "D", "E", "F", "G", "H", "I", "J", "K", "L", "M", "N", "O", "P", "Q", "R", "S", "T", "U", "V", "W", "X", "Y", "Z")
  if(!is.element(year, alpha)) alpha[year - 1995] else year
}
        
get.team <- function(team) {
  if(is.na(as.integer(team))){
    a[[6]]$id[which(a[[6]]$team == team)]
  } else a[[6]]$name[which(a[[6]]$id == team)]
}

getSeed <- function(season, wteam, lteam){
  list(a[[10]]$seed[intersect(which(a[[10]]$team == wteam), which(a[[10]]$season == season))], 
       a[[10]]$seed[intersect(which(a[[10]]$team == lteam), which(a[[10]]$season == season))])
}

getSAGP <- function(season, team){
  mean(a[[14]]$rating[intersect(which(a[[14]]$team == team), which(a[[14]]$season == season))])
}

getRankSet <- function(season, team){
  out <- matrix(NA, 1, 33)
  out <- as.data.frame(out)
  names(out) <- unique(a[[15]]$sys_name)[which(unique(a[[15]]$sys_name)%in%unique(ordinal_ranks_core_33$sys_name))]
  if(length(intersect(which(a[[15]]$season == season), which(a[[15]]$team == team))) == 0) return(out)
  team_ranks <- aggregate(orank ~ sys_name,
            data = a[[15]][intersect(which(a[[15]]$season == season), which(a[[15]]$team == team)),],
            FUN = mean,
            na.rm = T)
  for(i in names(out)){
    if(length(team_ranks$orank[which(team_ranks$sys_name == i)]) > 0) out[,i] <- team_ranks[which(team_ranks$sys_name == i), "orank"]
  }
  out
}

logloss <- function(y, yhat){
  -sum(y*log(yhat) + (1 - y)*log(1 - yhat))/length(y)
}
