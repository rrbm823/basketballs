clusterExport <- local({
  gets <- function(n, v) { assign(n, v, envir = .GlobalEnv); NULL }
  function(cl, list, envir = .GlobalEnv) {
    ## do this with only one clusterCall--loop on slaves?
    for (name in list) {
      clusterCall(cl, gets, name, get(name, envir = envir))
    }
  }
})

# Functions
createCluster = function(noCores, logfile = "/dev/null", export = NULL, lib = NULL) {
  require(doSNOW)
  cl <- makeCluster(noCores, type = "SOCK", outfile = logfile)
  if(!is.null(export)) clusterExport(cl, export)
  if(!is.null(lib)) {
    l_ply(lib, function(dum) { 
      clusterExport(cl, "dum", envir = environment())
      clusterEvalQ(cl, library(dum, character.only = TRUE))
    })
  }
  registerDoSNOW(cl)
  return(cl)
}

cl <- createCluster(4, export = list("getSAGP", "getRankSet", "a", "ordinal_ranks_core_33", "the_game"))


require(plyr)
a <- lapply(list.files(pattern = ".csv"), function(i) assign(gsub(".csv", "", i), read.csv(i, stringsAsFactors = F), envir = .GlobalEnv))
n <- lapply(a, names)
s <- lapply(a, summary)
##create ids for all the extra data
for(i in ls()){
  if("wteam"%in%names(get(i))) {
    data <- get(i)
    cl <- createCluster(4, export = list("data"))
    data$id <- laply(1:nrow(data), function(i){
      if(data$wteam[i] < data$lteam[i]){
        data[i,"id"] <- paste(data[i,"season"], data[i,"wteam"], data[i,"lteam"], sep = "_")
      }else data[i,"id"] <- paste(data[i,"season"], data[i,"lteam"], data[i,"wteam"], sep = "_")
    }, .parallel = TRUE)
    assign(i, data)
  }
}

test <- a[[7]]
test$team_a <- laply(1:nrow(a[[2]]), function(i) if(a[[2]]$wteam[i] < a[[2]]$lteam[i]) a[[2]]$wteam[i] else a[[2]]$lteam[i])
test$team_b <- laply(1:nrow(a[[2]]), function(i) if(a[[2]]$wteam[i] > a[[2]]$lteam[i]) a[[2]]$wteam[i] else a[[2]]$lteam[i])
test$rpi_a <- laply(1:nrow(a[[2]]), function(i) getRPI(a[[2]]$season[i], 130, test$team_a[i]), .parallel = TRUE)
test$rpi_b <- laply(1:nrow(a[[2]]), function(i) getRPI(a[[2]]$season[i], 130, test$team_b[i]), .parallel = TRUE)
the_game$sag_a <- laply(1:nrow(the_game), function(i) getSAGP(the_game$season[i], the_game$team_a[i]), .parallel = TRUE)
the_game$sag_b <- laply(1:nrow(the_game), function(i) getSAGP(the_game$season[i], the_game$team_b[i]), .parallel = TRUE)
t <- as.data.frame(laply(444:(nrow(the_game)), function(i){
  team_b <- unlist(strsplit(the_game$id[i], "_"))[3]
  getRankSet(the_game$season[i], team_b)
}, .parallel = TRUE))
s <- as.data.frame(laply(444:(nrow(the_game)), function(i){
  team_a <- unlist(strsplit(the_game$id[i], "_"))[2]
  getRankSet(the_game$season[i], team_a)
}, .parallel = TRUE))


svm_train_TRdrop2[,c("season", "team_a", "team_b")] <- laply(strsplit(svm_train_TRdrop2$id, "_"), identity)


test$outcome <- ifelse(test$wteam < test$lteam, 1, 0)

tourney_results_rmNA <- tourney_results[-union(which(is.na(tourney_results$sag_a)),which(is.na(tourney_results$sag_b))),]
the_game[which(!is.na(the_game$sag_a)),"sag_a"] <- (the_game[which(!is.na(the_game$sag_a)), "sag_a"] - mean(the_game[which(!is.na(the_game$sag_a)), "sag_a"]))/(2*sd(the_game[which(!is.na(the_game$sag_a)), "sag_a"]))
the_game[which(!is.na(the_game$sag_b)),"sag_b"] <- (the_game[which(!is.na(the_game$sag_b)), "sag_b"] - mean(the_game[which(!is.na(the_game$sag_b)), "sag_b"]))/(2*sd(the_game[which(!is.na(the_game$sag_b)), "sag_b"]))
scale(the_game) = 2* the_game$sag_a

bbglm2 <- train(outcome~sag_a + sag_b, data = train, method = "glm", family = binomial, trControl = testControl("LGOCV", p = .9))
test_tourney_games <- test[which(test$id%in%tourney_results$id[which(tourney_results$daynum > 135)]),]
actual_tourney_games <- merge(test_tourney_games, tourney_results, by.x = "id", by.y = "id")

test$pred <- predict(bbglm2, test)
set.seed(24)

for(letter in c("N", "O", "P", "Q", "R", "S")){
  train_prevs <- svm_train_TRdrop2[which(svm_train_TRdrop2$season < letter),]
  model_name <- paste("bbsvm", letter, sep = "_")
  assign(model_name, 
         svm(outcome~.,
               data = train_prevs[,1:49], scale = TRUE,
             gamma = 1/33, epsilon = 0.01, cross = 5,
               probability = TRUE, fitted = TRUE, seed = 24))
  ts_tourney_games <- which(test$id%in%tourney_results[which(tourney_results$season == letter), "id"])
  print(ts_tourney_games)
  pred <- predict(get(model_name), svm_train_TRdrop2[which(svm_train_TRdrop2$season == letter),])
  pred_s <- (pred - min(pred)) / (max(pred) - min(pred))
  pred_s[which(pred_s == 0)] <- pred_s[which(pred_s == 0)] + .000001
  pred_s[which(pred_s == 1)] <- pred_s[which(pred_s == 1)] - .000001
  test$pred[ts_tourney_games] <- pred_s
  print(logloss(svm_train_TRdrop2[which(svm_train_TRdrop2$season == letter), "outcome"], pred_s))
}

logloss(actual_tourney_games$outcome, predict(bbglm2, actual_tourney_games))
upset2 <- which(abs(as.integer(tourney_results[which(tourney_results$id%in%test$id),"wteam"] < tourney_results[which(tourney_results$id%in%test$id),"lteam"]) - test[actual_tourney_game, "pred"]) > .8)
upsets2 <- test[actual_tourney_game,][upset2,]