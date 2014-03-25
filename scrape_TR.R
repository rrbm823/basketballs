require(XML)
require(plyr)
for(i in 1:320){
  mainurl <- paste0("http://www.databasesports.com/ncaab/collegepage.htm?teamid=", i)

  ##xpaths
  p.name <- "//h1"
  Tree <- htmlTreeParse(mainurl, isURL = T, useInternalNodes = T)
  if(!is.null(Tree)) doc <- xmlRoot(Tree) else print(i)
  
  title <- unlist(xpathApply(doc, path=p.name, xmlValue, trim = T))
  title <- gsub(" NCAA.*", "", title)
  myTableNode <- getNodeSet(doc, path = "//table[@cellpadding='3']")
  if(!exists("tourney_record")){
    tourney_record <- readHTMLTable(myTableNode[[1]], header = T)
    tourney_record$team <- title
  } 
  else{
    x <- readHTMLTable(myTableNode[[1]], header = T)
    x$team <- title
    e <- tryCatch(rbind(tourney_record, x), error = function(e) e)
    if(inherits("e", "error")) next else tourney_record <- rbind(tourney_record, x)
  }
}
write.csv(as.data.frame(tourney_record), "TR.csv", row.names = F)
