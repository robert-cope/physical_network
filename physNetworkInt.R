####################################################
### FOR REVIEW ONLY ################################
### Robert C. Cope (c) 2015 ########################
### robert.cope@adelaide.edu.eu ####################
####################################################

pList<-c('shiny','ggplot2','maps','ncf','sp','maptools','plyr','rgeos','rgdal','xtable')
for (package in pList) {
  if (!require(package, character.only=T, quietly=T)) {
    install.packages(package)
    library(package, character.only=T)
  }
}


runApp('test_app')
