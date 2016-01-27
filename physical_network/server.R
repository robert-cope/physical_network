####################################################
### Please see:
### Cope RC, Ross JV, Wittmann TA, Prowse TAA, Cassey P. (2016) Integrative Analysis of the Physical Transport Network into Australia. PLoS One. In Press.
### Robert C. Cope (c) 2016 
### robert.cope@adelaide.edu.eu 
####################################################

Sys.setlocale('LC_ALL','C')

library(ggplot2)
library(maps)
statesSrcY<-read.csv('statesSrcY3.csv',header=T,stringsAsFactors=F,sep=' ')
statesSrcYm<-read.csv('statesSrcY3-m.csv',header=T,stringsAsFactors=F,sep=' ')
shinyServer(function(input, output) {
  
  yearInput <- reactive({
    switch(input$year,
           '1999' = 1999,
           '2000' = 2000,
           '2001' = 2001,
           '2002' = 2002,
           '2003' = 2003,
           '2004' = 2004,
           '2005' = 2005,
           '2006' = 2006,
           '2007' = 2007,
           '2008' = 2008,
           '2009' = 2009,
           '2010' = 2010,
           '2011' = 2011,
           '2012'=2012)
  })
  yearBase <- reactive({
    switch(input$yearB,
           'N/A' = 0,
           '1999' = 1999,
           '2000' = 2000,
           '2001' = 2001,
           '2002' = 2002,
           '2003' = 2003,
           '2004' = 2004,
           '2005' = 2005,
           '2006' = 2006,
           '2007' = 2007,
           '2008' = 2008,
           '2009' = 2009,
           '2010' = 2010,
           '2011' = 2011,
           '2012'=2012)
  })
  
  volPassR<-reactive({
    switch(input$volPass,
           'vol' = 1,
           'pass' = 0)
  })
  
  lastOrAll<-reactive({
    switch(input$lastOrAll,
           'last' = 1,
           'all' = 0)
  })
  

  
  
  output$distPlot <- renderPlot({
    ######
    
    if(lastOrAll() == 1){
    if(yearBase() == 0){
      yr = yearInput()
    map.world <- map_data(map = "world")
    p1<-ggplot(map.world, aes(x = long, y = lat,group=group))
    p1<-p1+geom_polygon()#+coord_cartesian(xlim = c(100, 180),ylim=c(-50,50))
    p1 <- p1 + theme(legend.position="none") # remove legend with fill colours
    statesSrcYY<-subset(statesSrcY,Year==yr)
    statesSrcYY$flightsPFreq <- ifelse(is.na(statesSrcYY$flightsPFreq),0,statesSrcYY$flightsPFreq)
    statesSrcYY$flightsCFreq <- ifelse(is.na(statesSrcYY$flightsCFreq),0,statesSrcYY$flightsCFreq)
    statesSrcYY$shipsFreq <- ifelse(is.na(statesSrcYY$shipsFreq),0,statesSrcYY$shipsFreq)
    statesSrcYY$flightsPTotal <- ifelse(is.na(statesSrcYY$flightsPTotal),0,statesSrcYY$flightsPTotal)
    if (volPassR() == 1){
      statesSrcYY$weightedTot<-statesSrcYY$flightsPFreq*input$wPass + input$wCargo*statesSrcYY$flightsCFreq+input$wShips*statesSrcYY$shipsFreq
    } else {
      statesSrcYY$weightedTot<-statesSrcYY$flightsPTotal/100*input$wPass + input$wCargo*statesSrcYY$flightsCFreq+input$wShips*statesSrcYY$shipsFreq
    }
    p1<-p1+geom_point(data=statesSrcYY,aes(x=x_coord_centroid,y=y_coord_centroid,size=weightedTot,group=StateID),col='red')
    print(p1)
    
    } else {
      yrB <- yearBase()
      yr = yearInput()
      map.world <- map_data(map = "world")
      p1<-ggplot(map.world, aes(x = long, y = lat,group=group))
      p1<-p1+geom_polygon()#+coord_cartesian(xlim = c(100, 180),ylim=c(-50,50))
      p1 <- p1 + theme(legend.position="none") # remove legend with fill colours
      statesSrcYY<-subset(statesSrcY,Year==yr)
      statesSrcYY$flightsPFreq <- ifelse(is.na(statesSrcYY$flightsPFreq),0,statesSrcYY$flightsPFreq)
      statesSrcYY$flightsCFreq <- ifelse(is.na(statesSrcYY$flightsCFreq),0,statesSrcYY$flightsCFreq)
      statesSrcYY$shipsFreq <- ifelse(is.na(statesSrcYY$shipsFreq),0,statesSrcYY$shipsFreq)
      statesSrcYY$flightsPTotal <- ifelse(is.na(statesSrcYY$flightsPTotal),0,statesSrcYY$flightsPTotal)
      
      
      statesSrcYB<-subset(statesSrcY,Year==yrB)
      statesSrcYB$flightsPFreq <- ifelse(is.na(statesSrcYB$flightsPFreq),0,statesSrcYB$flightsPFreq)
      statesSrcYB$flightsCFreq <- ifelse(is.na(statesSrcYB$flightsCFreq),0,statesSrcYB$flightsCFreq)
      statesSrcYB$shipsFreq <- ifelse(is.na(statesSrcYB$shipsFreq),0,statesSrcYB$shipsFreq)
      statesSrcYB$flightsPTotal <- ifelse(is.na(statesSrcYB$flightsPTotal),0,statesSrcYB$flightsPTotal)
      
      
      if (volPassR() == 1){
        statesSrcYY$weightedTot<-statesSrcYY$flightsPFreq*input$wPass + input$wCargo*statesSrcYY$flightsCFreq+input$wShips*statesSrcYY$shipsFreq
        statesSrcYB$weightedTot<-statesSrcYB$flightsPFreq*input$wPass + input$wCargo*statesSrcYB$flightsCFreq+input$wShips*statesSrcYB$shipsFreq
        #statesSrcYY$propW <- statesSrcYY$weightedTot / sum(statesSrcYY$weightedTot)
        #statesSrcYB$propW <- statesSrcYB$weightedTot / sum(statesSrcYB$weightedTot)
      } else {
        statesSrcYY$weightedTot<-statesSrcYY$flightsPTotal/100*input$wPass + input$wCargo*statesSrcYY$flightsCFreq+input$wShips*statesSrcYY$shipsFreq
        statesSrcYB$weightedTot<-statesSrcYB$flightsPTotal/100*input$wPass + input$wCargo*statesSrcYB$flightsCFreq+input$wShips*statesSrcYB$shipsFreq
        #statesSrcYY$propW <- statesSrcYY$weightedTot / sum(statesSrcYY$weightedTot)
        #statesSrcYB$propW <- statesSrcYB$weightedTot / sum(statesSrcYB$weightedTot)
      }
      statesSrcYY$propChange<- NA
      #print(statesSrcYB$propW)
      for(i in 1:length(statesSrcYY$propChange)){
        statesSrcYY[i,]$propChange <- abs(statesSrcYY[i,]$weightedTot - statesSrcYB[statesSrcYB$StateID ==statesSrcYY[i,]$StateID,]$weightedTot)
        
        #if(statesSrcYB[statesSrcYB$StateID ==statesSrcYY[i,]$StateID,]$propW > 0){
        #statesSrcYY[i,]$isMore<- statesSrcYY[i,]$propW / statesSrcYB[statesSrcYB$StateID ==statesSrcYY[i,]$StateID,]$propW
        #} else {
        #  statesSrcYY[i,]$isMore<-1
        #}
      }
      statesSrcYY$propChange <- statesSrcYY$propChange / max(statesSrcYY$propChange)
      p1<-p1+geom_point(data=statesSrcYY,aes(x=x_coord_centroid,y=y_coord_centroid,size=weightedTot,group=StateID,col=propChange))+scale_colour_gradientn(colours=c('lightblue','red'))
      print(p1)
    }
    #########
    } else {
    #########
    if(yearBase() == 0){
      yr = yearInput()
      map.world <- map_data(map = "world")
      p1<-ggplot(map.world, aes(x = long, y = lat,group=group))
      p1<-p1+geom_polygon()#+coord_cartesian(xlim = c(100, 180),ylim=c(-50,50))
      p1 <- p1 + theme(legend.position="none") # remove legend with fill colours
      statesSrcYY<-subset(statesSrcYm,Year==yr)
      statesSrcYY$flightsPFreqm <- ifelse(is.na(statesSrcYY$flightsPFreqm),0,statesSrcYY$flightsPFreqm)
      statesSrcYY$flightsCFreqm <- ifelse(is.na(statesSrcYY$flightsCFreqm),0,statesSrcYY$flightsCFreqm)
      statesSrcYY$shipsFreq <- ifelse(is.na(statesSrcYY$shipsFreq),0,statesSrcYY$shipsFreq)
      statesSrcYY$flightsPTotalm <- ifelse(is.na(statesSrcYY$flightsPTotalm),0,statesSrcYY$flightsPTotalm)
      if (volPassR() == 1){
        statesSrcYY$weightedTot<-statesSrcYY$flightsPFreqm*input$wPass + input$wCargo*statesSrcYY$flightsCFreqm+input$wShips*statesSrcYY$shipsFreq
      } else {
        statesSrcYY$weightedTot<-statesSrcYY$flightsPTotalm/100*input$wPass + input$wCargo*statesSrcYY$flightsCFreqm+input$wShips*statesSrcYY$shipsFreq
      }
      p1<-p1+geom_point(data=statesSrcYY,aes(x=x_coord_centroid,y=y_coord_centroid,size=weightedTot,group=StateID),col='red')
      print(p1)
      
    } else {
      yrB <- yearBase()
      yr = yearInput()
      map.world <- map_data(map = "world")
      p1<-ggplot(map.world, aes(x = long, y = lat,group=group))
      p1<-p1+geom_polygon()#+coord_cartesian(xlim = c(100, 180),ylim=c(-50,50))
      p1 <- p1 + theme(legend.position="none") # remove legend with fill colours
      statesSrcYY<-subset(statesSrcYm,Year==yr)
      statesSrcYY$flightsPFreqm <- ifelse(is.na(statesSrcYY$flightsPFreqm),0,statesSrcYY$flightsPFreqm)
      statesSrcYY$flightsCFreqm <- ifelse(is.na(statesSrcYY$flightsCFreqm),0,statesSrcYY$flightsCFreqm)
      statesSrcYY$shipsFreq <- ifelse(is.na(statesSrcYY$shipsFreq),0,statesSrcYY$shipsFreq)
      statesSrcYY$flightsPTotalm <- ifelse(is.na(statesSrcYY$flightsPTotalm),0,statesSrcYY$flightsPTotalm)
      
      
      statesSrcYB<-subset(statesSrcYm,Year==yrB)
      statesSrcYB$flightsPFreqm <- ifelse(is.na(statesSrcYB$flightsPFreqm),0,statesSrcYB$flightsPFreqm)
      statesSrcYB$flightsCFreqm <- ifelse(is.na(statesSrcYB$flightsCFreqm),0,statesSrcYB$flightsCFreqm)
      statesSrcYB$shipsFreq <- ifelse(is.na(statesSrcYB$shipsFreq),0,statesSrcYB$shipsFreq)
      statesSrcYB$flightsPTotalm <- ifelse(is.na(statesSrcYB$flightsPTotalm),0,statesSrcYB$flightsPTotalm)
      
      
      if (volPassR() == 1){
        statesSrcYY$weightedTot<-statesSrcYY$flightsPFreqm*input$wPass + input$wCargo*statesSrcYY$flightsCFreqm+input$wShips*statesSrcYY$shipsFreq
        statesSrcYB$weightedTot<-statesSrcYB$flightsPFreqm*input$wPass + input$wCargo*statesSrcYB$flightsCFreqm+input$wShips*statesSrcYB$shipsFreq
        #statesSrcYY$propW <- statesSrcYY$weightedTot / sum(statesSrcYY$weightedTot)
        #statesSrcYB$propW <- statesSrcYB$weightedTot / sum(statesSrcYB$weightedTot)
      } else {
        statesSrcYY$weightedTot<-statesSrcYY$flightsPTotalm/100*input$wPass + input$wCargo*statesSrcYY$flightsCFreqm+input$wShips*statesSrcYY$shipsFreq
        statesSrcYB$weightedTot<-statesSrcYB$flightsPTotalm/100*input$wPass + input$wCargo*statesSrcYB$flightsCFreqm+input$wShips*statesSrcYB$shipsFreq
        #statesSrcYY$propW <- statesSrcYY$weightedTot / sum(statesSrcYY$weightedTot)
        #statesSrcYB$propW <- statesSrcYB$weightedTot / sum(statesSrcYB$weightedTot)
      }
      statesSrcYY$propChange<- NA
      #print(statesSrcYB$propW)
      for(i in 1:length(statesSrcYY$propChange)){
        statesSrcYY[i,]$propChange <- abs(statesSrcYY[i,]$weightedTot - statesSrcYB[statesSrcYB$StateID ==statesSrcYY[i,]$StateID,]$weightedTot)
        
        #if(statesSrcYB[statesSrcYB$StateID ==statesSrcYY[i,]$StateID,]$propW > 0){
        #statesSrcYY[i,]$isMore<- statesSrcYY[i,]$propW / statesSrcYB[statesSrcYB$StateID ==statesSrcYY[i,]$StateID,]$propW
        #} else {
        #  statesSrcYY[i,]$isMore<-1
        #}
      }
      statesSrcYY$propChange <- statesSrcYY$propChange / max(statesSrcYY$propChange)
      p1<-p1+geom_point(data=statesSrcYY,aes(x=x_coord_centroid,y=y_coord_centroid,size=weightedTot,group=StateID,col=propChange))+scale_colour_gradientn(colours=c('lightblue','red'))
      print(p1)
    }
    #########
    }
    
  })
  
  output$distPlot2 <- renderPlot({
    yr = yearInput()
    statesSrcY$flightsPFreq <- ifelse(is.na(statesSrcY$flightsPFreq),0,statesSrcY$flightsPFreq)
    statesSrcY$flightsCFreq <- ifelse(is.na(statesSrcY$flightsCFreq),0,statesSrcY$flightsCFreq)
    statesSrcY$shipsFreq <- ifelse(is.na(statesSrcY$shipsFreq),0,statesSrcY$shipsFreq)
    statesSrcY$flightsPTotal <- ifelse(is.na(statesSrcY$flightsPTotal),0,statesSrcY$flightsPTotal)
    if (volPassR() == 1){
      statesSrcY$weightedTot<-statesSrcY$flightsPFreq*input$wPass + input$wCargo*statesSrcY$flightsCFreq+input$wShips*statesSrcY$shipsFreq
    } else {
      statesSrcY$weightedTot<-statesSrcY$flightsPTotal/100*input$wPass + input$wCargo*statesSrcY$flightsCFreq+input$wShips*statesSrcY$shipsFreq
    }
    t1<-subset(statesSrcY,Year==yr)
    t1<-t1[order(-t1$weightedTot),]
    keyStates<-t1[1:10,]$StateID
    pos<-matrix(nrow=10,ncol=14)
    for(i in 1:14){
      t2<-subset(statesSrcY,Year==1998+i)
      t2<-t2[order(-t2$weightedTot),]
      for (j in 1:10){
        pos[j,i] <- which(t2$StateID == keyStates[j])
      }
    }
    plot(1,xlim=c(1999,2012),ylim=c(1,10),xlab='Year',ylab='')
    for (j in 1:10){
      points(1999:2012,pos[j,],col=j,lwd=2,type='l')
    }
    abline(v=yr,col='red',lty=2)
  })
  
  
  output$view <- renderTable({
    if(lastOrAll()==1){
    #######
    yr = yearInput()
    statesSrcYY<-subset(statesSrcY,Year==yr)
    statesSrcYY$flightsPFreq <- ifelse(is.na(statesSrcYY$flightsPFreq),0,statesSrcYY$flightsPFreq)
    statesSrcYY$flightsCFreq <- ifelse(is.na(statesSrcYY$flightsCFreq),0,statesSrcYY$flightsCFreq)
    statesSrcYY$shipsFreq <- ifelse(is.na(statesSrcYY$shipsFreq),0,statesSrcYY$shipsFreq)
    statesSrcYY$flightsPTotal <- ifelse(is.na(statesSrcYY$flightsPTotal),0,statesSrcYY$flightsPTotal)
    if (volPassR() == 1){
      statesSrcYY$weightedTot<-statesSrcYY$flightsPFreq*input$wPass + input$wCargo*statesSrcYY$flightsCFreq+input$wShips*statesSrcYY$shipsFreq
    } else {
      statesSrcYY$weightedTot<-statesSrcYY$flightsPTotal/100*input$wPass + input$wCargo*statesSrcYY$flightsCFreq+input$wShips*statesSrcYY$shipsFreq
    }
    statesSrcYY<-statesSrcYY[order(-statesSrcYY$weightedTot),]
    tM<-head(statesSrcYY[,c(1,26,27,28,29,30)],n=10)
    colnames(tM)<- c("Source_region", "Passenger_Flights", "Cargo_Flights","Ships","#Passengers_flying", "Weighted_cumulative_impact_(I)")
    #M <- print(xtable(tM,digits=c(0,0,0,0,0,0,0)), 
    #           floating=FALSE, tabular.environment="array", comment=FALSE, print.results=FALSE,include.rownames=F)
    #html <- paste0("$$", M, "$$")
    #list(
    #  withMathJax(HTML(html))
    #)
    tM
    ########
    } else {
    ########
    yr = yearInput()
    statesSrcYY<-subset(statesSrcYm,Year==yr)
    statesSrcYY$flightsPFreqm <- ifelse(is.na(statesSrcYY$flightsPFreqm),0,statesSrcYY$flightsPFreqm)
    statesSrcYY$flightsCFreqm <- ifelse(is.na(statesSrcYY$flightsCFreqm),0,statesSrcYY$flightsCFreqm)
    statesSrcYY$shipsFreq <- ifelse(is.na(statesSrcYY$shipsFreq),0,statesSrcYY$shipsFreq)
    statesSrcYY$flightsPTotalm <- ifelse(is.na(statesSrcYY$flightsPTotalm),0,statesSrcYY$flightsPTotalm)
    if (volPassR() == 1){
      statesSrcYY$weightedTot<-statesSrcYY$flightsPFreqm*input$wPass + input$wCargo*statesSrcYY$flightsCFreqm+input$wShips*statesSrcYY$shipsFreq
    } else {
      statesSrcYY$weightedTot<-statesSrcYY$flightsPTotalm/100*input$wPass + input$wCargo*statesSrcYY$flightsCFreqm+input$wShips*statesSrcYY$shipsFreq
    }
    statesSrcYY<-statesSrcYY[order(-statesSrcYY$weightedTot),]
    tM<-head(statesSrcYY[,c(1,26,27,28,29,30)],n=10)
    colnames(tM)<- c("Source_region", "Passenger_Flights", "Cargo_Flights","Ships","#Passengers_flying", "Weighted_cumulative_impact_(I)")
    #M <- print(xtable(tM,digits=c(0,0,0,0,0,0,0)), 
    #           floating=FALSE, tabular.environment="array", comment=FALSE, print.results=FALSE,include.rownames=F)
    #html <- paste0("$$", M, "$$")
    #list(
    #  withMathJax(HTML(html))
    #)
    tM
    ########
    }
  },include.rownames=F,digits=c(0,0,0,0,0,0,0))
  

})
