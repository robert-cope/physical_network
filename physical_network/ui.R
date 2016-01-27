#library(shiny)

####################################################
### Please see:
### Cope RC, Ross JV, Wittmann TA, Prowse TAA, Cassey P. (2016) Integrative Analysis of the Physical Transport Network into Australia. PLoS One. In Press.
### Robert C. Cope (c) 2016 
### robert.cope@adelaide.edu.eu 
####################################################

shinyUI(fluidPage(
  
  titlePanel("Physical transport network into Australia"),
  
  sidebarLayout(
    sidebarPanel(
      sliderInput("wShips",
                  "Relative weight - ships:",
                  min = 0,
                  max = 50,
                  value = 1),
      sliderInput("wPass",
                  "Relative weight - passenger flights:",
                  min = 0,
                  max = 50,
                  value = 1),
      sliderInput("wCargo",
                  "Relative weight - cargo flights:",
                  min = 0,
                  max = 50,
                  value = 1),
      selectInput("year", "Choose a dataset:", 
                  choices = c("1999", "2000","2001","2002","2003","2004", "2005",
                              "2006","2007","2008","2009","2010","2011","2012"), selected='2012'),
      selectInput("yearB", "Choose a baseline:", 
                  choices = c('N/A',"1999", "2000","2001","2002","2003","2004", "2005",
                              "2006","2007","2008","2009","2010","2011","2012"),selected='N/A'),
      radioButtons('volPass', 'Compare to',
                   c("Volume"='vol',
                     "Passengers / 100"='pass'),selected='vol'),
      radioButtons('lastOrAll', 'Last-port-of-call or all of flight path',
                   c("Last port of call"='last',
                     "All airports in journey"='all'),selected='last'),
      plotOutput("distPlot2")
    ),
    
    mainPanel(
      plotOutput("distPlot",width='1000px',height='600px'),
      uiOutput("view")#,
    )
  )
))
