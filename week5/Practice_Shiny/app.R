#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#



#font_home <- function(path = '') file.path('~', '.fonts', path)
#if (Sys.info()[['sysname']] == 'Linux' &&
#    system('locate wqy-zenhei.ttc') != 0 &&
#    !file.exists(font_home('wqy-zenhei.ttc'))) {
#  if (!file.exists('wqy-zenhei.ttc'))
#    shiny:::download(
#      'https://github.com/rstudio/shiny-examples/releases/download/v0.10.1/wqy-zenhei.ttc',
#      'wqy-zenhei.ttc'
#    )
#  dir.create(font_home())
#  file.copy('wqy-zenhei.ttc', font_home())
#  system2('fc-cache', paste('-f', font_home()))
#}
#rm(font_home)
library(shiny)
print(sessionInfo())
print(installed.packages())
library(tidyverse)
library(reshape2)
all_un<-read.csv("1529_ALLUN.csv")
why_test<-read.table("AGE_WHY.csv", header=T, sep=",")
#un_data <- read.csv("ROC_82-106_UNEMPLOYMENT.csv", header=T, sep=",")
#age_edu_data <- read.csv("AGE_EDU.csv", header=T, sep=",")
age_why_data <- read.csv("AGE_WHY.csv", header=T, sep=",")
#newas <-gather(un_data,AGE, age_un, -YEAR)
#age_edu <-gather(age_edu_data,education, edu_un,-c(AGE,YEAR))
age_why<-gather(age_why_data,why, why_un,-c(AGE,YEARS))
#attach(newas)
ui <- 
navbarPage(
  "中華民國失業率統計",
  tabPanel(
    "歷史總統計l",
    sidebarLayout(
      sidebarPanel(
        sliderInput(
          "bins",
          "Number of Year:",
          min = 82,
          max = 106,
          value = 1
        ),
        verbatimTextOutput("Result")
      ),
      mainPanel(
        plotOutput("distPlot")
      )
    )
  ),
  tabPanel(
    "Why",
    sidebarLayout(
     sidebarPanel(
       sliderInput(
         "bins2",
         "Number of Year:",
         min = 82,
         max = 106,
         value = 1
       ),
       selectInput(
         "select",
         label = h3("Select Why"),
         choices = levels(factor(age_why[,3])),
         selected = levels(factor(age_why[,3]))[1]
       )
     ),
     mainPanel(
       textOutput("TEXT1"),
       plotOutput("distPlot2"),
       plotOutput("distPlot3")
     )
    )
  )
)

# Define server logic required to draw a histogram        geom_hline(yintercept=all_un[input$bins-81,3], linetype="dashed", 
server <- function(input, output) {
  output$TEXT1 <- renderText(
    paste("Year:",input$bins2)
  )
  output$Result <-renderText(
    {
      paste(
      "Year:",input$bins,
      "Total is:",all_un[input$bins-81,2],
      "Male is:",all_un[input$bins-81,3],
      "Female is:",all_un[input$bins-81,4],
      sep="\n"
      )
    }
  )
  output$distPlot2 <- renderPlot(
    {
      ggplot(subset(age_why,YEARS==input$bins2&!AGE=="TOTAL"), aes(x=why, y=why_un, fill=AGE)) + 
        geom_bar(stat="identity",position = position_stack(reverse = TRUE)) +
        coord_flip() +
        theme(legend.position = "top")
    }
  )
  output$distPlot3 <- renderPlot(
    {
        dataX = subset(age_why_data,select = c("YEARS","AGE",input$select))
        dataX = dcast(dataX, YEARS~AGE,sum)
        colnames(dataX) =  c("YEARS","DELETE","FIF","TWEN","TWENFITH","TOTAL")
        dataX = dataX[-26,-2]
        ggplot(dataX,aes(x=YEARS)) +
        geom_line(aes(y = TOTAL,group = 1,color="TOTAL")) +
        geom_line(aes(y = FIF,group = 2,color="15~19")) +
        geom_line(aes(y = TWEN,group = 3,color="20~25")) +
        geom_line(aes(y = TWENFITH,group = 4,color="25~29")) +
        scale_x_continuous(breaks=dataX$YEAR)+
        scale_y_continuous(breaks=seq(min(dataX$FIF,dataX$TWEN,dataX$TWENFITH),max(dataX$TOTAL),(as.integer((max(dataX$TOTAL)-min(dataX$FIF,dataX$TWEN,dataX$TWENFITH))/40)))) +
        #scale_fill_manual(values=c("RED", "BLUE", "GREEN"))+
        #scale_colour_manual(values=c("black", "black", "black")) +
        geom_vline(xintercept = input$bins2,
                   color = "black", size=1)+
        geom_hline(yintercept=dataX[input$bins2-81,5], linetype="dashed",
                  color = "black", size=0.5)+
        geom_hline(yintercept=dataX[input$bins2-81,4], linetype="dashed", 
                   color = "black", size=0.5)+
        geom_hline(yintercept=dataX[input$bins2-81,3], linetype="dashed", 
                   color = "black", size=0.5) +
        geom_hline(yintercept=dataX[input$bins2-81,2], linetype="dashed", 
                     color = "black", size=0.5)
    }
  )  
   output$distPlot <- renderPlot({
      # generate bins based on input$bins from ui.R
      #x    <- age_edu[,1]
      #bins <- seq(min(x), max(x), length.out = input$bins + 1)
      # draw the histogram with the specified number of bins
      #hist(x, breaks = bins, col = 'darkgray', border = 'white')
      
      ggplot(all_un, aes(x = YEAR)) +
        geom_area(aes(y = TOTAL, group = 1,fill = "總計",color="TOTAL")) +
        geom_area(aes(y = MALE, group = 2,fill="男",color = "MALE")) +
        geom_area(aes(y = FEMALE, group = 3,fill="女",color = "FEMALE")) +
        scale_x_continuous(breaks=all_un$YEAR) +
        scale_y_continuous(breaks=seq(44,271,by=8)) +
        scale_fill_manual(values=c("RED", "BLUE", "GREEN"))+
        scale_colour_manual(values=c("black", "black", "black")) +
        geom_vline(xintercept = input$bins,
                   color = "black", size=1)+
        geom_hline(yintercept=all_un[input$bins-81,2], linetype="dashed",
                   color = "black", size=0.5)+
        geom_hline(yintercept=all_un[input$bins-81,3], linetype="dashed", 
                   color = "black", size=0.5)+
        geom_hline(yintercept=all_un[input$bins-81,4], linetype="dashed", 
                 color = "black", size=0.5)
   })
}

# Run the application 
shinyApp(ui = ui, server = server)

