#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
library(readr)
library(shiny)
library(ggplot2)

un_data <- read_csv("ROC_82-106_UNEMPLOYMENT.csv")
un_data_f<-data.frame(un_data[,-1])
row.names(un_data_f) <-c(82:106)
un_data_f
ggplot(un_data_f, aes()) + geom_col(width = 0.9)
# Define UI for application that draws a histogram



newas <-un_data %>%
  gather(age, un, -YEAR) 
ggplot(newas, aes(x=YEAR, y=age, fill=un)) + 
  geom_bar(stat="identity", position="dodge")










ui <- fluidPage(
   
   # Application title
   titlePanel("中華民國歷年失業率"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
         sliderInput("bins",
                     "Number of bins:",
                     min = 82,
                     max = 106,
                     value = 1)
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
         plotOutput("distPlot")
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
   
   output$distPlot <- renderPlot({
      # generate bins based on input$bins from ui.R
      x    <- un_data[,1]
      bins <- seq(min(x), max(x), length.out = input$bins + 1)
      
      # draw the histogram with the specified number of bins
      hist(x, breaks = bins, col = 'darkgray', border = 'white')
   })
}

# Run the application 
shinyApp(ui = ui, server = server)

