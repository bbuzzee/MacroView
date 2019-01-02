#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(FredR)
library(shiny)
library(ggplot2)
library(tidyverse)
library(lubridate)
library(plotly)

api.key <- "59f051c54cc49a42ef1f3ba3426792b8"
fred <- FredR(api.key)


# Define UI for application that draws a histogram
ui <- fluidPage(

   # Application title
  tabsetPanel(
    tabPanel("Home",

   titlePanel("MacroView"),

   # Sidebar with a slider input for number of bins
   helpText("Insert text about app")

   ),
   tabPanel("Fed Data",
            plotlyOutput("FFPlot"))
)

)

# Define server logic required to draw a histogram
server <- function(input, output) {




  #=== FEDERAL FUNDS RATE PLOT ===#

  FF <- fred$series.observations("DFF")

   output$FFPlot <- renderPlotly({

     FF <- fred$series.observations("DFF")

     dt <-  FF %>% select(date, value) %>% as_tibble() %>%
       mutate(Date = as.Date(date), Rate = as.numeric(value)/100) %>%
       group_by(Date = floor_date(Date, unit = "month")) %>% summarize(Rate = mean(Rate))


     ff_gg <- dt %>% ggplot(aes(x = Date, y = Rate)) + geom_smooth(span = .25, se = FALSE, size = 1.5) + geom_line() + ggtitle("Federal Funds Rate")

     return(ggplotly(ff_gg))
   })


}

# Run the application
shinyApp(ui = ui, server = server)

