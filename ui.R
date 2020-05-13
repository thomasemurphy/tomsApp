#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggplot2)

# mylist <- list("2015"="400px", "2016"="800px")

# Define UI for application that draws a histogram
shinyUI(fluidPage(
    
    # Application title
    titlePanel("NBA Playoff Pool hosted by Me Tom Your Host"),

    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(
            # sliderInput("bins", "Number of bins:", min = 1, max = 50, value = 30),
            radioButtons("year", label="Year",
                         c("2015", "2016", "2017", "2018", "2019")),
            radioButtons("display", label="Display", c("Table", "Chart")),
            width=2
        ),

        # Show a plot of the generated distribution
        mainPanel(
            h4(textOutput('champ_data')),
            h4("Pool Final Standings:"),
            
            conditionalPanel(
                condition = "input.display == 'Table'",
                tableOutput('table')
                ),
            conditionalPanel(
                condition = "input.display == 'Chart'",
                plotOutput('chart')
                )
        )
    )
))
