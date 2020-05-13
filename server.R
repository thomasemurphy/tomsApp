#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(dplyr)
library(ggplot2)
library(reshape)

all_years_data <- read.csv("data/all_years_leaderboard.csv")
all_years_data[is.na(all_years_data[,])] <- 0
all_years_data$Total <- all_years_data$Round1 + all_years_data$Round2 +
    all_years_data$Round3 + all_years_data$Finals

yhlist <- list("2015"=14, "2016"=23, "2017"=39, "2018"=52, "2019"=73)

champs_list <- list("2015"="Finals: Warriors over Cavs in 6",
                    "2016"="Finals: Cavs over Warriors in 7",
                    "2017"="Finals: Warriors over Cavs in 5",
                    "2018"="Finals: Warriors over Cavs in 4",
                    "2019"="Finals: Raptors over Warriors in 6")

# Define server logic required to draw a histogram
shinyServer(function(input, output) {

    output$table <- renderTable({
        this_year_data <- subset(all_years_data, Year==input$year)
        this_year_data[,1] <- 1:nrow(this_year_data)
        this_year_data <- this_year_data[,-2]
        names(this_year_data) <- c("Rank", "Name", "Round 1", "Round 2", "Round 3", "Finals", "Total")
        this_year_data}, digits=0)
    
    output$chart <- renderPlot({
        
        this_year_data <- subset(all_years_data, Year==input$year)
        this_year_data[,1] <- 1:nrow(this_year_data)
        this_year_data <- this_year_data[,-2]
        names(this_year_data) <- c("Rank", "Name", "Round 1", "Round 2", "Round 3", "Finals", "Total")
        
        this_year_data$Name <- reorder(this_year_data$Name, this_year_data$Total)
        
        data_for_ggp <- melt(subset(this_year_data, select=-Total), id=c("Rank", "Name"))
        
        ggplot(data_for_ggp, aes(x=Name, y=value, fill=variable, label=value)) +
            geom_bar(stat="identity", position = position_stack(reverse = TRUE)) +
            xlab("") +
            ylab("") +
            coord_flip() +
            geom_text(size = 2, position = position_stack(vjust=0.5, reverse=TRUE)) +
            scale_fill_brewer(palette="Blues") +
            theme(legend.position = "top", legend.title=element_blank())
        
    },
    height = exprToFunction(yhlist[[as.character(input$year)]]*20)
        )
    
    output$champ_data <- renderText({champs_list[[as.character(input$year)]]})
    # output$champ_data <- renderText("What about this")

})
