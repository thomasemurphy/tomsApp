library(dplyr)
library(ggplot2)
library(wesanderson)
library(RColorBrewer)

make_year_df <- function(year){
  csv_filename <- paste0(as.character(year),"_lb.csv")
  df <- read.csv(csv_filename)
  
  if (year == 2019){
    names(df)[6] <- "Round1"; names(df)[7] <- "Round2"; names(df)[11] <- "Round3"; names(df)[12] <- "Finals"
  } else {
    for (i in 1:ncol(df)){
      names(df)[i] <- gsub(" ", "", as.character(df[1,i]))
    }
    df <- df[-1,]
  }
  
  for (i in 1:ncol(df)){
    if (length(grep("[Rr]ound", names(df)[i])) |
        length(grep("[Ff]inals", names(df)[i]))) {
      df[,i] <- as.numeric(as.character(df[,i]))
    }
    if (is.na(names(df)[i]) | names(df)[i]=="NA"){df <- df[,-i]}
  }
  
  rownames(df) <- 1:nrow(df)
  df[,"Year"] <- year
  
  if (year==2018){names(df)[names(df)=="Round4"] = "Finals"; df <- df[1:52,]}
  if (year==2016){df <- df[,-8]}
  
  df <- df %>% select(Year, Name, Round1, Round2, Round3, Finals)
  df
}

years <- 2015:2019

all_years_lb <- data.frame()
for (year in years){
  all_years_lb <- rbind(all_years_lb, make_year_df(year))
}

write.csv(all_years_lb, "all_years_leaderboard.csv")

all_years_data <- read.csv("all_years_leaderboard.csv")

# levels(all_years_data$Name) <- c(levels(all_years_data$Name), "King Clayton, Second of his Name, etc")
# 
# all_years_data$Name[all_years_data$Name ==
#                       'King Clayton, Second of his Name, Lord of the Andals and the First Men,
#                     Protector of the Realm, and the One True King of the Playoffs'] <-
#   'King Clayton, Second of his Name, etc'
# 
# levels(all_years_data$Name)[match(as.factor("King Clayton, Second of his Name, Lord of the Andals and the First Men,
#                     Protector of the Realm, and the One True King of the Playoffs"), levels(all_years_data$Name))] <- as.factor("King Clayton, Second of his Name, etc")

all_years_data[is.na(all_years_data[,])] <- 0
all_years_data$Total <- all_years_data$Round1 + all_years_data$Round2 +
  all_years_data$Round3 + all_years_data$Finals

this_year_data <- subset(all_years_data, Year==2019)
this_year_data[,1] <- 1:nrow(this_year_data)
this_year_data <- this_year_data[,-2]
names(this_year_data) <- c("Rank", "Name", "Round 1", "Round 2", "Round 3", "Finals", "Total")

# this_year_data <- this_year_data[order(-this_year_data$Total),]

this_year_data$Name <- reorder(this_year_data$Name, this_year_data$Total)

data_for_ggp <- melt(subset(this_year_data, select=-Total), id=c("Rank", "Name"))

ggplot(data_for_ggp, aes(x=Name, y=value, fill=variable, label=value)) +
  geom_bar(stat="identity", position = position_stack(reverse = TRUE)) +
  xlab("") +
  ylab("") +
  coord_flip() +
  geom_text(size = 2, position = position_stack(vjust=0.5, reverse=TRUE)) +
  scale_fill_manual(values=wes_palette(n=4, name="GrandBudapest2")) +
  # scale_fill_brewer(palette="Blues") +
  theme(legend.position = "top", legend.title=element_blank())




