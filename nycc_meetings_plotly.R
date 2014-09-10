library(devtools)
# install_github("ropensci/plotly")
library(plotly)

# set_credentials_file(username="rcquan", api_key="alu43a01xq")


library(stringr)

setwd("/Users/Quan/GitHub/nycc-legistar/")

nycMeetings <- read.csv("nyccMeetings.csv",
                        colClasses = c(rep("character", 5)),
                        stringsAsFactors = FALSE)


rmSpaces <- function(df) {gsub("^\\s+|\\s+$", "", df)}

nycMeetings <- data.frame(sapply(nycMeetings, rmSpaces))

nycMeetings[nycMeetings == ""] <- NA
head(nycMeetings)

library(dplyr)
library(ggplot2)
nycMeetings_df <- tbl_df(nycMeetings)

nycMeetings_df$Date <- as.POSIXct(nycMeetings$Date, format = "%m/%d/%Y")

# Remove defunct committees & Committee on Finance (outliers)
nycMeetings <- nycMeetings_df %>%
        mutate(DateTime = paste(Date,Time),
               Name = str_replace(Name, "Committee on ", ""),
               Month = factor(format(Date, format = "%b"), levels = month.abb),
               Year = factor(format(Date, format = "%Y")),
               Deferred = ifelse(Time == "Deferred", "Deferred", "Calendared")) %>%
        filter(!str_detect(Name, ignore.case("Inactive"))) %>%
        filter(!str_detect(Name, ignore.case("Subcommittee"))) %>%
        filter(!str_detect(Name, ignore.case("Task"))) %>%
        filter(!str_detect(Name, ignore.case("Town"))) %>%
        filter(!str_detect(Name, ignore.case("Whole"))) %>%
#         filter(!str_detect(Name, ignore.case("Finance"))) %>%
        filter(Deferred == "Calendared")
#         group_by(Name, Month) %>%
#         summarise(Mean.Month = mean(length(Name)))


tab <- data.frame(table(nycMeetings$Name, nycMeetings$Month))
names(tab) <- c("Name", "Month", "Count")
        

# Last two years -
# How do complaints correlate with meetings 

plot_meetings <- ggplot(tab, aes(x = Month, y = Count, group = Name)) + 
        geom_line(aes(color = Name)) +
        ggtitle("Total NYC Council Committee Meetings \n Held by Month from 2000-2013") + 
        theme(legend.position = "none")
        
plot_meetings

# initialize plotly()
py <- plotly()


r <- py$ggplotly(plot_meetings)
r$response$url

