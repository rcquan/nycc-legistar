#############################
# Ryan Quan
# NYCC Legistar Visualization
# Homework 1 , Problem 2
# 2014-09-15
#
# The following code visualizes
# the number of NYCC meetings held
# from 2000-2013
#############################
library(stringr)
library(dplyr)
library(XML)
library(ggplot2)
setwd("/Users/Quan/GitHub/nycc-legistar/")

CreateTable <- function(file){
    # parses HTML tree and retreives values
    # from tables as data.frame
    doc <- htmlParse(file)
    tables <- readHTMLTable(doc, 
                            stringsAsFactors = FALSE,
                            na.strings = "")
    df <- tables[[1]]
    return(df)
}

RemoveSpaces <- function(df) {
    # regex to remove leading and lagging spaces
    gsub("^\\s+|\\s+$", "", df)
}

nycMeetings <- CreateTable("nyccMeetings.html")
# remove leading and lagging spaces
nycMeetings <- data.frame(sapply(nycMeetings, RemoveSpaces))
# convert blank strings to NA
nycMeetings[nycMeetings == ""] <- NA
names(nycMeetings) <- c("Name", "Date", "Time", "Location", "Topic")
head(nycMeetings)
nycMeetings$Date <- as.POSIXct(nycMeetings$Date, format = "%m/%d/%Y")

# Remove defunct committees & Committee on Finance (outliers)
nycMeetings_plot <- nycMeetings %>%
    filter(!str_detect(Name, ignore.case("Inactive"))) %>%
    filter(!str_detect(Name, ignore.case("Subcommittee"))) %>%
    filter(!str_detect(Name, ignore.case("Task"))) %>%
    filter(!str_detect(Name, ignore.case("Town"))) %>%
    filter(!str_detect(Name, ignore.case("Finance"))) %>%
    filter(!str_detect(Name, ignore.case("Committee of the Whole"))) %>%
    mutate(DateTime = paste(Date,Time),
           Name = str_replace(Name, "Committee on ", ""),
           Name = str_replace(Name, ",.*$", ""),
           Name = str_replace(Name, "and Solid Waste Management", ""),
           Month = factor(format(Date, format = "%b"), levels = month.abb),
           Year = factor(format(Date, format = "%Y")),
           Deferred = ifelse(Time == "Deferred", "Deferred", "Calendared"))
        
#         group_by(Name, Month) %>%
#         summarise(Mean.Month = mean(length(Name)))
         
nycMeetings_year <- nycMeetings_plot %>%
        filter(Deferred == "Calendared") %>%
        group_by(Name, Year) %>%
        summarise(Count = n()) %>%
        filter(Year %in% c(2000:2013))

nycMeetings_month <- nycMeetings_plot %>%
        filter(Deferred == "Calendared") %>%
        filter(Year %in% c(2000:2013)) %>%
        group_by(Name, Month) %>%
        summarise(Count = n())

# Last two years -
# How do complaints correlate with meetings 


ggplot(nycMeetings_year, aes(x = Year, y = Count, group = 1)) + 
    geom_line(aes(color = Name)) + facet_wrap(~Name) +
    ggtitle("Number of Meetings") + 
    theme(legend.position = "none") + 
    theme(axis.text.x = element_text(angle = 60, hjust = 1))

ggplot(nycMeetings_month, aes(x = Month, y = Count, group = Name)) + 
    geom_line(aes(color = Name)) +
    ggtitle("Number of Meetings")


# All Committees
ggplot(nycMeetings_year, aes(x = Year, y = Count, group = Name)) + 
    geom_line(aes(color = Name)) +
    ggtitle("Average")


# Facet Committees
ggplot(nycMeetings, aes(x = Month)) + 
    geom_bar(stat = "bin", aes(color = factor(Deferred))) + 
    facet_wrap(~Name)


# Does the distribution of meetings match up with the number of legislation? 

# nycMeetings$Time[nycMeetings$Time == "Deferred"]
# qplot(nycMeetings$Name, col = nycMeetings$Time == "Deferred")
# table(nycMeetings$Name, nycMeetings$Time == "Deferred")
# 
# 
# nycMeetings$DateTime <- paste(nycMeetings$Date, nycMeetings$Time, sep = " ")


