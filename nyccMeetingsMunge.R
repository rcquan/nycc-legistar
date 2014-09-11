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
library(rCharts)
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
# change column names
names(nycMeetings) <- c("Name", "Date", "Time", "Location", "Topic")
nycMeetings$Date <- as.POSIXct(nycMeetings$Date, format = "%m/%d/%Y")

# Remove defunct committees & Committee on Finance (outliers)
nycMeetings_plot <- nycMeetings %>%
    # filter out 
    filter(!str_detect(Name, ignore.case("Inactive"))) %>%
    filter(!str_detect(Name, ignore.case("Subcommittee"))) %>%
    filter(!str_detect(Name, ignore.case("Task"))) %>%
    filter(!str_detect(Name, ignore.case("Town"))) %>%
    filter(!str_detect(Name, ignore.case("Finance"))) %>%
    filter(!str_detect(Name, ignore.case("Committee of the Whole"))) %>%
    mutate(DateTime = paste(Date,Time),
           # clean "name" variable
           Name = str_replace(Name, "Committee on ", ""),
           Name = str_replace(Name, ",.*$", ""),
           Name = str_replace(Name, "and Solid Waste Management", ""),
           # create new columns for month and year with proper format
           Month = factor(format(Date, format = "%b"), levels = month.abb),
           Year = factor(format(Date, format = "%Y")),
           # create new column for Status
           Status = ifelse(Time == "Deferred", "Deferred", "Calendared"))
        
#         group_by(Name, Month) %>%
#         summarise(Mean.Month = mean(length(Name)))

# annual meetings by committee (2000-2013)
nycMeetings_plot %>%
    filter(Status == "Calendared") %>%
    group_by(Name, Year) %>%
    summarise(Count = n()) %>%
    filter(Year %in% c(2000:2013)) %>%
    # 
    ggplot(aes(x = Year, y = Count, group = 1)) + 
    geom_line(aes(color = Name)) + facet_wrap(~Name) +
    ggtitle("Number of Meetings") + 
    theme(legend.position = "none") + 
    theme(axis.text.x = element_text(angle = 60, hjust = 1))
    
# total committee meetings by month (2000-2013)
nycMeetings_plot %>%
    filter(Status == "Calendared") %>%
    filter(Year %in% c(2000:2013)) %>%
    group_by(Name, Month) %>%
    summarise(Count = n()) %>%
    
    ggplot(aes(x = Month, y = Count, fill = Name)) + 
    geom_bar(stat = "identity") +
    ggtitle("Number of Meetings") + 
    theme_bw() +
    scale_fill_hue(l=40)

# top 10 deferral/calendared by committee
test <- nycMeetings_plot %>%
    dcast(Name ~ Status) %>%
    mutate(Total = Deferred + Calendared) %>%
    mutate(Percent = (Deferred / (Calendared + Deferred))) %>%
    arrange(desc(Percent)) %>%
    top_n(5) %>%
    melt() %>%
    filter(variable %in% c("Calendared", "Deferred"))

nPlot(value ~ Name, group = "variable", data = test, type = "multiBarChart")

# 
test <- nycMeetings_plot %>%
    filter (Year %in% c(2009:2013)) %>%
    group_by(Year, Status) %>%
    summarise(Count = n()) %>%
    mutate(Count = as.integer(Count))

nPlot(Count ~ Year, group = "Status", data = test, type = "multiBarChart")

# Last two years -
# How do complaints correlate with meetings 


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


