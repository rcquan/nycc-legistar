#############################
# Ryan Quan
# NYCC Legistar Visualization
# 
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

# change to your working directory
setwd("/Users/Quan/GitHub/nycc-legistar/nycc-meetings")

#############################
# FUNCTIONS
#############################

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

#############################
# PROCESSING
#############################

df <- CreateTable("nycc-meetings.html")
# remove leading and lagging spaces
df <- data.frame(sapply(df, RemoveSpaces))
# convert blank strings to NA
df[df == ""] <- NA
# change column names
names(df) <- c("Name", "Date", "Time", "Location", "Topic")
df$Date <- as.POSIXct(df$Date, format = "%m/%d/%Y")

# Remove defunct committees & Committee on Finance (outliers)
df_plot <- df %>%
    # filter out inactive and small committees
    filter(!str_detect(Name, ignore.case("Inactive"))) %>%
    filter(!str_detect(Name, ignore.case("Subcommittee"))) %>%
    filter(!str_detect(Name, ignore.case("Task"))) %>%
    filter(!str_detect(Name, ignore.case("Town"))) %>%
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

#############################
# VISUALIZATION
#############################

# annual meetings by committee (2000-2013)
df_plot %>%
    # consider only calendared meetings
    filter(Status == "Calendared") %>%
    # remove finance committee, an outlier
    filter(!str_detect(Name, ignore.case("Finance"))) %>%
    group_by(Name, Year) %>%
    summarise(Count = n()) %>%
    filter(Year %in% c(2000:2013)) %>%
    # facet plot by committees
    ggplot(aes(x = Year, y = Count, group = 1)) + 
    geom_line(aes(color = Name)) + facet_wrap(~Name) +
    ggtitle("Annual Meetings Held by New York City Council Committees (2000-2013)") + 
    theme(legend.position = "none") + 
    theme(axis.text.x = element_text(angle = 60, hjust = 1))


