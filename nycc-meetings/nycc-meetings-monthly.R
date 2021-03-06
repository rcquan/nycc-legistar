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
library(ggvis)

# change to your working directory
setwd("/Users/Quan/GitHub/nycc-legistar/nycc-meetings")

#############################
# FUNCTIONS
#############################

HTMLtoDF <- function(file){
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

df <- HTMLtoDF("nycc-meetings.html")
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
    filter(!str_detect(Name, ignore.case("Inactive")),
           !str_detect(Name, ignore.case("Subcommittee")),
           !str_detect(Name, ignore.case("Task")),
           !str_detect(Name, ignore.case("Town"))) %>%
    mutate(DateTime = paste(Date,Time),
           # clean "name" variable
           Name = str_replace(Name, "Committee on ", ""),
           Name = str_replace(Name, ",.*$", ""),
           Name = str_replace(Name, "and Solid Waste Management", ""),
           Name = str_replace(Name, "Justice Services", "Justice"),
           # create new columns for month and year with proper format
           Month = factor(format(Date, format = "%b"), levels = month.abb),
           Year = factor(format(Date, format = "%Y")),
           # create new column for Status
           Status = ifelse(Time == "Deferred", "Deferred", "Calendared"))

#############################
# VISUALIZATION
#############################

df_plot %>%
    filter(Year %in% c(2000:2013),
           Status == "Calendared") %>%
    group_by(Month) %>%
    summarise(Count = n()) %>%
    mutate(Proportion = (Count/sum(Count)*100)) %>%
    
    ggplot(aes(x = Month, y = Proportion)) +
    geom_bar(stat = "identity") +
    ylab("Proportion of Meetings Held (%)") +
    scale_y_continuous(breaks = seq(0, 16, 2)) +
    ggtitle("City Council Meetings (2000-2013)") +
    theme_bw()


Committees <- sort(unique(df_plot$Name))
df_plot %>%
    # data 
    filter(Year %in% c(2000:2013),
           Status == "Calendared") %>%
    xtabs(formula = ~Name + Month) %>%
    as.data.frame() %>%
    
    ggvis(x = ~Month, y = ~Freq, fill = ~Name) %>%
    filter(Name %in% eval(input_checkboxgroup(
        choices = Committees,
        selected = c("Aging", "Land Use"),
        label = "Committee Name"))) %>%
    layer_bars()
  