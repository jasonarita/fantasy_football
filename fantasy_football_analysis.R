# install.packages("XML")
# install.packages("RSQLite")
# install.packages("stringr")
# install.packages("ggplot2")


library(XML)      # Tools to Read HTML data from web pages 
library(RSQLite)  # Data manipulation tools
library(stringr)  # Data manipulation tools
library(ggplot2)  # Plotting graphs/visualization tools

# http://www.nfl.com/rulebook/beginnersguidetofootball


# Scrape Offense Data -----------------------------------------------------
year <- 2014
url <- paste0("http://sports.yahoo.com/nfl/stats/byteam?group=Offense&cat=Total&conference=NFL&year=season_" 
             , year 
             , "&sort=530&old_category=Total&old_group=Offense")
            
offense <- readHTMLTable(url, encoding = "UTF-8", colClasses = "character")[[7]]




# Clean Offense data ------------------------------------------------------

# Delete blank columns from raw dataset
offense <- offense[,-c(2,4,6,8,10,12,14,16,18,20,22,24,26,28)]

# Force all team names into char strings
offense[,1] <- as.character(offense[,1])

# Convert all stats to numerics
offense[,2:13] <- apply(offense[,2:13], 2, as.numeric)

# Convert time of posession (TOP) to seconds per game
offense[,14] <- as.numeric(substr(offense[,14], 1, 2))*60 
                + as.numeric(substr(offense[,14], 4, 6))


# Scrape Defense data -----------------------------------------------------
url <- paste0("http://sports.yahoo.com/nfl/stats/byteam?group=Defense&cat=Total&conference=NFL&year=season_" 
              , year 
              , "&sort=530&old_category=Total&old_group=Defense")

defense <- readHTMLTable(url, encoding = "UTF-8", colClasses = "character")[[7]]



# Clean Defense data ------------------------------------------------------

# Delete blank columns from raw dataset
defense <- defense[,-seq(2,length(defense), 2)]


# Force all team names into char strings
defense[,1] <- as.character(defense[,1])

# Convert all stats to numerics
defense[,2:13] <- apply(defense[,2:length(defense)], 2, as.numeric)


