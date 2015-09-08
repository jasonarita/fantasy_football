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
url     <- paste0("http://sports.yahoo.com/nfl/stats/byteam?group=Defense&cat=Total&conference=NFL&year=season_" 
              , year 
              , "&sort=530&old_category=Total&old_group=Defense")
defense <- readHTMLTable(url, encoding = "UTF-8", colClasses = "character")[[7]]


# Clean Defense data ------------------------------------------------------
defense           <- defense[,-seq(2,nVars, 2)]
nVars             <- length(defense)
defense[,1]       <- as.character(defense[,1])
defense[,2:nVars] <- apply(defense[,2:nVars], 2, as.numeric)



# Merge the Offense and Defense Data --------------------------------------

combined_df <- merge(offense, defense, by.x = "Team", by.y = "Team")

# Clean data --------------------------------------------------------------

colnames(combined_df)[3]  <- "OffPPG"
colnames(combined_df)[2]  <- "Games"
colnames(combined_df)[3]  <- "OffPPG"
colnames(combined_df)[4]  <- "OffYPG"
colnames(combined_df)[5]  <- "OffPassYPG"
colnames(combined_df)[6]  <- "OffRushYPG"
combined_df$G.y           <- NULL
colnames(combined_df)[15] <- "DefPPG"
colnames(combined_df)[16] <- "DefYPG"
colnames(combined_df)[17] <- "DefRushYPG"
colnames(combined_df)[18] <- "DefPassYPG"



# Data exploration: Histograms --------------------------------------------

hist(combined_df$OffPPG
     , breaks = 50
     , main = "Offensive Points Per Game"
     , xlab = "Offensive PPG"
     , ylab = "Number of Teams" 
     , col = "blue")

mean(combined_df$OffPPG)
sd(combined_df$OffPPG)
max(combined_df$OffPPG)
min(combined_df$OffPPG)

# Defensive Points Per Game
hist(combined_df$DefPPG
, breaks = 10
, main = "Defensive Points Per Game"
, xlab = "Defensive PPG"
, ylab = "Number of Teams"
, col = "red")

mean(combined_df$DefPPG)
sd(combined_df$DefPPG)

# Offensive first downs per game
hist(combined_df$`1stD/G`
, breaks = 10
, main = "Offensive 1st downs Per Game"
, xlab = "1st Downs Per Game"
, ylab = "Number of Teams"
, col = "green")
)