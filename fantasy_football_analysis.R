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
     , breaks = 10
     , main = "Offensive Points Per Game"
     , xlab = "Offensive PPG"
     , ylab = "Number of Teams" 
     , col = "blue")

# GGPLOT Version
# ggplot(combined_df, aes(x=OffPPG)) +
#   geom_histogram(aes(y=..density..), binwidth=2, color = "black", fill = "blue") +
#   geom_density()


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
max(combined_df$DefPPG)
min(combined_df$DefPPG)

# Offensive first downs per game
hist(combined_df$`1stD/G`
, breaks = 10
, main = "Offensive 1st downs Per Game"
, xlab = "1st Downs Per Game"
, ylab = "Number of Teams"
, col = "green")
rug(jitter(combined_df$`1stD/G`, amount = 0.01)
    , col = "green"
    , lwd = 2.0)




# Bar Charts: How different teams compare ---------------------------------

ppg <- transform(combined_df, Team=reorder(Team, combined_df$OffPPG))
ggplot(ppg, aes(x=Team, y=OffPPG)) + 
  geom_bar(stat="identity", color="black", fill="blue") + 
  coord_flip() + 
  labs(x="Team", y="Avg Points Per Game") + 
  ggtitle("Avg Points Per Game") + 
  theme(plot.title=element_text(size=18, face="bold"))


maxYards = 500

# Defensive YPG
defYPG <- transform(combined_df, Team=reorder(Team, combined_df$DefYPG))
ggplot(defYPG, aes(x=Team, y=DefYPG)) + 
  geom_bar(stat="identity", color="black", fill="red") + 
  ylim(0, maxYards) +
  coord_flip() +
  labs(x="Team", y="Avg Yards Allowed Per Game") +
  ggtitle(paste(year, "- Avg yards ALLOWED Per Game")) +
  theme(plot.title=element_text(size=18, face="bold"))
  
# Defensive Pass YPG
defPassYPG <- transform(combined_df, Team=reorder(Team, combined_df$DefPassYPG))

ggplot(defPassYPG, aes(x=Team, y=DefPassYPG)) +
  geom_bar(stat="identity", color="black", fill="firebrick4") +
  ylim(0, maxYards) +
  coord_flip() +
  labs(x="Team", y="Avg Pass Yards Allowed Per Game") +
  ggtitle(paste(year, "- Avg Pass Yards Allowed Per Game")) +
  theme(plot.title=element_text(size=18, face="bold"))




# Scatterplots: Relationships/correlations between variables --------------


# Offensive YPG vs PPG
ggplot(combined_df, aes(x=combined_df$OffYPG, y=combined_df$OffPPG, label=combined_df$Team)) +
  geom_point(shape=5, size=2, color="blue") +
  geom_text(aes(label=Team), hjust=1.10, vjust=0.45) +
  geom_smooth() +
  labs(x="Yards Per Game", y="Points Scored Per Game") +
  ggtitle("Offensive Yards per Game vs Points per Game") +
  theme(plot.title=element_text(size=18, face="bold"))

cor(combined_df$OffPPG, combined_df$OffYPG)

# Defensive YPG vs PPG
ggplot(combined_df, aes(x=combined_df$DefYPG, y=combined_df$DefPPG, label=combined_df$Team)) +
  geom_point(shape=5, size=2, color="firebrick1") +
  geom_text(aes(label=Team), hjust=.50, vjust=-1.45) +
  geom_smooth() +
  labs(x="Yards Per Game", y="Points Allowed Per Game") +
  ggtitle("Defensive Yards per Game vs Points per Game") +
  theme(plot.title=element_text(size=18, face="bold"))

cor(combined_df$DefPPG, combined_df$DefYPG)

# Time of Posession (TOP) vs PPG
ggplot(combined_df, aes(x=combined_df$TOP, y=combined_df$OffPPG, label=combined_df$Team)) +
  geom_point(shape=5, size=2, color="magenta") +
  geom_text(aes(label=Team), hjust=.50, vjust=-1.45) +
  geom_smooth() +
  labs(x="Time of Possession (Seconds)", y="Points Scored Per Game") +
  ggtitle("Time of Possession vs Points per Game") +
  theme(plot.title=element_text(size=18, face="bold"))

cor(combined_df$TOP, combined_df$OffPPG)




# Indexes -----------------------------------------------------------------

combined_df$OffPassStrength <-  max(combined_df$OffPassYPG) - combined_df$OffYPG
combined_df$OffPassStrength <- (1 - (combined_df$OffPassStrength/max(combined_df$OffPassStrength))) * 100

# Z-score
combined_df$OffPassStrength <- combined_df$OffYPG - mean(combined_df$OffPassYPG)
combined_df$OffPassStrength <- combined_df$OffPassStrength/sd(combined_df$OffPassYPG)

hist(x=combined_df$OffPassStrength)
