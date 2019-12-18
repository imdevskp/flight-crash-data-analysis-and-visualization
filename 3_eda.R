#------------------------------------------------------------
# Importing the datasets

library(plotly)
library(ggplot2)
library(wesanderson)
pal <- wes_palette("Zissou1", 100, type = "continuous")

#------------------------------------------------------------
# Working directory

getwd()
# setwd('../../imdevskp/Desktop/crashes/')

#----------------------------------------------------------------------------
# Read dataset

df<-read.csv('crashes_5600_1908_2009.csv')
print(head(df,4))

#----------------------------------------------------------------------------
# Viewing the data

print(colnames(df))
print(dim(df))
print(head(df,4))
str(df)

#-----------------------------------------------------------------------------
# Preprocessing

df$Date<-as.Date(df$Date,format="%m/%d/%Y")
df$Year<-format(as.Date(df$Date, format="%m-%d-%Y"),"%Y")
df$Month<-format(as.Date(df$Date, format="%m-%d-%Y"),"%m")
df$Day<-format(as.Date(df$Date, format="%m-%d-%Y"),"%d")
df$Weekday<-format(as.Date(df$Date, format="%m-%d-%Y"),"%A")

df$Location <- strsplit(as.character(df$Location), ", ")
df$Country <- sapply(df$Location,function(s) as.character(as.vector(s)[length(s)]))
df$Country <- df['Country']

df$Aboard <- as.numeric(df$Aboard)
df$Ground <- as.numeric(df$Ground)
df$Fatalities <- as.numeric(df$Fatalities)

#--------------------------------------------------------
# Percentage of People aboard died in crashes

no_of_aboard<-sum(df$Aboard, na.rm = T)
no_of_fatalities<-sum(df$Fatalities, na.rm = T)
perc_death<-(no_of_fatalities/no_of_aboard)*100
perc_death

#--------------------------------------------------------------------------
# Is friday the 13th just a myth ?

f13 <- subset(df, Day == 13)
f13 <- subset(f13, Weekday == 'Friday')

no_of_aboard<-sum(f13$Aboard, na.rm = T)
no_of_fatalities<-sum(f13$Fatalities, na.rm = T)
perc_death<-(no_of_fatalities/no_of_aboard)*100
perc_death

#--------------------------------------------------------------------------
# Percentage of crashes that resulted in death of people on ground

mean(df$Ground>10, na.rm = T)*100

#---------------------------------------------------------------------------

# crash that resulted in death of most number of people on ground
df[which.max(df$Ground),]
df[which.max(df$Ground),]['Ground']

# crash that with most fatalities
df[which.max(df$Fatalities),]
df[which.max(df$Fatalities),]['Fatalities']

# how many times did all the people on the flight died
sum(df$Aboard==df$Fatalities, na.rm = T)

# how many times did all the people on the plane survived
sum(df$Fatalities==0, na.rm = T)

# how many fatalities during training, testing, and sight seeing
print(sum(df$Route=='Training', na.rm = T))
print(sum(df$Route=='Test flight', na.rm = T))
print(sum(df$Route=='Sightseeing', na.rm = T))

#-----------------------------------------------------------------------------
