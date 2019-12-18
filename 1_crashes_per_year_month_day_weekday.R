#------------------------------------------------------------
# Importing the datasets

library(ggplot2)
library(wesanderson)
pal <- wes_palette("Zissou1", 100, type = "continuous")

#------------------------------------------------------------
# Working directory

getwd()
# setwd()

#----------------------------------------------------------------------------
# Read dataset

df<-read.csv('crashes_5600_1908_2009.csv')
print(head(df,4))

#----------------------------------------------------------------------------
# Viewing the data

df<-subset(df,select = -Summary)
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

#-----------------------------------------------------------------------------
# Fatalities vs Year, Month, Day, Weekday

ggplot(df,aes(Date))+
  geom_histogram()+
  labs(title='Year vs No. of Fatalities')

ggplot(df,aes(Month,Fatalities))+
  geom_col()+
  geom_smooth()+
  labs(title='Month vs No. of Fatalities')

ggplot(df,aes(Day,Fatalities))+
  geom_col()+
  geom_smooth()+
  labs(title='Day vs No. of Fatalities')

ggplot(df,aes(Weekday,Fatalities))+
  geom_col()+
  geom_smooth()+
  labs(title='Weekday vs No. of Fatalities')

#----------------------------------------------------------------------------
