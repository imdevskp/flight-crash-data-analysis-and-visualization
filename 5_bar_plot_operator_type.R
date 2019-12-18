#------------------------------------------------------------
# Importing the datasets

library(ggplot2)
library(wesanderson)
pal <- wes_palette("Zissou1", 100, type = "continuous")

#------------------------------------------------------------
# Working directory

getwd()
setwd()

#----------------------------------------------------------------------------
# Read dataset

df<-read.csv('../3_flight_crashes/data/crashes_5600_1908_2009.csv')
print(head(df,4))

#----------------------------------------------------------------------------
# Viewing the data

df<-subset(df,select = -Summary)
print(colnames(df))
print(dim(df))
print(head(df,4))
str(df)

#------------------------------------------------------------------------

operator<-data.frame(table(df$Operator))
op<-operator[order(-operator$Freq),]

op<-head(op,10)
ggplot(op,aes(reorder(Var1,Freq),Freq))+
  geom_col(aes(fill=Freq))+
  coord_flip()+
  scale_fill_gradientn(colours = pal)

#------------------------------------------------------------------------------

typ<-data.frame(table(df$Type))
typ<-type[order(-typ$Freq),]

typ<-head(typ,10)
ggplot(typ,aes(reorder(Var1,Freq),Freq))+
  geom_col(aes(fill=Freq))+
  coord_flip()+
  scale_fill_gradientn(colours = pal)

#---------------------------------------------------------------------------
