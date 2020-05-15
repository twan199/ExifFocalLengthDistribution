library(exifr)
library(dplyr)
library(leaflet)
library(ggplot2)
library(plyr)

setwd("C:/Users/Bouts/Desktop/New folder") 
year <- 2014
dat_filtered <- readRDS("data2014.rds") #filter data on year
main_df <- data.frame(Focal_length=numeric(), Amount=numeric(), Year=character(), stringsAsFactors=FALSE) # dataframe to merge years

foclen <- as.data.frame(table(dat_filtered$FocalLength)) # count frequency focal lengths
colnames(foclen) <- c("Focal_length","Amount") # change df column names
foclen2 <- data.frame(Focal_length = 18:55, Amount = 0) # empty df containing all focal lengths, neccesary for plot
a <- merge(foclen2, foclen, by=c("Focal_length", "Amount"),all=TRUE) # merge both df
a <- ddply(a,"Focal_length",numcolwise(sum)) # sum merged df double focal length occurances
a$Focal_length<- as.numeric(as.character(a$Focal_length)) #make column numeric
a$Amount<- as.numeric(as.character(a$Amount))#make column numeric
a$Year <- year # add column containing year
main_df <- rbind(main_df, a) # add df to main df
saveRDS(main_df,"main_df14.rds")