library(exifr)
library(dplyr)
library(leaflet)
library(ggplot2)
library(plyr)

setwd("C:/Users/Bouts/Desktop/New folder")
files <- list.files("C:/Users/Bouts/Desktop/New folder",recursive = T, pattern = "*.CR2")
dat <- read_exif(files)
years <- c(2014:2020)
years <- c(2020)
for (year in years){
  year <- toString(year)
  dat_filtered <- filter(dat, grepl(year,FileCreateDate))
  foclen <- as.data.frame(table(dat_filtered$FocalLength))
  colnames(foclen) <- c("Focal_length","Amount")
  foclen2 <- data.frame(Focal_length = 18:55, Amount = 0)
  a <- merge(foclen2, foclen, by=c("Focal_length", "Amount"),all=TRUE)
  a <- ddply(a,"Focal_length",numcolwise(sum))
  a$Focal_length<- as.numeric(as.character(a$Focal_length))
  a$Amount<- as.numeric(as.character(a$Amount))
  plt <- ggplot(a, aes(x=Focal_length, y=Amount))+ geom_bar(stat='identity') +
    labs(title=year, x ="Focal length", y = "Amount")
  
  filename1 <- paste0("Focal_length_dist_",year,".png")
  png(filename1, width = 6, height = 6, units = "in", pointsize = 48, res = 300)
  print(plt)
  dev.off()
}
