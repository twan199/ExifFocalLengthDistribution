library(exifr)
library(dplyr)
library(leaflet)
library(ggplot2)
library(plyr)

years <- c(2013,2014,2015,2016,2017,2018,2019,2020) # years of the images
main_df <- data.frame(Focal_length=numeric(), Amount=numeric(), Year=character(), stringsAsFactors=FALSE) # dataframe to merge years

for (year in years){ # loop through years
  year <- toString(year) # select year
  print(year)
  pathwimages <- paste0("E:/Photos/Canon 600D/Canon 600d/",year,"/")# Working directory containing images
  setwd(pathwimages) 
  files <- list.files(pathwimages,recursive = T, pattern = "*.CR2")
  memory.limit(size=56000)
  # Chunking to bypass memory limitations
  # max <- 1
  # x <- seq_along(files)
  # d1 <- split(files, ceiling(x/max))
  # dat <- list()
  # 
  # #count <- 0
  # for (i in d1){
  #   dat <- c(dat,read_exif(i))
  #   gc() 
  #   #count <- count +1
  #   #print(100*count/length(d1))
  # }
  dat <- read_exif(files) #read exif data from images
  setwd("C:/Users/Bouts/Desktop/New folder") 

  dat_filtered <- filter(dat, grepl(year,FileModifyDate)) #filter data on year
  filename <- paste0("data",year,".rds")
  
  foclen <- as.data.frame(table(dat_filtered$FocalLength)) # count frequency focal lengths
  colnames(foclen) <- c("Focal_length","Amount") # change df column names
  foclen2 <- data.frame(Focal_length = 18:55, Amount = 0) # empty df containing all focal lengths, neccesary for plot
  a <- merge(foclen2, foclen, by=c("Focal_length", "Amount"),all=TRUE) # merge both df
  a <- ddply(a,"Focal_length",numcolwise(sum)) # sum merged df double focal length occurances
  a$Focal_length<- as.numeric(as.character(a$Focal_length)) #make column numeric
  a$Amount<- as.numeric(as.character(a$Amount))#make column numeric
  a$Year <- year # add column containing year
  main_df <- rbind(main_df, a) # add df to main df
  # saveRDS(main_df, filename)
  # plt2 <- ggplot(main_df, aes(x=Focal_length, y=Amount))+ geom_bar(stat='identity') +
  #   labs(x ="Focal length", y = "Amount") + xlim(15, 60)
  # filename2 <- paste0(year,"_Focal_length_dist",".png")
  # png(filename2, width = 6, height = 6, units = "in", pointsize = 48, res = 300)
  # print(plt2)
  # dev.off()
}

main_df_1855 <- main_df[main_df$Focal_length < 56, ]
plt <- ggplot(main_df_1855, aes(x=Focal_length, y=Amount))+ geom_bar(stat='identity') +
  labs(x ="Focal length", y = "Amount") + facet_wrap(~Year)  #+ ylim(0, 1000)+ xlim(15, 60)
filename1 <- paste0("1_Focal_length_dist",".png")
png(filename1, width = 6, height = 6, units = "in", pointsize = 48, res = 300)
print(plt)
dev.off()

plt <- ggplot(main_df_1855, aes(x=Focal_length, y=Amount))+ geom_bar(stat='identity', aes(fill = Year)) +
  labs(x ="Focal length", y = "Amount") + theme(legend.key.size = unit(0.5, "cm"))#+ ylim(0, 1000) + xlim(15, 60)
filename1 <- paste0("2_Focal_length_dist",".png")
png(filename1, width = 6, height = 6, units = "in", pointsize = 48, res = 300)
print(plt)
dev.off()

norm_main_df <- data.frame(Focal_length=numeric(), Amount=numeric(), Year=character(), stringsAsFactors=FALSE) # dataframe to merge years
for (year in years){
  norm_main_df1 <- main_df_1855[main_df_1855$Year == year, ]
  maxim <- max(norm_main_df1$Amount)
  norm_main_df1$Amount <- norm_main_df1$Amount/maxim
  norm_main_df <- rbind(norm_main_df, norm_main_df1)
}


plt <- ggplot(norm_main_df, aes(x=Focal_length, y=Amount))+ geom_bar(stat='identity') +
  labs(x ="Focal length", y = "Amount") + facet_wrap(~Year) #+ xlim(15, 60) #+ ylim(0, 1000)
filename1 <- paste0("3_Focal_length_dist",".png")
png(filename1, width = 6, height = 6, units = "in", pointsize = 48, res = 300)
print(plt)
dev.off()


