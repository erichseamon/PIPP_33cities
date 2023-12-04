#add city to each file

# library(data.table)  
# setwd("/mnt/ceph/erichs/git/PIPP_33cities")
# files <- list.files(path = "./data/33cities/", pattern = ".csv")
# cities <- c(files)
# 
# setwd("/mnt/ceph/erichs/git/PIPP_33cities/data/33cities/")
# 
# for (i in cities){
# 
# city <- read.csv(i)
# city$city <- gsub("\\.csv$", "", i)  
# write.csv(city, file = paste("/mnt/ceph/erichs/git/PIPP_33cities/data/33cities/", i, sep=""), row.names = FALSE)
# }

#read csv

library(data.table)  
library(dplyr)
library(ggplot2)
library(factoextra)
setwd("/mnt/ceph/erichs/git/PIPP_33cities")
files <- list.files(path = "./data/33cities/", pattern = ".csv")
setwd("/mnt/ceph/erichs/git/PIPP_33cities/data/33cities/")
temp <- lapply(files, fread, sep=",")
data <- rbindlist( temp )

#reduce cities to three for visualization

data2 <- subset(data, city == c("san francisco", "austin", "atlanta"))

data2 %>%
  ggplot( aes(x=time, y=hospitalizatoin, group=city, color=city)) +
  geom_line()

#select mean values and add city as rowname
data_mean_prep <- data
data_mean <- select(data_mean_prep, contains("mean"))

data_mean$time <- data$time
data_mean$city <- data$city

data_timepoint <- data_mean[time == "2020-02-05",]

k2 <- kmeans(data_timepoint[,1:73], centers = 5, nstart = 25)
d2 <- as.data.frame(data_timepoint)
rownames(d2) <- data_timepoint$city

fviz_cluster(k2, data=d2[,1:73])

library(zoo)
data_atlanta <- data_mean[city == "atlanta",]
data_atlanta$time <- as.Date(data_atlanta$time)
data_atlanta_ts <- read.zoo(data_atlanta)


test <- loess(1:nrow(data_atlanta)~X..Analytic....mean.., data_atlanta)
