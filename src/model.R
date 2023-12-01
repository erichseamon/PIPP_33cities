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

data2 <- subset(data, city == c("san francisco", "austin", "atlanta"))

data2 %>%
  ggplot( aes(x=time, y=mask_seven2021, group=city, color=city)) +
  geom_line()



x <- rbind(matrix(rbinom(250, 2, 0.25), ncol = 5),
           matrix(rbinom(250, 2, 0.75), ncol = 5))
colnames(x) <- c("a", "b", "c", "d", "e")

data_mean_prep <- data
rownames(data_mean_prep) <- data$city
data_mean <- select(data_mean_prep, contains("mean"))

k2 <- kmeans(data_mean, centers = 5, nstart = 25)

fviz_cluster(k2, data = data$city)





wineUrl <- 'http://archive.ics.uci.edu/ml/machine-learning-databases/wine/wine.data'
wine <- read.table(wineUrl, header=FALSE, sep=',',
                                      stringsAsFactors=FALSE,
                                       col.names=c('Cultivar', 'Alcohol', 'Malic.acid',
                                                                         'Ash', 'Alcalinity.of.ash',
                                                                         'Magnesium', 'Total.phenols',
                                                                          'Flavanoids', 'Nonflavanoid.phenols',
                                                                          'Proanthocyanin', 'Color.intensity',
                                                                         'Hue', 'OD280.OD315.of.diluted.wines',
                                                                         'Proline'
                                                      ))
