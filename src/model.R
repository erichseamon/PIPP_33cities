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
#library(dbnR)
library(caret)
library(zoo)
library(pamr)
library(animation)
library(FeatureImpCluster)
library(flexclust)
library(gridExtra)
library(sf)
library(RColorBrewer)
setwd("/mnt/ceph/erichs/git/PIPP_33cities")
files <- list.files(path = "./data/33cities/", pattern = ".csv")
setwd("/mnt/ceph/erichs/git/PIPP_33cities/data/33cities/")
temp <- lapply(files, fread, sep=",")
data <- rbindlist( temp )

#reduce cities to three for visualization

data2 <- subset(data, city == c("san francisco", "austin", "atlanta"))
data_aggregate <- aggregate(data$hospitalizatoin, list(data$time), FUN=sum)
colnames(data_aggregate) <- c("time", "hospitalizations")
data_aggregate %>%
  ggplot( aes(x=time, y=hospitalizations)) +
  geom_line() + theme_bw()


#select mean values and add city as rowname
data_mean_prep <- data
data_mean <- select(data_mean_prep, contains("mean"))

data_mean$time <- data$time
data_mean$city <- data$city

#data_timepoint <- data_mean[time == "2020-02-05",]

#rollingmeanDF <- data.frame(group = c(1, 1, 1, 2, 2, 2), value1 = 1:6, value2 = 7:12)

citynames <- unique(data_mean$city)

cityfinal <- NULL
for (i in citynames) {
 
  cityone <- data_mean[data_mean$city == i,] 
  cityone_mean <- rollapply(cityone[,1:73], 14, mean, na.rm = TRUE, fill = NA)
  
  cityfinal <- rbind(cityfinal, cityone_mean)
}

cityfinal <- cbind(cityfinal, data_mean[,74:75])

#data_timepoint <- cityfinal[cityfinal$time == "2020-11-07" | cityfinal$time == "2020-11-08"| cityfinal$time == "2020-11-09"| cityfinal$time == "2020-11-10"| cityfinal$time == "2020-11-11",]

daterangez <- unique(cityfinal$time)
daterangez <- daterangez[15:length(daterangez)-7]

setwd("/mnt/ceph/erichs/git/PIPP_33cities/")

#daterangez <- daterangez[1]
for (i in as.list(daterangez)) {
  cs_data <- NULL
  clusterframe <- NULL
  data_timepoint <- cityfinal[cityfinal$time == i,]
  data_aggregate2 <- data_aggregate[data_aggregate$time == i,]
  
#data_timepoint <- cityfinal[cityfinal$time == "2020-03-01",]

cluster_wss <- fviz_nbclust(data_timepoint[,1:4], kmeans, method = "wss")
cluster_sil <- fviz_nbclust(data_timepoint[,1:4], kmeans, method = "silhouette")

#cs_data <- c(which.max(cluster_sil$data$y), cs_data)

clusternumber_sil <- cluster_sil$data[which.max(cluster_sil$data$y), ]$clusters

if (as.numeric(clusternumber_sil) >= 3) {
  clusternumber_sil = 3
  k_colorz_dend = c("#00BFC4","#F8766D",  "#00BA38")
  k_colorz_clust = c("#F8766D", "#00BFC4", "#00BA38")
} else {
  k_colorz = c("#00BFC4", "#F8766D")
}

k_colorz_dend = c("#00BFC4","#F8766D")
k_colorz_clust = c("#00BFC4","#F8766D")
k_colorz = c("#00BFC4","#F8766D")

k2 <- kmeans(na.omit(data_timepoint[,1:4]), centers = 2, nstart = 25)
h2 <- hcut(na.omit(data_timepoint[,1:4]), k = 2, hc_method = "complete")

#k2 <- kmeans(na.omit(data_timepoint[,1:4]), centers = as.numeric(clusternumber_sil), nstart = 25)
#h2 <- hcut(na.omit(data_timepoint[,1:4]), k = as.numeric(clusternumber_sil), hc_method = "complete")

d2 <- as.data.frame(data_timepoint)
rownames(d2) <- data_timepoint$city
d2$city <- c("Atlanta", "Austin", "Baltimore", "Boston", "Chicago", "Dallas", "Denver", "Detroit", "Houston", "Kansas City", "Las Vegas", "Los Angeles",
"Minneapolis", "Nashville", "New Orleans", "Orlando", "Philadelphia", "Phoenix", "Raleigh", "Salt Lake City", "San Diego", "San Francisco", "Seattle", 
"Saint Louis", "Saint Paul", "Tampa", "Washington D.C.")

#clusterframe <- rbind(clusterframe, cbind.data.frame(d2$time, d2$city, k2$cluster, h2$cluster[1]))
clusterframe <- cbind.data.frame(d2$time, d2$city, k2$cluster, h2$cluster[1])



colnames(clusterframe) <- c("date", "NAME", "kcluster", "hcluster")

first_cluster_count <- nrow(clusterframe[clusterframe$kcluster == 1,])
second_cluster_count <- nrow(clusterframe[clusterframe$kcluster == 2,])

if (first_cluster_count > second_cluster_count) {
  
} else {
  clusterframe[clusterframe$kcluster == 1,]$kcluster = 3
  clusterframe[clusterframe$kcluster == 2,]$kcluster = 4
  clusterframe[clusterframe$kcluster == 3,]$kcluster = 2
  clusterframe[clusterframe$kcluster == 4,]$kcluster = 1
}


first_cluster_count <- length(k2[k2$cluster == 2,])
second_cluster_count <- length(k2[k2$cluster == 2,])

if (first_cluster_count > second_cluster_count) {
  
} else {
  k2[k2$cluster == 1,]$cluster = 3
  k2[k2$cluster == 2,]$cluster = 4
  k2[k2$cluster == 3,]$cluster = 2
  k2[k2$cluster == 4,]$cluster = 1
}




#options(repr.plot.width = 5, repr.plot.height =2) 
map1 <- fviz_cluster(k2, data=d2[,1:4], ellipse.type = "norm", show.clust.cent = FALSE, ellipse.alpha = 0, labelsize = 5) +
  scale_shape_manual(values=c(3,17,19, 8, 7)) + theme(legend.position = "none") + theme(panel.background = element_blank()) + scale_colour_manual(values = k_colorz_clust)

# Visualize dendrogram
h2$labels <- data_timepoint$city
map2 <- fviz_dend(h2, show_labels = TRUE, rect = TRUE, cex = .5, k_colors = k_colorz_dend)


covid1 <- ggplot( data=data_aggregate, aes(x=time, y=hospitalizations)) + geom_line() +geom_point(data=data_aggregate2, aes(x=time, y=hospitalizations), color="red")

  
#US map

  states <- st_read(dsn = "./shapefiles/states.shp")
  states <- subset(states, STATE_NAME != "Alaska")
  states <- subset(states, STATE_NAME != "Hawaii")
  states <- subset(states, STATE_NAME != "District of Columbia")
  
  cities <- st_read(dsn = "./shapefiles/cities_PIPP2.shp")
  
  clusterframe_cities <- merge(cities, clusterframe, by="NAME", duplicateGeoms = TRUE)
  
  
  library(viridis)
  my_breaks <- c(0, .002, .004, .006, .008, .010, .012, .014, .016)
  p <- ggplot(states) + geom_sf() + geom_point(data=clusterframe_cities, aes(geometry = geometry, colour=factor(clusterframe_cities$kcluster)), stat = "sf_coordinates") + theme(panel.background = element_blank()) + theme(legend.position = "none") +  scale_colour_manual(values = k_colorz_clust)
  
  g <- arrangeGrob(arrangeGrob(p, covid1, nrow=1), arrangeGrob(map1,map2, nrow=1), nrow=2, heights = c(2,2)) 
  
  #g <- grid.arrange(p, arrangeGrob(map1, map2), nrow = 2)

#g <- grid.arrange(map1, map2, p, ncol=2, nrow=2)

  ggsave(paste("./maps/", i, ".jpg", sep=""), g)

}

setwd("/mnt/ceph/erichs/git/PIPP_33cities/maps/")
system("convert -delay 40 *.jpg cluster_animate.gif")

#kmeans feature importance

set.seed(10)
res <- kcca(dat$data,k=4)
set.seed(10)
FeatureImp_res <- FeatureImpCluster(res,as.data.table(dat$data))
plot(FeatureImp_res)

library(zoo)
data_atlanta <- data_mean[city == "atlanta",]
data_atlanta$time <- as.Date(data_atlanta$time)
data_atlanta_ts <- read.zoo(data_atlanta)


test <- loess(1:nrow(data_atlanta)~X..Analytic....mean.., data_atlanta)


