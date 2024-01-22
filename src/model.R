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
library(dbnR)
library(caret)
library(zoo)
library(pamr)
library(animation)
library(FeatureImpCluster)
library(flexclust)
library(gridExtra)
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

cs_data <- NULL
for (i in as.list(daterangez)) {
  data_timepoint <- cityfinal[cityfinal$time == i,]
  
#data_timepoint <- cityfinal[cityfinal$time == "2020-03-01",]

cluster_wss <- fviz_nbclust(data_timepoint[,1:4], kmeans, method = "wss")
cluster_sil <- fviz_nbclust(data_timepoint[,1:4], kmeans, method = "silhouette")

#cs_data <- c(which.max(cluster_sil$data$y), cs_data)

clusternumber_sil <- cluster_sil$data[which.max(cluster_sil$data$y), ]$clusters

if (as.numeric(clusternumber_sil) > 3) {
  clusternumber_sil = 3
} else {
  
}

k2 <- kmeans(na.omit(data_timepoint[,1:4]), centers = as.numeric(clusternumber_sil), nstart = 25)
h2 <- hcut(na.omit(data_timepoint[,1:4]), hc_method = "complete")

d2 <- as.data.frame(data_timepoint)
rownames(d2) <- data_timepoint$city


#options(repr.plot.width = 5, repr.plot.height =2) 
map1 <- fviz_cluster(k2, data=d2[,1:4], ellipse.type = "norm", show.clust.cent = FALSE,ellipse.alpha = 0 ) +
  scale_shape_manual(values=c(3,17,19, 8, 7)) 

# Visualize dendrogram
h2$labels <- data_timepoint$city
map2 <- fviz_dend(h2, show_labels = TRUE, rect = TRUE)

g <- grid.arrange(map1, map2, ncol=2, nrow=1)
ggsave(paste("./maps/", i, ".jpg", sep=""), g)

}

#system("convert -delay 40 *.jpg cluster_animate.gif")

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


#---dbnr run


library(dbnR)

size <- 3

split_rows = createDataPartition(data_atlanta$X..hospital....mean.., p = 0.8, list = FALSE, times = 1)
dt_train = data_atlanta[split_rows,] 
dt_test = data_atlanta[-split_rows,] 

size <- 10

net <- learn_dbn_struc(dt_train[,1:73], size)


f_dt_train <- fold_dt(dt_train[,1:73], size)
f_dt_val <- fold_dt(dt_test[,1:73], size)
fit <- dbnR::fit_dbn_params(net, f_dt_train, method = "mle-g")

dbnR::plot_dynamic_network(fit)



#dbnR example

library(dbnR)
library(data.table)
data(motor)
summary(motor)

size = 10
dt_train <- motor[1:2800]
dt_val <- motor[2801:3000]
blacklist <- c("motor_speed_t_0", "i_d_t_0")
blacklist <- rbind(blacklist, c("motor_speed_t_0", "i_q_t_0"))
blacklist_tr <- c("stator_tooth_t_1", "coolant_t_0")
blacklist_tr <- rbind(blacklist_tr, c("stator_tooth_t_2", "coolant_t_1"))
whitelist_tr <- c("coolant_t_2", "stator_yoke_t_0")
net <- dbnR::learn_dbn_struc(dt_train, size, method = "dmmhc", blacklist = blacklist,
                             blacklist_tr = blacklist_tr,
                             restrict = "mmpc", maximize = "hc",
                             restrict.args = list(test = "cor"),
                             maximize.args = list(score = "bic-g", maxp = 10))


f_dt_train <- fold_dt(dt_train, size)
f_dt_val <- fold_dt(dt_val, size)
fit <- dbnR::fit_dbn_params(net, f_dt_train, method = "mle-g")

dbnR::plot_dynamic_network(fit)


#multilevel test

#set filepath for data file
filepath <- "https://quantdev.ssri.psu.edu/sites/qdev/files/AMIBshare_daily_2019_0501.csv"

#read in the .csv file using the url() function
AMIB_daily <- read.csv(file=url(filepath),header=TRUE)

#subsetting to variables of interest
AMIB_daily <- AMIB_daily[ ,c("id","day","negaff","pss")]
