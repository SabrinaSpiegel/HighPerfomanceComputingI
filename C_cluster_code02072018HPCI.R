

# datei: C_cluster_code02072018HPCI



library(readr)
library(ggmap)
library(plyr)
library(dplyr)
library(ggplot2)
library(AppliedPredictiveModeling)
library(tree)
library(RgoogleMaps)
library(tibble)    # data_frame
library(cluster)  # for daisy cluster

getwd()
setwd( "C:/Users/SaSp/Desktop")
dir()

data1a <- read.csv("B_addedData.csv", sep=",")

summary(data1a)

#   this code contains the following: 

#   calculate clusters 

#    1: only long and lat matter
#    2: long, lat, and preissqm





# 1. Teil: nur longitutde und latitude zählen für die clusterbildung
####################################################################

# save only long and lat of data1a in separate data_frame

lat_1<- data1a$latitude
lon_1<- data1a$longitude

data_new=data_frame(lat_1,lon_1)  

# plot data_new
qplot(data=data_new, y=lat_1, x=lon_1, 
      main="longitude and latitude of sold apartments Graz")


#Create distance matrix
#---------------------------------------------------------------------------
d= as.matrix(dist(cbind(data_new$lon_1,data_new$lat_1))) 
d=ifelse(d<5,d,0) #keep only distance < 5
d=as.dist(d)
hc <- hclust(dist(data_new)) # hierarchical clustering
clust <- cutree(hc, 20)
plot(hc)
graz_clust <- cutree(hc,k=20) # cut the dendrogram to generate 20 clusters


# Plot clusters


Graz_cluster <- ggplot(data=data1a)+
  geom_point(data=data_new, aes(x=lon_1, y=lat_1),
             col=c(1:20)[graz_clust],pch=19)+ 
  ggtitle("Graz: 20 price clusters for apartments")
Graz_cluster+labs(title = "Graz: 20 price clusters for apartments",  x = "lon", y = "lat")


# save clustermembership for each apartment
# -----------------------------------------

data1b <- cbind(data1a, graz_clust)

data1b <- as_data_frame(data1b)   # as_tibble is faster than as_data_frame

names(data1b)



# calculate mean m^2 in cluster and save as extra variable
##########################################################

library(dplyr)

data1c <- arrange(data1b, graz_clust)     # arrange accord to clusters


# group by clustermembership, then calculate mean m^2 for apartments
# in each cluster, then save as NutzFl_mean

data1c <- data1c %>%
  group_by(graz_clust) %>%
  mutate(NutzFl_mean = mean(NutzFl, na.rm=TRUE))

  
data1c <-  as_data_frame(data1c)


str(data1c)


##################
##################
###################


# 2. Teil: preissqm + longitude + latitude zählen für clusterbildung
####################################################################

summary(data1a)



# save long and lat and preissqm from data1a in 
# separate data_frame

lat<- data1a$latitude
lon<- data1a$longitude
preissqm <- data1a$preissqm

library(tibble)
data_new=data_frame(lat,lon, preissqm)
data_new <- as_data_frame(data_new)    #better for large datasets


# sample of data bc code is too slow (max I could do is 3000)
# -----------------------------------------------------------
data_sample <- sample_n(data_new, size=500) 


#plot data_new

Plot_preissqm <- 
  ggplot(data_new, aes(x= lon, y=lat, color=preissqm))+
  geom_point()+
  scale_color_gradient(low= "yellow", high="black")+
  ggtitle("Price per square meter for apartments in Graz (new and used)")
Plot_preissqm 



# create distance function for my data_set
# -------------------------------------------

# daisy computes dissimilarity matrix for more than 2 dimensions
# metric = gower function (can handle mixed factor and numerical input)
# note this is very time intensive (only do for sample)
# weights give relative importance of inputs ind distance meassure
# (lon, lat, preissqm)

library(cluster) # for daisy cluster 

daisy1 <- daisy(data_sample, metric = c("gower"),
                stand = FALSE, type = list(), weights=c(4,4,1))

summary(daisy1)



#Create distance matrix
#---------------------------------------------------------------------------
d= as.matrix(dist(daisy1))
d=ifelse(d< 0.001,d,0) #keep only distance  d < x 
d=as.dist(d)
hc <- hclust(dist(daisy1)) # hierarchical clustering

# cut the dendrogram to generate k=x clusters
graz_clust_10 <- cutree(hc,k=10) 
graz_clust_20 <- cutree(hc,k=20)
graz_clust_30 <- cutree(hc,k=30)

plot(hc)          # plots dendrogram


# Plot clusters
# --------------

Graz_cluster_20 <- ggplot(data=data_sample)+
  geom_point(data=data_sample, aes(x=lon, y=lat),
             col=c(1:100)[graz_clust_20],pch=19)+ 
  ggtitle("Graz: 20 price clusters for apartments, 
          based on long, lat and preissqm (weights 4:1)")
Graz_cluster_20



Graz_cluster_30 <- ggplot(data=data_sample)+
  geom_point(data=data_sample, aes(x=lon, y=lat),
             col=c(1:30)[graz_clust_30],pch=19)+ 
  ggtitle("Graz: 30 price clusters for apartments, 
          based on long, lat and preissqm (weights 4:1)")
Graz_cluster_30


Graz_cluster_10 <- ggplot(data=data_sample)+
  geom_point(data=data_sample, aes(x=lon, y=lat),
             col=c(1:10)[graz_clust_10],pch=19)+ 
  ggtitle("Graz: 10 price clusters for apartments, 
          based on long, lat and preissqm (weights 4:1)")
Graz_cluster_10


# save clustermembership for each apartment
# -----------------------------------------

#graz_clust_10  
#graz_clust_20 
#graz_clust_30 


graz_clust_30


# keep the membership to the 30 clusters as variable in dataset: 

data1d <- cbind(data1a, graz_clust_30)

names(data1d)



###########################################################################
# save this dataset as a dataset in "C:/Users/SaSp/Desktop"
###########################################################################


# 1. Teil: cluster membership based on only lon + lat: 

write.csv(data1c, 
          file = "C:/Users/SaSp/Desktop/C_cluster1_added.csv",
          row.names = FALSE)

# 2. Teil: cluster membership basked on lon + lat + preissqm

write.csv(data1d, 
          file = "C:/Users/SaSp/Desktop/C_cluster2_added.csv",
          row.names = FALSE)



















############################


  
