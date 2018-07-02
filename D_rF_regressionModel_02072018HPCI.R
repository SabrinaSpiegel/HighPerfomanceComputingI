

#
# file name: D_rF_regressionModel_02072018HPCI
#

# Random Forest ZTDaten plus Data we created


# Note:
# use dataframe "C_cluster1_added.csv" (cluster based on lon+lat)
# or "C_cluster2_added.csv" (cluster based on (lon, lat +preissqm))
# as input siehe : 
# C_cluster_code


library(readr)
library(plyr)
library(rpart)
library(party)
library(caret)
library(randomForest)
library(RANN)

# note that ggplot 2 masks randomForest and kernlab packages!

getwd()
setwd( "C:/Users/SaSp/Desktop")
dir()

data2 <- 
  read.csv("C:/Users/SaSp/Desktop/C_cluster1_added.csv",
                  sep=";")


# Check variable class: 
sapply(data2, class)
str(data2)  

# change these variables into factor variables
data2$GebAlter <- as.factor(data2$GebAlter)
data2$graz_clust <- as.factor(data2$graz_clust)
data2$Postleitzahl <- as.factor(data2$Postleitzahl)
# wenn jahrdatum nicht als factor sondern als integer -->
# we are putting too much rigitity on the connection from 
# one year to the next
data2$jahrdatum <- as.factor(data2$jahrdatum)  

names(data2)
#########################################################################
# VARIABLE selection   -    select variables for model
#########################################################################

# Some variables in data2 have too many categories for Random Forest (max 53): 

library(dplyr)

sapply(data2, nlevels) 
# with Random Forest (max 53 levels)

# original data set (when using extended data set deactive this)

data2 <- subset(data2, c("GesamtPreis", "NutzFl", "longitude", "latitude","Parkplatz", "Keller","Balkon",
                                 "Garten","Postleitzahl","AlterKategorie","logpreissqm", "jahrdatum", "landverkaeufer",
                                 "landkaeufer", "statusverkaeufer", "statuskaufer"))
                 

str(data2)


##########################################






##########################################
# 1. Version:  use only old apartments

# --> neue Bautraeger Wohnungen schlieﬂe ich zur Zeit aus um homogeneren
# Markt zu haben 


summary(data2$Bautraeger) # falsch: 5368, wahr: 527

data2 <- dplyr::filter(data2, trimws(Bautraeger) == "FALSCH")

data2 <- subset(data2, select = -c(Bautraeger))


##############################

# set limits on square meter price
# in A part level is set at 7000 Euros

# set a lower bound of ???1000 per square meter:
data2 <- dplyr::filter(data2, trimws(preissqm) >1000)

######################

# choose whether all cases or only complete cases should be considered 
# and call dataset "data": 

sum(is.na(data2))

data3 <- data2[complete.cases(data2),]
# data3 <- data2     # this line if NAs stay in model

# Check variable class: 
sapply(data3, class)




library(tibble)     
data3 <- as_data_frame(data3)




####################################################################
#  skewness checks etc 
####################################################################

# take only complete cases (although we checked for that higher up)
data4 <- data3[complete.cases(data3),]

sapply(data4, class)

data4$NutzFl_mean <- as.integer(data4$NutzFl_mean)

Column_classes <- sapply(names(data4),function(x){class(data4[[x]])})
numeric_columns <-names(Column_classes[Column_classes != "factor"])

Column_classes
numeric_columns

# determining skew of each numeric variable

library(fBasics)  # for skewness calculation

skew <- sapply(numeric_columns,function(x){skewness(data4[[x]],na.rm = T)})
skew

# plot the skewed parameters:
library(ggplot2)

ggplot(data=data4, aes(nearestNMS_meter))+  geom_density(color="red")+ 
  geom_density(aes(nearestVS_meter), color="blue")+
  geom_density(aes(nearestApotheke_meter))+ 
  geom_density(aes(preissqm), color="green")+
  geom_density(aes(Kindergarten_meter), color="purple")+
  geom_density(aes(Kinderkrippen_meter), color="pink")+
  ggtitle("skewed parameters")+
  labs(x = "nearestNMS_meter (red), nearestVS_meter (blue), 
      nearestSchool_meter,
      Kindergarten_meter (purple), Kinderkrippen_meter (pink),
       nearestApotheke_meter (black), preissqm (green)",
       y = "Density")

# NutzFl and Gesamtpreis and sumnoise have such a high peak,  they 
# don't fit on same plot
ggplot(data=data4, aes(GesamtPreis))+  geom_density()

ggplot(data=data4,  aes(NutzFl))+
       geom_histogram(aes(NutzFl), color ="purple")+ 
  labs(title="NutzFl der verkauften Wohnungen Graz")

# noisemax
ggplot(data=data4,  aes(noisemax))+
  geom_histogram(aes(noisemax), color ="purple")


# Let us determine a threshold skewness and transform all variables above 
# the treshold.

skew <- skew[skew > 0.75]

# transform excessively skewed features with log(x + 1)

for(x in names(skew)) 
{
  data4[[x]] <- log(data4[[x]] + 1)
}

head(data4)

# plot the skew variables after they have been "logged"
# ----------------------------------------------------

library(ggplot2)

ggplot(data=data4, aes(nearestNMS_meter))+
  geom_density(color="red")+ geom_density(aes(nearestVS_meter), color="blue")+
  geom_density(aes(nearestApotheke_meter))+ 
  ggtitle("skewed parameters after taking logs")+
  labs(x = "nearestNMS_meter, nearestVS_meter,
       nearestApotheke_meter", y = "Density")

ggplot(data=data4, aes(GesamtPreis))+
  geom_density()+
  ggtitle("log(GesamtPreis)")

ggplot(data=data4, aes(NutzFl))+
  geom_density(aes(NutzFl), color ="purple")+
  ggtitle("log(NutzFl)")


head(data4)



#####################################################
#  Model A - use logGesamtPreis als y variable  
#           random forest via randomForest 
#####################################################

# restrict price range for squaremeter
#select lower price range for used apartments than for new ones

# data5 <- dplyr::filter(data4, trimws(preissqm) > log(800)) 
# this does not work - excludes all apartments. why???

#data5 <- dplyr::filter(data4, trimws(preissqm) < log(4000)) 
# this excludes about 60 cases

summary(data4)

# exclude variables that are not needed for this model
# ----------------------------------------------------
library(dplyr)
data4 <- as_data_frame(data4)

# Exclude some variables

data5 <- dplyr::select(data4, -c(preissqm, GBName, KATNAM, GebAlter, 
                       longitude, latitude))

str(data5)
# Check variable class: 
sapply(data5, class)




set.seed(1234)
id <- sample(2, nrow(data5), prob=c(0.9,0.1), replace=TRUE)
data_train_a <- data5[id==1,]
data_test_a <- data5[id==2,]

library(tibble)         #  better for large datasets
data_train_a <- as_tibble(data_train_a)  # as_tibble is faster than as_data_frame
data_test_a <- as_tibble(data_test_a)


my_forest_a <- randomForest(GesamtPreis~., data = data_train_a, 
                            ntree = 501, 
                          na.action=na.omit, mtry = 8,
                          importance = TRUE, proximity = TRUE)
print(my_forest_a)  


# predict the test data for the randomForest model oob
# ----------------------------------------------------

predict_my_forest_a <- predict(object = my_forest_a, newdata= data_test_a)  

# create a table "results" where first column shows actual price
# and second column shows predicted price for test data 
# this is still in logs

results_a <- data.frame(actual = round(data_test_a$GesamtPreis, 2),
                      predict_my_forest_a = round(predict_my_forest_a, 2))
#print(results_a)


# get score of importance for each variable:

importance(my_forest_a)  

plot(my_forest_a$importance)

varImpPlot(my_forest_a)

