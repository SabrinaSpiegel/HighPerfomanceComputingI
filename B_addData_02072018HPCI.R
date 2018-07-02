
# dateiname: B_addData_02072018HPCI

#####################################################
#                                                   #
# include Pflichtschulen, höhere Schulen, Apotheken
# Enfernung zum Stadtzentrum, Kindergärten,
# Kinderkrippen, ParkandRIde-stellen
# in dataset and calculate distance function        #
#                                                   #
#####################################################

# Date: 02072018


library(readr)
library(plyr)
library(dplyr)
library(RANN)
library(ggplot2)
library(ggmap)
library(tibble)  # for data_frame
library(geosphere)


getwd()
setwd( "C:/Users/SaSp/Desktop")
dir()


data1a <- read.csv("A_cleaned_data.csv", sep=",")

head(data1a)


# Check variable class: 
sapply(data1a, class)

data1a$Postleitzahl <- as.factor(data1a$Postleitzahl)
#data1a$jahrdatum <- as.factor(data1a$jahrdatum)
data1a$GebAlter <- as.factor(data1a$GebAlter)




#################################################################
# create new dataset with only following variables - 
# subset data1a
#################################################################

# Convert to a tibble (better for large data sets)
library(tibble)
data1b <- as_data_frame(data1a)
# Print
head(data1b)


#################################################################

# Data von Graz: Pflichtschulstandorte
# ------------------------------------------------------------
data_Pflichtschulen <- read.csv("Pflichtschulen_PUNKT.csv", sep=";")

summary(data_Pflichtschulen)
str(data_Pflichtschulen)

# KAT2 tells us that they are "Pflichtschulen"
# KAT3 tells us whether they are VS, Hauptschulen, 
# neue Mittelschulen, oder Sonderschulen

# Check variable class: 
sapply(data_Pflichtschulen, class)

# rename variables to fit with our zt data set
names(data_Pflichtschulen)
names(data_Pflichtschulen)[names(data_Pflichtschulen) == 'PHI'] <- 
  'latitude'
names(data_Pflichtschulen)[names(data_Pflichtschulen) == 'LAMDA'] <- 
  'longitude'
names(data_Pflichtschulen)[names(data_Pflichtschulen) == 'PLZ'] <- 
  'Postleitzahl'


# plot the data
# -------------
qplot(longitude, latitude, data = data_Pflichtschulen, 
      main="Pflichtschulen")
qplot(longitude, latitude, color = KAT3, 
      data = data_Pflichtschulen, main="Pflichtschulen")


# Variablen in dataset
myvars_Pflichtschulen <- c("NAME", "ORT", "KAT2", 
                           "KAT3", "longitude",
                           "latitude", "ANSCHRIFT", "Postleitzahl")
table(myvars_Pflichtschulen)

# new dataset: 
data_Pflichtschulen <- data_Pflichtschulen[myvars_Pflichtschulen]
#head(data_Pflichtschulen)
sapply(data_Pflichtschulen, class)
######################################################################

# Data from Graz: höhere Schulen
# ------------------------------------------------------------
data_hoehereSchulen <- read.csv("HoehereSchulen_PUNKT.csv", sep=";")

# summary(data_hoehereSchulen)
str(data_hoehereSchulen)

# Check variable class: 
sapply(data_hoehereSchulen, class)

# rename variables to fit with our zt data set
#
names(data_hoehereSchulen)[names(data_hoehereSchulen) == 'PHI'] <- 
  'latitude'
names(data_hoehereSchulen)[names(data_hoehereSchulen) == 'LAMDA'] <- 
  'longitude'
names(data_hoehereSchulen)[names(data_hoehereSchulen) == 'PLZ'] <- 
  'Postleitzahl'


# variablen in dataset
myvars_hoehereSchulen <- c("NAME", "ORT", "KAT2", 
                           "KAT3", "longitude",
                           "latitude", "ANSCHRIFT", "Postleitzahl")
table(myvars_hoehereSchulen)

# new dataset: 
data_hoehereSchulen <- data_hoehereSchulen[myvars_hoehereSchulen]
head(data_hoehereSchulen)
sapply(data_hoehereSchulen, class)

# plot the data
qplot(longitude, latitude, data = data_hoehereSchulen, 
      main="höhere Schulen")
qplot(longitude, latitude, color = KAT3,  
      data = data_hoehereSchulen, main="höhere Schulen Graz")  

plot_hoehereSchulen <- ggplot(data=data_hoehereSchulen) + 
  geom_point(mapping=aes(x=longitude, y=latitude, 
                         color=KAT3))
plot_hoehereSchulen

######################################################################

# plot graph with inputs from two data sources

# plot0: verkaufte wohnungen graz
plot0 <- ggplot(data=data1b) + 
  geom_point(mapping=aes(data1b$longitude, data1b$latitude), 
             color = "red") 
plot0

# plot1: verkaufte wohnungen graz + höhere schulen
(plot1 <- ggplot(data1b) + 
    geom_point(mapping = aes(x=data1b$longitude, y= data1b$latitude), 
               color="red") +
    geom_point(data = data_hoehereSchulen, aes(x=longitude, y=latitude)))
plot1

# plot 2: Pflichtschulen + höhere Schulen 
plot2 <- ggplot(data_hoehereSchulen) + 
  geom_point(mapping = aes(x=longitude, y= latitude), color="blue") +
  geom_point(data = data_Pflichtschulen, aes(x=longitude, y=latitude, 
                                             color="red"))
plot2

# plot 3: Pflichtschulen (blau) + höhere schulen (schwarz) 
# + Wohnungsverkäufe graz (rot)
library(RgoogleMaps)
library(ggmap)

lat.range=range(data1a$latitude)
lon.range=range(data1a$longitude)

geo.basemap <- GetMap.bbox(lon.range, lat.range,destfile="geo_BaseMap.png",maptype="roadmap",zoom=11)

PlotOnStaticMap(geo.basemap, data1a$latitude, data1a$longitude, zoom=20, cex=0.5, pch=19, col="red",FUN=points, add=F)

lat.centre=median(data1a$latitude)
lon.centre=median(data1a$longitude) 

# URL für Karte mit Zentrum Graz

geo.basemap2 <- get_map(location=c("lon.centre","lat.centre"),maptype="roadmap",source="google",zoom=12)


plot3 <- ggplot(data1b)+ 
    geom_point(mapping = aes(x=data1b$longitude, y= data1b$latitude), 
               color="red") +
    geom_point(data = data_hoehereSchulen, aes(x=longitude, y=latitude)) +
    geom_point(data=data_Pflichtschulen, aes(x=longitude, y=latitude), color="blue")

plot3+labs(title="apartment transactions and schools in Graz", x = "longitude", y = "latitude", caption="red=apartment transactions,blue=primary schools,black=secondary schools")

# to do here: add labels for axis, heading and explanations for which 
#color is which... 


######################################################################
# merge Pflichtschulen und hoehere Schulen dataset into one
# data_schulen
######################################################################

#names(data_Pflichtschulen)  # check that column names are the same
#names(data_hoehereSchulen)

data_Schulen <- rbind(data_Pflichtschulen,data_hoehereSchulen)
data_Schulen <- as_data_frame(data_Schulen)
summary(data_Schulen)
nrow(data_Schulen)

library(tibble)
data_Schulen <- as_data_frame(data_Schulen)
# Print
head(data_Schulen)

 

# create subset of data_Schulen to look only at VS, NMS...
#----------------------------------------------------------------
# only VS
#--------

data_Schulen_VS <- 
  dplyr::filter(data_Schulen, trimws(KAT3) == "Volksschule")

# trimws helps to keep dplyr stable

# only NMS or Hauptschule or Realschule or (Sonderschule so far not)
#--------------------------------------------------------------------
data_Schulen_NMS <- 
  dplyr::filter(data_Schulen, trimws(KAT3) %in% c("Neue Mittelschule", 
                            "Polytechnische Schule", "Realschule"))




# create subsets of dataframes with just longitude and latitude 
# coordinates
#---------------------------------------------------------------
#
data_Schulen_coord <- select(data_Schulen, longitude, latitude)
head(data_Schulen_coord)

data_Schulen_VS_coord <- select(data_Schulen_VS, longitude, latitude)
head(data_Schulen_VS_coord)

data_Schulen_NMS_coord <- select(data_Schulen_NMS, longitude, latitude)
head(data_Schulen_NMS_coord)

# ZT daten
data1b_coord <- select(data1b, longitude, latitude)
head(data1b_coord)


###################################################################
# create distance function between the two data sets
###################################################################

library(geosphere)

#dist between apartment and school
# reihen = schulen, spalten = wohnungen
#Schooldistance1 <- distm(data_Schulen_coord, data1b_coord,
#fun = distHaversine)  

#dist between apartment and school
# reihen = wohnungen, Spalten = schulen
Schooldistance2 <- distm(data1b_coord,data_Schulen_coord,
                         fun=distHaversine )  

# try out the distance matrices:

#summary(Schooldistance2)  
          #for each school this shows how close each sold apartment is
#head(Schooldistance2)
Schooldistance2[1,1]        # school1 is from apartm1 1672m away
Schooldistance2[1,2]
min(Schooldistance2[1,])  
              #closest school for the first apartm. is 910m away

# ---------------------------------------------------------------------
# find name of closest school to data2 and save it as variable 
# "nearestSchool" in data2

data1b$nearestSchool<- data_Schulen$NAME[max.col(-Schooldistance2)]

#summary(data2$nearestSchool)
head(data1b$nearestSchool)


# -------------------------------------------------------------
# create a matrix with two columns: 
# 1st column: which is the closest school?
# 2nd column: how many meters away?

X <- Schooldistance2
library(tibble)
X <- as_data_frame(X)

colnames(X) <- data_Schulen$NAME  # set column names equal to schoolnames
rownames(X) 

resultA <- (sapply(seq(nrow(X)), function(i) {   # which school is closest?
  j <- which.min(X[i,]) }))                     # this is already done above differently
resultB <- (sapply(seq(nrow(X)), function(i) {   # how many meters to next school?
  j <- min(X[i,]) }))

print(X)
print(resultA)        # which is the closest school?
print (resultB)       # how many meters to next school?
 
# check that it works

resultA[1] # shows that for first apartment closest school is VS Graz Gösting
resultB[1]  # shows that it is 910 meters away

# save the meters to nearest school as variable in data1b

data1b$nearestSchool_meter <- resultB

# ----------------------------------------------------------------
# nearest KAT3 school (VS, NMS....)
data1b$nearest_KAT3 <- data_Schulen$KAT3[max.col(-Schooldistance2)]
head(data1b$nearest_KAT3)

# head(data1b)
# print(data1b$nearest_KAT3)

###################################################################
# now the same for only NMS


#dist between apartment and NMS
# reihen = wohnungen, Spalten = schulen
Schooldistance2_NMS <- 
  distm(data1b_coord,data_Schulen_NMS_coord,fun=distHaversine )  

# try out the distance matrices:
head(Schooldistance2_NMS)
Schooldistance2_NMS[1,1]  # for apartm 1 the closest MNS is 2982m away

# ----------------------------------------------------------------------
# find name of closest NMS to data1b and save it as variable "nearestNMS"
# in data1b
# this section does the same as the next section (still a good test :-))

data1b$nearestNMS<- data_Schulen_NMS$NAME[max.col(-Schooldistance2_NMS)]

#summary(data2$nearestSchool)
head(data1b$nearestNMS)


# ----------------------------------------------------------------------
# create a matrix with two columns: 1st column: which is the closest NMS
# second column: how many meters away

X <- Schooldistance2_NMS
library(tibble)
X <- as_data_frame(X)

colnames(X) <- data_Schulen_NMS$NAME  # set column names equal NMS names
rownames(X) 

resultA <- (sapply(seq(nrow(X)), function(i) {   # which school is closest?
  j <- which.min(X[i,]) }))                     # this is already done above differently

resultB <- (sapply(seq(nrow(X)), function(i) {  # how many meters to next school?
  j <- min(X[i,]) }))

print(X)
print(resultA)        # which is the closest school?
print (resultB)       # how many meters to next school?

# check that it works
resultA[1] # shows that for first apartment closest NMS is HS Graz-Andritz
resultB[1]  # shows that it is 2347 meters away

# save the meters to nearest NMS as variable in data1b
data1b$nearestNMS_meter <- resultB

#########################################################################
# now the same for only VS


#dist between apartment and VS
# reihen = wohnungen, Spalten = schulen
Schooldistance2_VS <- distm(data1b_coord,data_Schulen_VS_coord,
                            fun=distHaversine )  

# try out the distance matrices:
head(Schooldistance2_VS)
Schooldistance2_VS[1,1]

# ----------------------------------------------------------------------------

# create a matrix with two columns: 1st column: which is the closest school
# second column: how many meters away

X <- Schooldistance2_VS
library(tibble)
X <- as_data_frame(X)

colnames(X) <- data_Schulen_vS$NAME  # set column names equal NMS names
rownames(X) 

resultA <- (sapply(seq(nrow(X)), function(i) { # which school is closest?
  j <- which.min(X[i,]) }))                       

resultB <- (sapply(seq(nrow(X)), function(i) { # how many meters to next school?
  j <- min(X[i,]) }))

print(X)
print(resultA)        # which is the closest school?
print (resultB)       # how many meters to next school?

# check that it works
resultA[1] # shows that for first apartment closest VS is V12???
resultB[1]  # shows that it is 910m away

# save the meters to nearest VS as variable in data1b
data1b$nearestVS_meter <- resultB

# viele Schulen haben Namen mit Umlauten etc. 
# die werden hier geändert: 

class(data1b$nearestSchool)  # saved as factor


library(tibble)
data1c <- as_data_frame(data1b)     # the ZTdata plus Schooldistances
str(data1c)





#########################################################################
#########################################################################

# now add pharmacy data

######################################################################

head(data1c)

# Apotheken Data Graz
# -----------------
Apotheken_data <- read.csv("Apotheken_19012018.csv", sep=";")

summary(Apotheken_data)

# Apotheken mit latitude und lontitude
qplot(PHI, LAMBDA...., data = Apotheken_data, main = "Apotheken Graz")

# Check variable class: 
sapply(Apotheken_data, class)

# rename variables to fit with our zt data set
names(Apotheken_data)
names(Apotheken_data)[names(Apotheken_data) == 'PHI'] <- 'latitude'
names(Apotheken_data)[names(Apotheken_data) == 'LAMBDA....'] <- 
  'longitude'
names(Apotheken_data)[names(Apotheken_data) == 'PLZ'] <- 
  'Postleitzahl'

# change "," into "." and longitude and latitude from factor to numeric 

Apotheken_data$longitude <- as.character(Apotheken_data$longitude)
Apotheken_data$latitude <- as.character(Apotheken_data$latitude)

Apotheken_data$longitude <-
  gsub(",", ".", Apotheken_data$longitude, fixed=TRUE)
Apotheken_data$latitude <-
  gsub(",", ".", Apotheken_data$latitude, fixed=TRUE)

Apotheken_data$longitude <- 
  as.numeric(Apotheken_data$longitude)
Apotheken_data$latitude <- 
  as.numeric(Apotheken_data$latitude)

# Check variable class: 
sapply(Apotheken_data, class)


# Convert to a tibble (better for large data sets)
library(tibble)
Apotheken_data <- as_data_frame(Apotheken_data)
head(Apotheken_data)

myvars_Apotheken <- c("NAME", "ORT", 
                      "KAT3", "longitude",
                      "latitude", "ANSCHRIFT", "Postleitzahl")

library(dplyr)
Apotheken_data <- select(Apotheken_data, myvars_Apotheken)        

summary(Apotheken_data)


# plot Apotheken and Wohnungen
#------------------------------
library(ggplot2)
library(ggmap)

# plot0: verkaufte wohnungen graz
plot0 <- ggplot(data=data1c) + 
  geom_point(mapping=aes(data1c$longitude, data1c$latitude), 
             color = "red") 
plot0

# plot01: Apotheken
plot01 <- ggplot(data=Apotheken_data) + 
  geom_point(mapping = aes(x=longitude, y=latitude))
plot01

# plot1: verkaufte wohnungen graz + Apotheken 
(plot1 <- ggplot(data1c) + 
    geom_point(mapping = aes(x=data1c$longitude, y= data1c$latitude), 
               color="red") +
    geom_point(data = Apotheken_data, aes(x=longitude,
                                          y=latitude)))
plot1

# plot 2: Pflichtschulen (blau) + höhere schulen (schwarz) +
# Apotheken (grün)
# + Wohnungsverkäufe graz (rot)
(plot2 <- ggplot(data1c) + 
    geom_point(mapping = aes(x=data1c$longitude, y= data1c$latitude), 
               color="red") +
    geom_point(data = data_hoehereSchulen, aes(x=longitude, y=latitude)) + 
    geom_point(data=data_Pflichtschulen, aes(x=longitude, y=latitude), 
               color="blue")+
    geom_point(data = Apotheken_data, aes(x=longitude,
                                          y=latitude), color="green")+
    ggtitle("Wohnungen (rot), Pflichtschulen(blau), höhere Schulen 
            (rot) und Apotheken (grün)"))
plot2


# create subsets of dataframes with just longitude and latitude coordinates
#---------------------------------------------------------------------------
#
Apotheken_data_coord <- select(Apotheken_data, longitude, latitude)
head(Apotheken_data_coord)

# ZT daten
data1c_coord <- select(data1c, longitude, latitude)
head(data1c_coord)


#####################################################################
# create distance function between the two data sets
###################################################################

library(geosphere)  

#dist between apartment and school
# reihen = wohnungen, Spalten = Apotheken
ApothekenDist <- distm(data1c_coord,Apotheken_data_coord,fun=distHaversine )  

# try out the distance matrices:
summary(ApothekenDist)  
ApothekenDist[1,]   # distance from first apartment to all 60 pharmacies




# -------------------------------------------------------------------------
# create a matrix with two columns: 1st column: which is the closest school
# second column: how many meters away

X <- ApothekenDist
library(tibble)
X <- as_data_frame(X)

colnames(X) <- Apotheken_data$NAME  # set column names equal to pharmacy Names
rownames(X) 

resultA <- (sapply(seq(nrow(X)), function(i) { # which pharmacy is closest?
  j <- which.min(X[i,]) }))                    # this is already done above differently

resultB <- (sapply(seq(nrow(X)), function(i) { # how many meters to next pharmacy?
  j <- min(X[i,]) }))

print(X)
# print(resultA)        # which is the closest pharmacy?
# print (resultB)       # how many meters to next pharmacy?

# check that it works
resultA[1] # shows that for first apartment closest 
#pharmacy is Apotheke Shopping Nord
resultB[1]  # shows that it is 650 meters away


# I could also have saved the name of closest Pharmacy to the dataset,
# but I don't think we need it
# so only new variable that I add is distance to next pharmacy
data1c$nearestApotheke_meter <- resultB

str(data1c)



#####################################################################


#################################
#   Distance to city center     #
#################################


# Hauptplatz Graz longitude and latitude:
# -----------------------------------------
#Hauptplatz longitude 15.438391
#Hauptplatz latitude  47.070794

Hauptplatz_coord <- c(15.438391, 47.070794)

Hauptplatz_coord 

# ZT daten - longitude und latitude
head(data1c_coord)

HauptplatzDist <- distm(data1c_coord,Hauptplatz_coord,fun=distHaversine )  

X <- HauptplatzDist
library(tibble)
X <- as_data_frame(X)

#colnames(X) <- Hauptplatz_coord  # 
rownames(X) 
                 # 

resultC <- (sapply(seq(nrow(X)), function(i) { 
  j <- min(X[i,]) }))
 

# check that it works:
resultC[1]  # shows that for first apartm Hauptplatz is 
            # 5010m away



# save Dist to Hauptplatz as extra column in dataset. 
data1c$HauptplatzDist <- resultC

str(data1c)


summary(data1c$HauptplatzDist)



################################################
# Aerzte and Bevölkerungsdaten
################################################

# calculated in Aerzte_Graz_2015

Aerzte_data <- read.csv("Aerzte.csv", sep=",")

print(Aerzte_data)

names(data1c)
names(Aerzte_data)

summary(data1c$Bezirk)
summary(Aerzte_data$Bezirk)



# rename Bezirke in Aerzte_data
#------------------------------

class(Aerzte_data$Bezirk)  # factor variable

# save as character var:
Aerzte_data$Bezirk <- as.character(Aerzte_data$Bezirk)

Aerzte_data$Bezirk[Aerzte_data$Bezirk == "InnereStadt"] <- 
  "Innere Stadt"
Aerzte_data$Bezirk[Aerzte_data$Bezirk == "St.Peter"] <- 
  "St. Peter"
Aerzte_data$Bezirk[Aerzte_data$Bezirk == "St.Leonhard"] <- 
  "St. Leonhard"
Aerzte_data$Bezirk[Aerzte_data$Bezirk == "Gösting"] <- 
  "Goesting"
Aerzte_data$Bezirk[Aerzte_data$Bezirk == "Straßgang"] <- 
  "Strassgang"

# save back to factor var:
Aerzte_data$Bezirk <- as.factor(Aerzte_data$Bezirk)

#now merge the Aerzte and Bevölk. data with our old dataframe:
#------------------------------------------------------------

data1d <- merge(data1c, Aerzte_data, by="Bezirk")


########################################################################
########################################################################



head(data1d)

# Kindergärten Graz
####################


Kindergarten <- 
  read.csv("Kindergarten_graz_PUNKT.csv", sep=";")

summary(Kindergarten)


# Check variable class: 
sapply(Kindergarten, class)

# rename variables to fit with our zt data set
names(Kindergarten)
names(Kindergarten)[names(Kindergarten) == 'PHI'] <- 
  'latitude'
names(Kindergarten)[names(Kindergarten) == 'LAMBDA'] <- 
  'longitude'
names(Kindergarten)[names(Kindergarten) == 'PLZ']<- 
  'Postleitzahl'



# Convert to a tibble 
library(tibble)
Kindergarten <- as_tibble(Kindergarten)
head(Kindergarten)

myvars_Kindergarten <- c("NAME", "ORT", 
                      "KAT3", "longitude",
                      "latitude", "ANSCHRIFT", "Postleitzahl")

library(dplyr)
Kindergarten<- select(Kindergarten, myvars_Kindergarten)        

summary(Kindergarten)

# plot the Kindergarten in Graz
# -----------------------------
qplot(longitude, latitude, data = Kindergarten, 
      main="Kindergarten")



# create subsets of dataframes with just longitude and latitude 
#--------------------------------------------------------------
#
Kindergarten_coord <- select(Kindergarten, longitude, latitude)
head(Kindergarten_coord)

# ZT daten: create new coord for data1d
data1d_coord <- select(data1d, longitude, latitude)
head(data1d_coord)



# create distance function between the two data sets
----------------------------------------------------

library(geosphere)  

#dist between apartment and Kindergarten
# reihen = wohnungen, Spalten = Kindergarten
KindergartenDist <- 
  distm(data1d_coord,Kindergarten_coord,fun=distHaversine )  

# try out the distance matrices:
str(KindergartenDist)  
# KindergartenDist[1,]   # distance from first apartment to all 60 pharmacies
KindergartenDist[1,1]   # dist from 1st apartm to 1st Kiga = 1717m
KindergartenDist[1,100]  # dist 1st apartm to 100th Kiga = 3621m


# -------------------------------------------------------------------------
# create a matrix with two columns:
# 1st column: which is the closest Kiga
# second column: how many meters away

X <- KindergartenDist
library(tibble)
X <- as_data_frame(X)

colnames(X) <- Kindergarten$NAME  # set column names equal to Kindergarten Names
rownames(X) 

resultA <- (sapply(seq(nrow(X)), function(i) { # which Kindergarten is closest?
  j <- which.min(X[i,]) }))                    # this is already done above differently

resultB <- (sapply(seq(nrow(X)), function(i) { # how many meters to next Kindergarten?
  j <- min(X[i,]) }))

print(X)
# print(resultA)        # which is the closest Kindergarten?
# print (resultB)       # how many meters to next Kindergarten?

# check that it works
resultA[1] # shows that for first apartment closest 
#Kindergarten
resultB[1]  # shows that it is 650 meters away


# only new variable that I add is distance to next Kindergarten
data1d$Kindergarten_meter <- resultB

# sapply(data1d, class)   # check that all vars saved right


data1e <- as_data_frame(data1d)    # save as new data frame

###############################################################
##############################################################


head(data1e)

# add Kinderkrippen Graz
########################


Kinderkrippen <- read.csv("Kinderkrippen.csv", sep=";")

names(Kinderkrippen)

# Check variable class: 
sapply(Kinderkrippen, class)

# rename variables to fit with our zt data set
names(Kinderkrippen)
names(Kinderkrippen)[names(Kinderkrippen) == 'PHI'] <- 'latitude'
names(Kinderkrippen)[names(Kinderkrippen) == 'LAMBDA'] <- 
  'longitude'
names(Kinderkrippen)[names(Kinderkrippen) == 'PLZ'] <- 
  'Postleitzahl'


# change "," into "." 
# and longitude and latitude from factor to numeric 
# -------------------------------------------------

Kinderkrippen$longitude <- as.character(Kinderkrippen$longitude)
Kinderkrippen$latitude <- as.character(Kinderkrippen$latitude)

Kinderkrippen$longitude <-
  gsub(",", ".", Kinderkrippen$longitude, fixed=TRUE)
Kinderkrippen$latitude <-
  gsub(",", ".", Kinderkrippen$latitude, fixed=TRUE)

Kinderkrippen$longitude <- 
  as.numeric(Kinderkrippen$longitude)
Kinderkrippen$latitude <- 
  as.numeric(Kinderkrippen$latitude)

# Check variable class: 
sapply(Kinderkrippen, class)


# Convert to data_frame 
library(tibble)
Kinderkrippen<- as_data_frame(Kinderkrippen)
head(Kinderkrippen)

myvars_Kinderkrippen <- c("NAME", "ORT", 
                      "KAT3", "longitude",
                      "latitude", "ANSCHRIFT", "Postleitzahl")

library(dplyr)
Kinderkrippen <- select(Kinderkrippen, myvars_Kinderkrippen)        

# summary(Kinderkrippen)


# plot Kinderkrippen in Graz
# -----------------------------
qplot(longitude, latitude, data = Kinderkrippen, 
      main="Kinderkrippen")



# create subsets of dataframes with just longitude and latitude 
#--------------------------------------------------------------
#
Kinderkrippen_coord <- select(Kinderkrippen, longitude, latitude)
head(Kinderkrippen_coord)

# ZT daten: create new coord for data1d
data1e_coord <- select(data1e, longitude, latitude)
head(data1e_coord)



# create distance function between the two data sets
----------------------------------------------------
  
library(geosphere)  

KinderkrippenDist <- 
  distm(data1e_coord,Kinderkrippen_coord,fun=distHaversine )  

# try out the distance matrices:
str(KinderkrippenDist)  
# KindergartenDist[1,]   
KinderkrippenDist[1,1]   # dist from 1st apartm to 1st Kiga = 4125m
KinderkrippenDist[1,50]  # dist 1st apartm to 50th Kiga = 3182m


# -------------------------------------------------------------------------
# create a matrix with two columns:
# 1st column: which is the closest Kinderkrippe
# second column: how many meters away

X <- KinderkrippenDist
library(tibble)
X <- as_data_frame(X)

colnames(X) <- Kinderkrippen$NAME  # set column names equal to Kinderkrippen Names
# rownames(X) 

resultA <- (sapply(seq(nrow(X)), function(i) { # which Kindergarten is closest?
  j <- which.min(X[i,]) }))                    # this is already done above differently

resultB <- (sapply(seq(nrow(X)), function(i) { # how many meters to next Kindergarten?
  j <- min(X[i,]) }))


# print(resultA)        # which is the closest Kinderkrippen?
# print (resultB)       # how many meters to next Kinderkrippenn?

# check that it works
resultA[1] # shows that for first apartment closest Krippe is
# Zwergennest 

resultB[1]  # shows that it is 332 meters away


# only new variable that I add is distance to next Kinderkrippe
data1e$Kinderkrippen_meter <- resultB

# sapply(data1e, class)   # check that all vars saved right


data1f <- as_data_frame(data1e)    # save as new data frame



##########################################################
##########################################################




# add Park and Ride Graz
########################


ParkandRide <- read.csv("ParkandRide_graz.csv", sep=";")

names(ParkandRide)

# Check variable class: 
sapply(ParkandRide, class)

# rename variables to fit with our zt data set
names(ParkandRide)
names(ParkandRide)[names(ParkandRide) == 'PHI'] <- 'latitude'
names(ParkandRide)[names(ParkandRide) == 'LAMBDA'] <- 
  'longitude'



# change "," into "." 
# and longitude and latitude from factor to numeric 
# -------------------------------------------------

ParkandRide$longitude <- as.character(ParkandRide$longitude)
ParkandRide$latitude <- as.character(ParkandRide$latitude)

ParkandRide$longitude <-
  gsub(",", ".", ParkandRide$longitude, fixed=TRUE)
ParkandRide$latitude <-
  gsub(",", ".", ParkandRide$latitude, fixed=TRUE)

ParkandRide$longitude <- 
  as.numeric(ParkandRide$longitude)
ParkandRide$latitude <- 
  as.numeric(ParkandRide$latitude)

# Check variable class: 
sapply(ParkandRide, class)


# Convert to data_frame 
library(tibble)
ParkandRide<- as_data_frame(ParkandRide)
head(ParkandRide)

myvars_ParkandRide <- c("NAME", "ORT", 
                          "KAT3", "longitude",
                          "latitude", "ANSCHRIFT")
                        
library(dplyr)
ParkandRide <- select(ParkandRide, myvars_ParkandRide)        

# summary(ParkandRide)


# plot ParkandRide in Graz
# -----------------------------
qplot(longitude, latitude, data = ParkandRide, 
      main="ParkandRide")



# create subsets of dataframes with just longitude and latitude 
#--------------------------------------------------------------
#
ParkandRide_coord <- select(ParkandRide, longitude, latitude)
head(ParkandRide_coord)

# ZT daten: create new coord for data1d
data1f_coord <- select(data1f, longitude, latitude)



# create distance function between the two data sets
----------------------------------------------------
  
  library(geosphere)  

ParkandRideDist <- 
  distm(data1f_coord,ParkandRide_coord,fun=distHaversine )  

# try out the distance matrices:
str(ParkandRideDist)  
# KindergartenDist[1,]   
ParkandRideDist[1,1]   # dist from 1st apartm to P&R is 7349m
ParkandRideDist[1,5]  # dist 1st apartm to 5th is 6117m


# -------------------------------------------------------------------------
# create a matrix with two columns:
# 1st column: which is the closest P&R
# second column: how many meters away

X <- ParkandRideDist
library(tibble)
X <- as_data_frame(X)

colnames(X) <- ParkandRide$NAME  # set column names equal to P&R Names
# rownames(X) 

resultA <- (sapply(seq(nrow(X)), function(i) { # which P&R is closest?
  j <- which.min(X[i,]) }))                    

resultB <- (sapply(seq(nrow(X)), function(i) { # how many meters to next P&R?
  j <- min(X[i,]) }))


# check that it works
resultA[1] # shows that for first apartment closest P&R is Winzödl

resultB[1]  # shows that it is 1519 meters away


# only new variable that I add is distance to next Kinderkrippe
data1f$ParkandRide_meter <- resultB

# sapply(data1e, class)   # check that all vars saved right


data1g <- as_data_frame(data1f)    # save as new data frame


















###########################################################################
# save this dataset as a dataset in "C:/Users/Robert/Documents/R code RF"
###########################################################################

write.csv(data1g, 
          file = "C:/Users/SaSp/Desktop/B_addedData.csv",
          row.names = FALSE)

# remember to then save somewhere else 
#(otherwise it will be changed with next round)