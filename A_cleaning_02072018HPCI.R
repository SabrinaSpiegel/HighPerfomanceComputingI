# 
#
#
# Datei: A_cleaning_02072018HPCI

# open ZT datafile in csv format and let it run through this program
# 
############################
#                          #
# Cleaning the data file   #
#                          #
############################



library(readr)
library(ggmap)
library(ggplot2)
library(stringi)
library(plyr)
library(dplyr)
library(RgoogleMaps)


getwd()
setwd( "C:/Users/SaSp/Desktop")
dir()

data1a <- read.csv("ExportDatensatzGraz19012018CSV.csv", sep=";")

library(tibble)  # making data_frame

data1a <- as_data_frame(data1a)
summary(data1a)
str(data1a)


# the following variables need to be changed from integer to factor variables:
# ---------------------------------------------------------------------------

data1a$BGNr <- as.factor(data1a$BGNr)
data1a$TZ <- as.factor(data1a$TZ)
data1a$jahrdatum <- as.factor(data1a$jahrdatum)
data1a$GBNr <- as.factor(data1a$GBNr)
data1a$GebAlter <- as.factor(data1a$GebAlter)
data1a$GEMNR <- as.factor(data1a$GEMNR)
data1a$BEZNR <- as.factor(data1a$BEZNR)
data1a$KGNummer <- as.factor(data1a$KGNummer)
data1a$Postleitzahl <- as.factor(data1a$Postleitzahl)

# make sure the following variables are numeric: 

data1a$preissqm <- as.numeric(data1a$preissqm)
data1a$longitude <- as.numeric(as.character(data1a$longitude))
data1a$latitude <- as.numeric(as.character(data1a$latitude))


# the following should be "character" variables

data1a$GBName <- as.character(data1a$GBName)
data1a$landkaeufer <- as.character(data1a$landkaeufer)
data1a$landverkaeufer <- as.character(data1a$landverkaeufer)
data1a$KATNAM <- as.character(data1a$KATNAM)
data1a$Ortk <- as.character(data1a$Ortk)
data1a$GemName <- as.factor(data1a$GemName)

# Vertragsdatum as date

library(data.table)
data1a$Vertragsdatum <- as.Date(data1a$Vertragsdatum, format="%d/%m/%Y")


str(data1a)


# exclude Verwandtschaft, Konkurs, extreme values 
# ------------------------------------------------

# note: by using trimws (trim white space) I can make the filter function 
# run a lot smoother. 

data1a <- dplyr::filter(data1a, trimws(Verwandtschaft) == "FALSCH") 
data1a <- dplyr::filter(data1a, trimws(Konkurs) == "FALSCH")

data1a <- dplyr::filter(data1a,(preissqm > 800))
data1a <- dplyr::filter(data1a,(preissqm < 6000))                        


# exclude points way outside Graz (falsche Eintr�ge)
# --------------------------------------------------

data1a <- dplyr::filter(data1a, trimws(longitude) > 15.3) 
data1a <- dplyr::filter(data1a, trimws(latitude) > 47.0)

# check with plot
qplot(longitude, latitude, data= data1a)

# check data structure 
str(data1a)


# Rename  names with �, �, � to ae, oe, ss etc.
# -----------------------------------------------------

# variable names
names(data1a)[names(data1a)== "Fl�.cheKat1"] <- "FlaecheKat1"
names(data1a)[names(data1a)== "Fl�.cheKat2"] <- "FlaecheKat2"

# individual variable values 
data1a$GBName[data1a$GBName == "Stra�Ygang"] <- "Strassgang"
data1a$GBName[data1a$GBName ==  "Stra�Ygang"] <- "Strassgang"
data1a$GBName[data1a$GBName == "Gösting"] <- "Goesting"
data1a$GBName[data1a$GBName == "Graz Stadt-Fölling"] <- "Graz-Foelling"

data1a$Ortk[data1a$Ortk == "Stra�Ygang"] <- "Strassgang"
data1a$Ortk[data1a$Ortk ==  "Stra�Ygang"] <- "Strassgang"
data1a$Ortk[data1a$Ortk == "Gösting"] <- "Goesting"
data1a$Ortk[data1a$Ortk == "Graz Stadt-Fölling"] <- "Graz-Foelling"

data1a$nearestSchool[data1a$nearestSchool == "VS Graz-Stra�Ygang"] <- "VS Graz-Stra�gang"
data1a$nearestSchool[data1a$nearestSchool == "VS Graz-Schönau"] <- "VS Graz-Sch�nau"
data1a$nearestSchool[data1a$nearestSchool == "VS Graz-Gösting"] <- "VS Graz-G�sting"
data1a$nearestSchool[data1a$nearestSchool == "HS Graz-Stra�Ygang"] <- "HS Graz-Stra�gang"
data1a$nearestSchool[data1a$nearestSchool == "VS Graz-Jägergrund"] <- "VS Graz-J�gergrund"
data1a$nearestSchool[data1a$nearestSchool == "BRG Graz, Körösistra�Ye"] <- "BRG Graz, K�r�sistra�e"
data1a$nearestSchool[data1a$nearestSchool == "HBLA Schrödingerstra�Ye"] <- "HBLA Schr�dingerstra�e"
data1a$nearestSchool[data1a$nearestSchool == "HS Graz-Fröbel"] <- "HS Graz-Fr�bel"
data1a$nearestSchool[data1a$nearestSchool == "Praxis HS der Päd.Hochschule in Stmk."] <- "Praxis HS der P�d.Hochschule in Stmk."
data1a$nearestSchool[data1a$nearestSchool == "Schule für Sozialbetr./Altenarbeit d. Diakoniewerkes"] <- "Schule f�r Sozialbetr./Altenarbeit d. Diakoniewerkes"
data1a$nearestSchool[data1a$nearestSchool == "HTBLVA Gösting (BULME)"] <- "HTBLVA G�sting (BULME)"
data1a$nearestSchool[data1a$nearestSchool == "BORG Dreierschützengasse"] <- "BORG Dreiersch�tzengasse"
data1a$nearestSchool[data1a$nearestSchool == "BG/BRG Graz, Klusemannstra�Ye"] <- "BG/BRG Graz, Klusemannstra�e"
data1a$nearestSchool[data1a$nearestSchool == "Landessonderschule für körperbeh. und mehrfach behinderte Kinder"] <- "Landessonderschule f�r k�rperbeh. und mehrfach behinderte Kinder"
data1a$nearestSchool[data1a$nearestSchool == "Schule für med.-tech. Fachdienst"] <- "Schule f�r med.-tech. Fachdienst"
data1a$nearestSchool[data1a$nearestSchool == "Regenbogenschule - Förderung alternativer Lernmethoden in Graz"] <- "Regenbogenschule - F�rderung alternativer Lernmethoden in Graz"
data1a$nearestSchool[data1a$nearestSchool == "HS Graz-St. Andr�"] <- "HS Graz-St. Andr�"
data1a$nearestSchool[data1a$nearestSchool == "Praxis VS der Päd. Hochschule"] <- "Praxis VS der P�d. Hochschule"
data1a$nearestSchool[data1a$nearestSchool == "Heilstättenschule LKH-Universitätsklinikum Graz (SPZ)"] <- "Heilst�ttenschule LKH-Universit�tsklinikum Graz (SPZ)"
data1a$nearestNMS[data1a$nearestNMS == "HS Graz-Stra�Ygang"] <- "HS Graz-Stra�gang"
data1a$nearestNMS[data1a$nearestNMS == "HS Graz-Fröbel"] <- "HS Graz-Fr�bel"
data1a$nearestNMS[data1a$nearestNMS == "Praxis HS der Päd.Hochschule in Stmk."] <- "Praxis HS der P�d.Hochschule in Stmk."
data1a$nearestNMS[data1a$nearestNMS == "HS Graz-St. Andr�"] <- "HS Graz-St. Andr�"

data1a$nearestVS[data1a$nearestVS == "VS Graz-Stra�Ygang"] <- "VS Graz-Stra�gang"
data1a$nearestVS[data1a$nearestVS == "VS Graz-Schönau"] <- "VS Graz-Sch�nau"
data1a$nearestVS[data1a$nearestVS == "VS Graz-Gösting"] <- "VS Graz-G�sting"
data1a$nearestVS[data1a$nearestVS == "VS Graz-Jägergrund"] <- "VS Graz-J�gergrund"

data1a$nearest_KAT3[data1a$nearest_KAT3 == "Allgemein bildende höhere Schule"] <- "Allgemein bildende h�here Schule"
data1a$nearest_KAT3[data1a$nearest_KAT3 == "Höhere Bundeslehranstalt"] <- "H�here Bundeslehranstalt"
data1a$nearest_KAT3[data1a$nearest_KAT3 == "Höhere technische - gewerbliche Lehranstalt"] <- "H�here technische - gewerbliche Lehranstalt"
data1a$nearest_KAT3[data1a$nearest_KAT3 == "Kaufmännische Schule"] <- "Kaufm�nnische Schule"
data1a$nearest_KAT3[data1a$nearest_KAT3 == "Sonderschule für Kinder mit sonderpäd. Förderbedarf"] <- "Sonderschule f�r Kinder mit sonderp�d. F�rderbedarf"


# Freie Felder bei Postleitzahl k�nnen durch die GBNamen ausgef�llt werden: 
# -------------------------------------------------------------------------

# zuerst filtere die Reihen nach Postleitzahl == empty

subset1 <- subset(data1a, (is.na(data1a$Postleitzahl)))
subset2 <- subset(data1a, (!is.na(data1a$Postleitzahl)))

# dann die PLZ des GBNamens einf�gen:

subset1$Postleitzahl[subset1$GBName =='Lend'] <- '8020'
subset1$Postleitzahl[subset1$GBName =='Algersdorf'] <- '8020'
subset1$Postleitzahl[subset1$GBName =='Andritz'] <- '8045'
subset1$Postleitzahl[subset1$GBName =='Engelsdorf'] <- '8041'
subset1$Postleitzahl[subset1$GBName =='Goesting'] <- '8051'
subset1$Postleitzahl[subset1$GBName =='Geidorf'] <- '8010'
subset1$Postleitzahl[subset1$GBName =='St. Leonhard'] <- '8010'
subset1$Postleitzahl[subset1$GBName =='Gries'] <- '8020'
subset1$Postleitzahl[subset1$GBName =='Jakomini'] <- '8010'
subset1$Postleitzahl[subset1$GBName =='Liebenau'] <- '8041'
subset1$Postleitzahl[subset1$GBName =='St. Peter'] <- '8042'
subset1$Postleitzahl[subset1$GBName =='Waltendorf'] <- '8010'
subset1$Postleitzahl[subset1$GBName =='Wenisbuch'] <- '8044'
subset1$Postleitzahl[subset1$GBName =='Wetzelsdorf'] <- '8052'
subset1$Postleitzahl[subset1$GBName =='Mariatrost'] <- '8044'
subset1$Postleitzahl[subset1$GBName =='Rudersdorf'] <- '8020'
subset1$Postleitzahl[subset1$GBName =='Puntigam'] <- '8055'
subset1$Postleitzahl[subset1$GBName =='Graz Foelling'] <- '8044'
subset1$Postleitzahl[subset1$GBName =='Ragnitz'] <- '8047'
subset1$Postleitzahl[subset1$GBName =='Stifting'] <- '8010'
subset1$Postleitzahl[subset1$GBName =='Strassgang'] <- '8054'
subset1$Postleitzahl[subset1$GBName =='innere Stadt'] <- '8010'
subset1$Postleitzahl[subset1$GBName =='Graz Stadt-Messendorf'] <- '8042'

# Areas with two or more possible post codes: --> check on map???
subset1$Postleitzahl[subset1$GBName =='Graz Stadt-Weinitzen'] <- '8045' # could also be 8044
subset1$Postleitzahl[subset1$GBName =='Baierdorf'] <- '8020' # 8020 od 8052
subset1$Postleitzahl[subset1$GBName =='Webling'] <- '8054'

# die zwei subsets wieder zusammenbauen und check, dass ok: 

data1b <- rbind(subset1, subset2)

sum(is.na(data1b$Postleitzahl))   # 51 NAs gibt's noch -> aber ich mag nicht mehr...




# Bei Terasse, Parkplatz, Balkon, Garten, (und vielleicht Keller)
# die meisten sind UK (unknown). Aber wir k�nnen mit gr. Wahrscheinlihkeit annehmen, 
# dass UK = N ist. Bzw eine neue Kategorie "UK_or_N" schaffen. 

data1b$Terrasse <- revalue(data1b$Terrasse, c("UK"= "UK_or_N", 
                                              "N" = "UK_or_N"
))
data1b$Balkon <- revalue(data1b$Balkon, c("UK"= "UK_or_N", 
                                          "N" = "UK_or_N"
))
data1b$Garten <- revalue(data1b$Garten, c("UK"= "UK_or_N", 
                                          "N" = "UK_or_N"
))
data1b$Parkplatz <- revalue(data1b$Parkplatz, c("UK"= "UK_or_N", 
                                                "N" = "UK_or_N"
))





################################################################################
# create new dataset with only following variables - restrict columns in dataset
################################################################################

# Convert to a tibble (better for large data sets)
library(tibble)
data2 <- as_data_frame(data1b)
# Print
head(data2)

# names(data2)  # shows all possible variables

# select the best suited variables: 

myvars <- c("GesamtPreis","preissqm",  "NutzFl",  
            "NutzungsKat","maxbbd", "widmungkat",
            "longitude", "latitude", 
            "Terrasse", "Balkon", "Garten","Keller", 
            "FlaecheKat1",
            "KATNAM",  "Postleitzahl", "GBName",
            "Vertragsdatum","jahrdatum", 
            "GebAlter","AlterKategorie", "Bautraeger")

table(myvars) 

#create new data frame with only the named variables: 

library(dplyr)
data2 <- select(data2, myvars) 

summary(data2)

# Factor levels that don't make sense --> cleaning

str(data2$maxbbd)
levels(data3$maxbbd)
# levels maxbbd
data1a$maxbbd[data1a$maxbbd == ".2)"] <- "0.2"
data1a$maxbbd[data1a$maxbbd == "67a"] <- "NA"
data1a$maxbbd[data1a$maxbbd == "67b"] <- "NA"
data1a$maxbbd[data1a$maxbbd == "¶Pa"] <- "NA"
data1a$maxbbd[data1a$maxbbd == "�f¶G"] <- "NA"
data1a$maxbbd[data1a$maxbbd == "ALD"] <- "NA"
data1a$maxbbd[data1a$maxbbd == "and"] <- "NA"
data1a$maxbbd[data1a$maxbbd == "ben"] <- "NA"
data1a$maxbbd[data1a$maxbbd == "cke"] <- "NA"
data1a$maxbbd[data1a$maxbbd == "ein"] <- "NA"
data1a$maxbbd[data1a$maxbbd == "Frh"] <- "NA"
data1a$maxbbd[data1a$maxbbd == "Spo"] <- "NA"

levels(data2$widmungkat)
#levels widmungkat
data1a$mwidmungkat[data1a$widmungkat == "allgemeine"] <- "Allgemeines Wohngebiet"
data1a$mwidmungkat[data1a$widmungkat == "Gewerbe-"] <- "Gewerbe- und Industriegebiet"
data1a$mwidmungkat[data1a$widmungkat == "J/I"] <- "Industriegebiet"      
data1a$mwidmungkat[data1a$widmungkat == "WA"] <- "Allgemeines Wohngebiet"
data1a$mwidmungkat[data1a$widmungkat == "#Funktion!"] <- "Bauland"
data1a$mwidmungkat[data1a$widmungkat == "Sonstige"] <- "Sonstige Widmung"



# save the data.frame as a csv Datei 
###########################################################################
# # save data.frame as csv Datei in "C:/Users/Robert/Documents/R code RF
###########################################################################

write.csv(data2, 
          file = "C:/Users/SaSp/Desktop/A_cleaning_data.csv",
          row.names = FALSE)











