#hurricaines of interest:
# Fran 1996
# Floyd 1999
# Matthew 2016

#download and unzip ftp://eclipse.ncdc.noaa.gov/pub/ibtracs/v03r10/wmo/csv/Allstorms.ibtracs_wmo.v03r10.csv.gz
#place in 'wasteStorm/data/'

#download 1996, 1999, 2016 TRI files from https://www.epa.gov/toxics-release-inventory-tri-program/tri-basic-data-files-calendar-years-1987-2016
#place in 'wasteStorm/data/TRI/'

#load TRI data
setwd('/Users/mackfinkel/Desktop/wasteStorm/data/TRI/')
fileNames <- list.files()
TRI <- data.frame()
for (i in 1:length(fileNames)){
  currentTRI <- read.csv(fileNames[i], stringsAsFactors = FALSE)
  # select for animal agriculture (NAICS prefix for animal ag is 112. exclude aquaculture at 1125)
  # NAICS code documentation available at https://www.census.gov/eos/www/naics/
  currentTRI <- currentTRI[substr(as.character(currentTRI$PRIMARY_NAICS), 1, 3) == "112" & substr(as.character(currentTRI$PRIMARY_NAICS), 1, 4) != "1125" | substr(as.character(currentTRI$PRIMARY_NAICS), 1, 4) == "3116",]
  
  currentTRI <- currentTRI[,c("YEAR", 
                              "FACILITY_NAME", 
                              "STREET_ADDRESS", 
                              "CITY", 
                              "COUNTY", 
                              "ST", 
                              "ZIP", 
                              "LATITUDE", 
                              "LONGITUDE", 
                              "PRIMARY_NAICS", 
                              "CHEMICAL", 
                              "UNIT_OF_MEASURE", 
                              "ON.SITE_RELEASE_TOTAL", 
                              "X8.1B_ON.SITE_OTHER_RELEASES",
                              "X8.6_TREATMENT_ON.SITE", 
                              "PARENT_COMPANY_NAME")]
  
  #select nitrogen based chemicals. phosphorus also produced in quantities smaller and proportional
  currentTRI <- currentTRI[currentTRI$CHEMICAL == "AMMONIA" | currentTRI$CHEMICAL == "NITRATE COMPOUNDS",]
  
  # waste = on site released + treated on site
  currentTRI$waste <- currentTRI$ON.SITE_RELEASE_TOTAL + currentTRI$X8.6_TREATMENT_ON.SITE + currentTRI$X8.1B_ON.SITE_OTHER_RELEASES
  
  TRI <- rbind(TRI, currentTRI)
}

# only look at facilities that have waste
TRI <- TRI[TRI$waste > 0,]

noParent <- is.na(TRI$PARENT_COMPANY_NAME)

TRI$PARENT_COMPANY_NAME[noParent] <- TRI$FACILITY_NAME[noParent]

# sum facility waste over 5 year period
library(plyr)
TRIcleaner <- ddply(TRI, .(YEAR, LATITUDE, LONGITUDE, PARENT_COMPANY_NAME, COUNTY, STREET_ADDRESS, CITY, ST, ZIP), summarize,
                    WASTE = sum(waste, na.rm=TRUE)/1000000)

library(ggplot2)
library(ggmap)
library(maps)
library(mapdata)

setwd('..')
storms <- read.csv('Allstorms.ibtracs_wmo.v03r10.csv', stringsAsFactors = FALSE, skip = 1)

#hurricaines of interest:
# Fran 1996
# Floyd 1999
# Matthew 2016

storms <- storms[-1,]
storms$Latitude <- as.numeric(storms$Latitude)
storms$Longitude <- as.numeric(storms$Longitude)
storms$Nature <- substring(storms$Nature, 2)
storms$Nature[storms$Nature=="TD"] = "Tropical Depression"
storms$Nature[storms$Nature=="TS"] = "Tropical Storm"
storms$Nature[storms$Nature=="ET"] = "Extratropical"
storms$Nature[storms$Nature=="SS"] = "Subtropical Storm"

states <- map_data("state")
MATTHEW <-ggplot(data = states) + 
  geom_polygon(aes(x = long, y = lat, group = group), fill = "white", color = "black") + 
  geom_point(data = TRIcleaner[TRIcleaner$YEAR == 2016,], mapping = aes(x = LONGITUDE, y = LATITUDE, color = WASTE)) +
  geom_path(data = storms[storms$Name == "MATTHEW" & storms$Season == 2016,], aes(x = Longitude, y = Latitude)) +
  geom_point(data = storms[storms$Name == "MATTHEW" & storms$Season == 2016,], aes(x = Longitude, y = Latitude, shape = Nature)) +
  guides(fill=TRUE) +  # do this to leave off the color legend
  coord_fixed(xlim = c(-85, -70),  ylim = c(25, 36), ratio = 1.3) +
  ylab(label = "") + xlab(label = "") +
  ggtitle(label = "Animal Waste Near Hurricane Matthew", subtitle = "2016") +
  labs(shape="Storm type", colour="Animal Waste\n(million lbs)") +
  theme(panel.grid = element_blank(), panel.background = element_rect(fill = "gray"))

FLOYD <-ggplot(data = states) + 
  geom_polygon(aes(x = long, y = lat, group = group), fill = "white", color = "black") + 
  geom_point(data = TRIcleaner[TRIcleaner$YEAR == 1999,], mapping = aes(x = LONGITUDE, y = LATITUDE, color = WASTE)) +
  geom_path(data = storms[storms$Name == "FLOYD" & storms$Season == 1999,], aes(x = Longitude, y = Latitude)) +
  geom_point(data = storms[storms$Name == "FLOYD" & storms$Season == 1999,], aes(x = Longitude, y = Latitude, shape = Nature)) +
  guides(fill=TRUE) +  # do this to leave off the color legend
  coord_fixed(xlim = c(-82.5, -70),  ylim = c(28, 40), ratio = 1.3) +
  ylab(label = "") + xlab(label = "") +
  ggtitle(label = "Animal Waste Near Hurricane Floyd", subtitle = "1999") +
  labs(shape="Storm type", colour="Animal Waste\n(million lbs)") +
  theme(panel.grid = element_blank(), panel.background = element_rect(fill = "gray"))

FRAN <-ggplot(data = states) + 
  geom_polygon(aes(x = long, y = lat, group = group), fill = "white", color = "black") + 
  geom_point(data = TRIcleaner[TRIcleaner$YEAR == 1996,], mapping = aes(x = LONGITUDE, y = LATITUDE, color = WASTE)) +
  geom_path(data = storms[storms$Name == "FRAN" & storms$Season == 1996,], aes(x = Longitude, y = Latitude)) +
  geom_point(data = storms[storms$Name == "FRAN" & storms$Season == 1996,], aes(x = Longitude, y = Latitude, shape = Nature)) +
  guides(fill=TRUE) +  # do this to leave off the color legend
  coord_fixed(xlim = c(-85, -70),  ylim = c(30, 43), ratio = 1.3) +
  ylab(label = "") + xlab(label = "") +
  ggtitle(label = "Animal Waste Near Hurricane Fran", subtitle = "1996") +
  labs(shape="Storm type", colour="Animal Waste\n(million lbs)") +
  theme(panel.grid = element_blank(), panel.background = element_rect(fill = "gray"))

library(ggplot2)
library (rgdal)
library (rgeos)
library(maptools)
library(tmap)

# florence data from shp file in "Advisory Forecast Track, Cone of Uncertainty, and Watches/Warnings" category https://www.nhc.noaa.gov/gis/
# link used: https://www.nhc.noaa.gov/gis/forecast/archive/al062018_5day_latest.zip
pgn <- readOGR("al062018_5day_latest/al062018-055A_5day_pgn.shp")
ln <- readOGR("al062018_5day_latest/al062018-055A_5day_lin.shp")
FLORENCE <- ggplot(data = states) + 
  geom_polygon(aes(x = long, y = lat, group = group), fill = "white", color = "black") +
  geom_polygon(data=pgn, aes(long, lat, group = group), alpha = .2) +
  geom_path(data=ln, aes(long, lat, group = group)) +
  geom_point(data=ln, aes(long, lat, group = group), shape = 17) +
  geom_point(data = TRIcleaner[TRIcleaner$YEAR == 1996,], mapping = aes(x = LONGITUDE, y = LATITUDE, color = WASTE)) +
  coord_fixed(xlim = c(-87, -72),  ylim = c(31, 40.5), ratio = 1.3) +
  ylab(label = "") + xlab(label = "") +
  ggtitle(label = "Animal Waste on Hurricane Florence Path", subtitle = "2018 (path as of 9-13-18 12 AM, waste data from 2016)") +
  labs(shape="Storm type", colour="Animal Waste\n(million lbs)") +
  theme(panel.grid = element_blank(), panel.background = element_rect(fill = "gray"))
