# farm waste, poverty, and race

setwd('/Users/mackfinkel/Documents/waste/')

#Census data source: Steven Manson, Jonathan Schroeder, David Van Riper, and Steven Ruggles. IPUMS National Historical Geographic Information System: Version 12.0 [Database]. Minneapolis: University of Minnesota. 2017. http://doi.org/10.18128/D050.V12.0
#tri files: https://www.epa.gov/toxics-release-inventory-tri-program/tri-basic-data-files-calendar-years-1987-2016

#load census data, CPS five year estimates 2011-2015
popData <- read.csv('nhgis0003_csv/nhgis0003_ds215_20155_2015_blck_grp.csv')
popDataClean <- popData[,c("GISJOIN", "STATE", "STATEA", "COUNTY", "COUNTYA", "TRACTA", "BLKGRPA", "ADKJE001", "ADKWE001", "ADKXE002", "ADKXE003", "ADKXE004", "ADKXE005", "ADKXE006", "ADKXE007", "ADKXE008", "ADKXE009", "ADKXE010", "ADNEE001", "ADNEE002", "ADNEE003", "ADNEE004", "ADNEE005", "ADNEE006", "ADNEE007", "ADNEE008", "ADSBE002", "ADSBE017", "ADSBE018", "ADSBE033", "ADSBE034", "ADSBE050", "ADSBE051", "ADSBE066" ,"ADOVE001")]
popDataClean <- popDataClean[-1,]

# load TRI data
setwd('TRI/')
fileNames <- list.files()
TRI <- data.frame()
# load facility data from 2011 - 2015
for (i in 1:length(fileNames)){
  currentTRI <- read.csv(fileNames[i])
  
  # select for animal agriculture
  currentTRI <- currentTRI[substr(as.character(currentTRI$PRIMARY_NAICS), 1, 4) == "3111" | substr(as.character(currentTRI$PRIMARY_NAICS), 1, 4) == "3115" | substr(as.character(currentTRI$PRIMARY_NAICS), 1, 4) == "3116",]
  
  # select variables of interest
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
                              "X5.5.2_LAND_TREATMENT", 
                              "X5.2_STACK_AIR", 
                              "X5.1_FUGITIVE_AIR", 
                              "X5.3_WATER", 
                              "OFF.SITE_RELEASE_TOTAL", 
                              "TOTAL_RELEASES", 
                              "X8.1B_ON.SITE_OTHER_RELEASES",
                              "X8.6_TREATMENT_ON.SITE", 
                              "PARENT_COMPANY_NAME")]
  TRI <- rbind(TRI, currentTRI)
}
# waste = on site released + treated on site
TRI$waste <- TRI$ON.SITE_RELEASE_TOTAL + TRI$X8.6_TREATMENT_ON.SITE

# only look at facilities that have waste
TRI <- TRI[TRI$waste > 0,]

# sum facility waste over 5 year period
library(plyr)
TRIcleaner <- ddply(TRI, .(LATITUDE, LONGITUDE, PARENT_COMPANY_NAME, COUNTY, STREET_ADDRESS, CITY, ST, ZIP), summarize,
                  ON.SITE_RELEASE_TOTAL = sum(ON.SITE_RELEASE_TOTAL, na.rm=TRUE),
                  X5.5.2_LAND_TREATMENT = sum(X5.5.2_LAND_TREATMENT, na.rm=TRUE),
                  X5.2_STACK_AIR = sum(X5.2_STACK_AIR, na.rm=TRUE),
                  X5.1_FUGITIVE_AIR = sum(X5.1_FUGITIVE_AIR, na.rm=TRUE),
                  X5.3_WATER = sum(X5.3_WATER, na.rm=TRUE),
                  TOTAL_RELEASES = sum(TOTAL_RELEASES, na.rm=TRUE),
                  X8.1B_ON.SITE_OTHER_RELEASES = sum(X8.1B_ON.SITE_OTHER_RELEASES, na.rm=TRUE),
                  X8.6_TREATMENT_ON.SITE = sum(X8.6_TREATMENT_ON.SITE, na.rm=TRUE))


library(geosphere)

library(foreign)
# load block group data from census CPS 2015
setwd('/Users/mackfinkel/Documents/waste/')
blckGrp <- read.dbf('nhgis0001_shape/nhgis0001_shapefile_tl2015_us_blck_grp_2015/US_blck_grp_2015.dbf')

# get variables of interest
blckGrp <- blckGrp[,c("INTPTLAT", "INTPTLON", "ALAND", "Shape_Area", "GISJOIN")]

# merge census data
blckPop <- merge(blckGrp, popDataClean, by=c("GISJOIN","GISJOIN"))

# convert longitude and latitude from factors to numbers
blckPop$INTPTLON <- as.numeric(as.character(blckPop$INTPTLON))
blckPop$INTPTLAT <- as.numeric(as.character(blckPop$INTPTLAT))

blckPop$wasteProx <- 0

# cycle through facilities to determine waste and add to block level data
for (i in 1:length(TRIcleaner$ST)){
  print(i)
  
  # determine which facilities are less than 3 miles away from a waste producing facility
  if (is.na(TRIcleaner$LATITUDE[i]) | is.na(TRIcleaner$LONGITUDE[i])) next
  idx <- abs(TRIcleaner$LATITUDE[i] - blckPop$INTPTLAT) < 1 & abs(TRIcleaner$LONGITUDE[i] - blckPop$INTPTLON) < 1
  if (any(idx)==FALSE) next
  dist <- distm(blckPop[idx,c("INTPTLON", "INTPTLAT")], TRIcleaner[i,c("LONGITUDE", "LATITUDE")])
  dist <- (dist/1609.344)
  idx[which(idx == TRUE)] <- dist < 3
  
  # determine this facility's waste
  waste <- (TRIcleaner$ON.SITE_RELEASE_TOTAL[i] + TRIcleaner$X8.6_TREATMENT_ON.SITE[i])
  
  # multiply waste by proximity
  wasteProx <- waste*(3 - dist[dist < 3])
  
  # add this facility's contribution to already calculated waste for this census block group
  blckPop$wasteProx[idx] <- blckPop$wasteProx[idx] + wasteProx
  
}
# blckPop$biggestIdx <- NULL

#total population
blckPop$totalPop <- as.numeric(as.character(blckPop$ADKWE001))

# aggregate income of population
blckPop$income <- blckPop$ADOVE001

# index which block groups are near waste
blckPop$noWaste <- FALSE
blckPop$noWaste[blckPop$wasteProx == 0] <- TRUE

library(gridExtra)

#income ADOVE001
#convert income to income per person
blckPop$incomePop <- as.numeric(as.character(blckPop$income))/as.numeric(as.character(blckPop$totalPop))

#get income percentile
library(dplyr)

#get deciles
blckPop$incomeRounded <- ntile(blckPop$incomePop, 10)

library(plyr)
# get nearby waste values by income percentile
incomePollution <- ddply(blckPop[!is.na(blckPop$incomeRounded),], .(incomeRounded), summarize,
         wasteProxMean = mean(wasteProx, na.rm=TRUE),
         wasteNear = sum(!noWaste, na.rm = TRUE),
         incomeMedian = median(incomePop))

incomePollution$nearWasteWasteProxMean <- ddply(blckPop[!is.na(blckPop$incomeRounded) & !blckPop$noWaste,], .(incomeRounded), summarize,
                         nearWasteWasteProxMean = mean(wasteProx, na.rm=TRUE))$nearWasteWasteProxMean

library(highcharter)

#plot income and average nearby waste count
incomeAvgWaste <- highchart() %>%
  hc_add_series(incomePollution, type = "column", name = "Average 5-year waste (lbs)", hcaes(incomeRounded, wasteProxMean)) %>%
  hc_title(text = "More toxic animal waste in low-income areas") %>%
  hc_xAxis(title = list(text = "Income decile")) %>%
  hc_yAxis(title = list(text = "Average 5-year waste (lbs)")) %>%
  hc_tooltip(valueDecimals = 0, shape = "rectangle", shared = TRUE) %>%
  hc_legend(enabled = FALSE) %>%
  hc_credits(enabled = TRUE, text = "Source: CPS & TRI, 2011-2015", href = "https://www.epa.gov/toxics-release-inventory-tri-program/tri-basic-data-files-calendar-years-1987-2016")

#plot number of facilities by income
incomeNearWaste <- highchart() %>%
  hc_add_series(incomePollution, type = "column", name = "Communities near waste-producing facilities", hcaes(incomeRounded, wasteNear)) %>%
  hc_title(text = "Low-income areas near more polluting animal facilities") %>%
  hc_xAxis(title = list(text = "Income decile")) %>%
  hc_yAxis(title = list(text = "Block groups near polluting animal facilities")) %>%
  hc_tooltip(valueDecimals = 0, shape = "rectangle", shared = TRUE) %>%
  hc_legend(enabled = FALSE) %>%
  hc_credits(enabled = TRUE, text = "Source: CPS & TRI, 2011-2015", href = "https://www.epa.gov/toxics-release-inventory-tri-program/tri-basic-data-files-calendar-years-1987-2016")


# get linear regression fit and 95% CI
modelIncome <- lm(wasteProx ~ incomePop, data = blckPop)
modelSummary<- summary(modelIncome)
# F(1, 219124) = 130.6, p-value: < 2.2e-16




