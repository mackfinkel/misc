library(plyr)
library(reshape2)

setwd('Documents/agricultureData/')
#get biomasses and save as "weightData.csv"
#https://quickstats.nass.usda.gov/DEF4FCD8-72BF-330E-9FCD-86F5424C7A2F?long_desc__LIKE=MEASURED+IN+LB+%2F+HEAD%2C+LIVE+BASIS#D4C61356-F28E-3385-BDDB-1D8DD60342BA
#https://quickstats.nass.usda.gov/results/D4C61356-F28E-3385-BDDB-1D8DD60342BA

weight <- read.csv('weightData.csv')
weight<-weight[!grepl("CALVES",weight$Data.Item) & grepl("MEASURED IN LB / HEAD, LIVE BASIS",weight$Data.Item) & weight$Period == "YEAR",]
weight <- weight[,c("Commodity", "Data.Item", "Value")]
weight$Value <- as.numeric(gsub(",", "", weight$Value))
weight <- dcast(weight, Commodity ~ Data.Item, value.var = "Value")
weight <- data.frame(animal = weight$Commodity,
           mean = apply(weight[-1], 1, FUN=mean, na.rm=TRUE),
           min = apply(weight[-1], 1, FUN=min, na.rm=TRUE),
           max = apply(weight[-1], 1, FUN=max, na.rm=TRUE))

weightUse <- data.frame(t(weight[-1]))
names(weightUse) <- weight[,1]

#get area of county
#LND110210D - land area in square miles as of 2010
#variable specified in http://www2.census.gov/prod2/statcomp/usac/excel/Mastdata.xls
#download http://www2.census.gov/prod2/statcomp/usac/excel/LND01.xls

library(gdata)
countyArea <- read.xls("LND01.xls", sheet = 1, header = TRUE)
#isolate square miles of land as of 2010
countyArea <- countyArea[,c("Areaname", "STCOU", "LND110210D")]
#STCOU = STATE ANSI + COUNTY ANSI
countyArea$STCOU <- sprintf("%05s",countyArea$STCOU)

#get animals per county
#hogs sheep cows goats 
#https://quickstats.nass.usda.gov/#F5EF8E5B-2E38-3739-996D-295035F57121
#https://quickstats.nass.usda.gov/results/F5EF8E5B-2E38-3739-996D-295035F57121
CattleGoatsHogsSheep <- read.csv('inventory/CattleGoatsHogsSheep.csv', colClasses = "character")
CattleGoatsHogsSheep$Value <- as.numeric(gsub(",","",as.character(CattleGoatsHogsSheep$Value)))
CattleGoatsHogsSheep <- CattleGoatsHogsSheep[,c("County", "State", "State.ANSI", "County.ANSI", "Commodity", "Value")]

#turkey chicken duck
#https://quickstats.nass.usda.gov/#D294537D-4800-3599-8ED1-C15EF1170606
#https://quickstats.nass.usda.gov/results/D294537D-4800-3599-8ED1-C15EF1170606
ChickenTurkeyDuck <- read.csv('inventory/ChickenTurkeyDuck.csv', colClasses = "character")
ChickenTurkeyDuck$Value <- as.numeric(gsub(",","",as.character(ChickenTurkeyDuck$Value)))
ChickenTurkeyDuck <- ddply(ChickenTurkeyDuck, .(County, State, State.ANSI, County.ANSI, Commodity), summarize, Value = sum(Value, na.rm=TRUE))

all <- rbind(CattleGoatsHogsSheep, ChickenTurkeyDuck)
all <- dcast(all, County + State + State.ANSI + County.ANSI ~ Commodity, value.var = "Value")

all$STCOU <- paste(all$State.ANSI, all$County.ANSI, sep = "")
all$State.ANSI <- NULL
all$County.ANSI <- NULL

countyAll <- merge(all, countyArea, by=c("STCOU","STCOU"))

countyAll$CATTLEWeightAvg <- countyAll$CATTLE * weightUse$CATTLE[1]
countyAll$CATTLEWeightMin <- countyAll$CATTLE * weightUse$CATTLE[2]
countyAll$CATTLEWeightMax <- countyAll$CATTLE * weightUse$CATTLE[3]

countyAll$CHICKENSWeightAvg <- countyAll$CHICKENS * weightUse$CHICKENS[1]
countyAll$CHICKENSWeightMin <- countyAll$CHICKENS * weightUse$CHICKENS[2]
countyAll$CHICKENSWeightMax <- countyAll$CHICKENS * weightUse$CHICKENS[3]

countyAll$DUCKSWeightAvg <- countyAll$DUCKS * weightUse$DUCKS[1]
countyAll$DUCKSWeightMin <- countyAll$DUCKS * weightUse$DUCKS[2]
countyAll$DUCKSWeightMax <- countyAll$DUCKS * weightUse$DUCKS[3]

countyAll$GOATSWeightAvg <- countyAll$GOATS * weightUse$GOATS[1]
countyAll$GOATSWeightMin <- countyAll$GOATS * weightUse$GOATS[2]
countyAll$GOATSWeightMax <- countyAll$GOATS * weightUse$GOATS[3]

countyAll$HOGSWeightAvg <- countyAll$HOGS * weightUse$HOGS[1]
countyAll$HOGSWeightMin <- countyAll$HOGS * weightUse$HOGS[2]
countyAll$HOGSWeightMax <- countyAll$HOGS * weightUse$HOGS[3]

countyAll$SHEEPWeightAvg <- countyAll$SHEEP * weightUse$SHEEP[1]
countyAll$SHEEPWeightMin <- countyAll$SHEEP * weightUse$SHEEP[2]
countyAll$SHEEPWeightMax <- countyAll$SHEEP * weightUse$SHEEP[3]

countyAll$TURKEYSWeightAvg <- countyAll$TURKEYS * weightUse$TURKEYS[1]
countyAll$TURKEYSWeightMin <- countyAll$TURKEYS * weightUse$TURKEYS[2]
countyAll$TURKEYSWeightMax <- countyAll$TURKEYS * weightUse$TURKEYS[3]

countyAll$AllWeightAvg <- rowSums(countyAll[,grepl("Avg", colnames(countyAll))], na.rm = TRUE)
countyAll$AllWeightMin <- rowSums(countyAll[,grepl("Min", colnames(countyAll))], na.rm = TRUE)
countyAll$AllWeightMax <- rowSums(countyAll[,grepl("Max", colnames(countyAll))], na.rm = TRUE)

countyAll$lbsPerSqMile <- countyAll$AllWeightAvg/countyAll$LND110210D

library(highcharter)

data("usgeojson")

countyAll$Areaname <- as.character(countyAll$Areaname)

chart <- highchart() %>%
  hc_title(text = "Counties by Animal Density Percentile") %>%
  hc_add_series_map(map = uscountygeojson, df = countyAll,
                    value = "lbsPerSqMilePctile", joinBy = c("fips", "STCOU"),
                    dataLabels = list(enabled = TRUE,
                                      format = '{point.properties.postalcode}')) %>%
  hc_colorAxis(stops = color_stops()) %>%
  hc_legend(valueDecimals = 0, valueSuffix = "%") %>%
  hc_mapNavigation(enabled = TRUE) %>%
  hc_tooltip(valueDecimals = 0,
             headerFormat = "",
             pointFormat = '<small>{point.Areaname}</small><br/><tr><td>Percentile: </td><td><b>{point.lbsPerSqMilePctile:.2f}</b>%<br/><td>Cattle: </td><td><b>{point.CATTLE}</b><br/><td>Hogs: </td><td><b>{point.HOGS}</b><br/><td>Chickens: </td><td><b>{point.CHICKENS}</b><br/><td>Goats: </td><td><b>{point.GOATS}</b><br/><td>Turkeys: </td><td><b>{point.TURKEYS}</b></td><br/></tr>')

