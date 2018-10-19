#TRI animal ag waste by year

# download all TRI files from https://www.epa.gov/toxics-release-inventory-tri-program/tri-basic-data-files-calendar-years-1987-2016
setwd('/Users/mackfinkel/Documents/wasteYears/TRI')

fileNames <- list.files()
TRI <- data.frame()
for (i in 1:length(fileNames)){
  currentTRI <- read.csv(fileNames[i], stringsAsFactors = FALSE)
  # select for animal agriculture (NAICS prefix for animal ag is 112. exclude aquaculture at 1125)
  # NAICS code documentation available at https://www.census.gov/eos/www/naics/
  currentTRI <- currentTRI[substr(as.character(currentTRI$PRIMARY_NAICS), 1, 3) == "112" & substr(as.character(currentTRI$PRIMARY_NAICS), 1, 4) != "1125" | substr(as.character(currentTRI$PRIMARY_NAICS), 1, 4) == "3116",]
  
  currentTRI <- currentTRI[,c("YEAR", 
                              "FACILITY_NAME",
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


# sum facility waste by year and parent company
library(plyr)
TRIcleaner <- ddply(TRI, .(YEAR, PARENT_COMPANY_NAME), summarize,
                    waste = sum(waste, na.rm=TRUE))

#if not one of these, call "other"
TRIcleaner$PARENT_COMPANY_NAME <- as.character(TRIcleaner$PARENT_COMPANY_NAME)
TRIcleaner$PARENT_COMPANY_NAME <- sub("[.]", "", TRIcleaner$PARENT_COMPANY_NAME)
TRIcleaner$PARENT_COMPANY_NAME[TRIcleaner$PARENT_COMPANY_NAME == "CARGILL INC  PARAMOUNT PRODUCTS DIV"] <- "CARGILL INC"
TRIcleaner$PARENT_COMPANY_NAME[grepl("CARGILL", TRIcleaner$PARENT_COMPANY_NAME)] <- "CARGILL"
TRIcleaner$PARENT_COMPANY_NAME[grepl("TYSON", TRIcleaner$PARENT_COMPANY_NAME)] <- "TYSON"
TRIcleaner$PARENT_COMPANY_NAME[grepl("JBS", TRIcleaner$PARENT_COMPANY_NAME)] <- "JBS"

arrange(TRIcleaner, desc(waste))

#464 unique parent companies
length(unique(TRIcleaner$PARENT_COMPANY_NAME))

companies <- ddply(TRIcleaner, .(PARENT_COMPANY_NAME), summarize,
                   waste = sum(waste, na.rm=TRUE))
companies <- arrange(companies, desc(waste))

#get names of top 6 companies
companies <- companies$PARENT_COMPANY_NAME[!is.na(companies$PARENT_COMPANY_NAME)][1:3]

# also remove "springdale"
TRIcleaner$PARENT_COMPANY_NAME[grepl("SPRINGDALE", TRIcleaner$PARENT_COMPANY_NAME) | !grepl(gsub(", ", "|", toString(as.character(companies))), TRIcleaner$PARENT_COMPANY_NAME)] <- "OTHER"

TRIcleaner <- ddply(TRIcleaner, .(YEAR, PARENT_COMPANY_NAME), summarize,
                    waste = sum(waste, na.rm=TRUE))

TRIcleaner <- TRIcleaner[TRIcleaner$waste > 0,]

#capitalize 1st words
simpleCap <- function(x) {
  s <- strsplit(x, " ")[[1]]
  paste(toupper(substring(s, 1,1)), substring(s, 2),
        sep="", collapse=" ")
}

#fix capitalization
TRIcleaner$PARENT_COMPANY_NAME <- as.character(sapply(tolower(TRIcleaner$PARENT_COMPANY_NAME), simpleCap))
TRIcleaner$PARENT_COMPANY_NAME <- gsub("Jbs", "JBS", TRIcleaner$PARENT_COMPANY_NAME)
TRIcleaner$PARENT_COMPANY_NAME <- gsub("Usa|Us", "USA", TRIcleaner$PARENT_COMPANY_NAME)

library(highcharter)

#make chart
wastePlot <- highchart() %>%
  hc_add_series(TRIcleaner, type = "area", stacking = "normal", hcaes(x = YEAR, y = round(waste), group = PARENT_COMPANY_NAME)) %>%
  hc_title(text = "Large Growth in Animal Agriculture Waste") %>%
  hc_xAxis(title = list(text = "Year")) %>%
  hc_yAxis(title = list(text = "Waste (lbs)")) %>%
  hc_tooltip(valueDecimals = 0, shape = "rectangle", shared = TRUE) %>%
  hc_legend(align = "left", verticalAlign = "top", layout = "vertical", floating = "true", x = 85, y = 33) %>%
  hc_credits(enabled = TRUE, text = "Source: Toxics Release Inventory", href = "https://www.epa.gov/toxics-release-inventory-tri-program") %>%
  hc_plotOptions(series = list(marker = list(enabled = FALSE)))


