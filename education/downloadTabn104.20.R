# 1. Download "Table 104.20. Percentage of persons 25 to 29 years old with selected levels of educational attainment, by race/ethnicity and sex: Selected years, 1920 through 2017" from internet
# 2. Clean data
# 3. Save as .csv
# Data viewable at https://nces.ed.gov/programs/digest/d17/tables/dt17_104.20.asp

# download and read file from internet
require(gdata)

# create directory to download file
newDir <- "educationTemp"
dir.create(newDir)
setwd(newDir)

# download and load file
url <- "https://nces.ed.gov/programs/digest/d17/tables/xls/tabn104.20.xls"
download.file(url, 'tabn104.20.xls', mode="wb")
tabn104.20 <- data.frame(read.xls('tabn104.20.xls'))

# convert to characters
tabn104.20[] <- lapply(tabn104.20, as.character)
#tabn104.20 <-  sapply(tabn104.20, function(x) as.character(x))

# remove NA columns, empty columns
tabn104.20 <- data.frame(lapply(tabn104.20, function(x) {gsub("!|†|---|‡|[(]|[)]", "", x)}))

# tabn104.20 <- gsub("!|†|---|‡|[(]|[)]", "", tabn104.20)
tabn104.20[tabn104.20 == ""] <- NA
tabn104.20 <-  tabn104.20[,colSums(is.na(tabn104.20))<nrow(tabn104.20)]

# remove footnotes
tabn104.20 <- data.frame(lapply(tabn104.20, function(x) {gsub("\\\\", "_", x)}))
tabn104.20 <- data.frame(lapply(tabn104.20, function(x) {gsub("_[[:digit:]]_", "", x)}))
#tabn104.20 <- gsub("\\\\", "_", tabn104.20)
#tabn104.20 <- gsub("_[[:digit:]]_", "", tabn104.20)


# get years isolated in column 1
tabn104.20[,1] <- as.character(tabn104.20[,1])
tabn104.20[,1][grepl("[[:digit:]]{4}", tabn104.20[,1])] <- gsub("[.]|[[:space:]]", "", tabn104.20[,1][grepl("[[:digit:]]{4}", tabn104.20[,1])])


# remove first row containing parenthetical note
tabn104.20 <- tabn104.20[-1,]

# label first column as "year"
tabn104.20[1,1] <- "Year"

# combine labeling rows
tabn104.20[] <- lapply(tabn104.20, as.character)
tabn104.20[1:2,][is.na(tabn104.20[1:2,])] <- ""
tabn104.20[1,] <- paste0(tabn104.20[1,], tabn104.20[2,])
tabn104.20 <- tabn104.20[-2,]

# remove useless numbered row
tabn104.20 <- tabn104.20[-2,]

# label standard error columns (the empty columns in the first row)
tabn104.20[1,][tabn104.20[1,] == ""] <- "SE"

# make row 1 column names, convert to data frame
colnames(tabn104.20) <- tabn104.20[1,]
tabn104.20 <- tabn104.20[-1,]

# regulate factors to numbers
tabn104.20[,-1] <- lapply(tabn104.20[,-1], as.double)

# set "group" and "education" to convert column 1 categories to their own columns
tabn104.20$Group <- NA
tabn104.20$Education <- NA
tabn104.20[,1] <- as.character(tabn104.20[,1])
nonYears <- nchar(tabn104.20[,1]) != 4 & !grepl("[[:digit:]]{4}", tabn104.20[,1])
nonYearsIdx <- which(nonYears)
group <- tabn104.20[nonYearsIdx[1],1]
groupIdx <- nonYearsIdx[1]
education <- tabn104.20[nonYearsIdx[2],1]
educationIdx <- nonYearsIdx[2]
for (i in nonYearsIdx[3:length(nonYearsIdx)]) {
  if (nonYears[i + 1] & !nonYears[i+2]) {
    tabn104.20$Group[groupIdx:i-1] <- group
    group <- tabn104.20[i,1]
    groupIdx <- i+3
  } else if (!nonYears[i + 1] & !nonYears[i+2]){
    tabn104.20$Education[educationIdx:i-1] <- education
    education <- tabn104.20[i,1]
    educationIdx <- i+2
  }
}

# remove non-year columns
tabn104.20 <- tabn104.20[nchar(tabn104.20[,1]) == 4,]

# save file
write.csv(tabn104.20, paste0(as.character(substitute(tabn104.20)), '.csv'))
