
# download and run r script that downloads the scf
download.file("https://raw.githubusercontent.com/mackfinkel/misc/master/econ/downloadSCF.R", fileName, mode="wb")
source('downloadSCF.R')

scf <- read.csv("scf.csv")[,-1]

# codebook for summary variables available at http://sda.berkeley.edu/sdaweb/docs/scfcomb/DOC/hcbk.htm

# use dplyr or some shit
# compress to year, race, education, median net worth, mean net worth, median income, mean income

scf$YEAR = as.numeric(scf$YEAR)

library(plyr)

# get race variables
scfRace <- ddply(scf, .(YEAR, RACE), summarize,
                incomeMedian = median(INCOME),
                incomeMean = mean(INCOME),
                wealthMedian = median(NETWORTH),
                wealthMean = mean(NETWORTH))

# calculate gaps
scfRace <- reshape(scfRace, idvar = "YEAR", timevar = "RACE", direction = "wide")
scfRace$incomeGap <- 100 - 100*scfRace$incomeMean.2/scfRace$incomeMean.1
scfRace$incomeGap <- 100 - 100*scfRace$incomeMean.2/scfRace$incomeMean.1
scfRace$wealthGap <- 100 - 100*scfRace$wealthMean.2/scfRace$wealthMean.1

scfEd <- scf
scfEd$EDUC[scf$EDUC < 8] <- 1
scfEd$EDUC[scf$EDUC >= 8 & scfEd$EDUC < 12] <- 2
scfEd$EDUC[scf$EDUC >= 12] <- 3

scfEducation <- ddply(scfEd, .(YEAR, EDUC), summarize,
                      incomeMedian = median(INCOME),
                      wealthMedian = median(NETWORTH))

scfEducation <- scfEducation[!is.na(scfEducation$EDUC),]

scfEducationCollege <- ddply(scf[scf$EDUC >= 12 & !is.na(scf$EDUC),], .(YEAR, RACE), summarize,
                      incomeMedian = median(INCOME),
                      incomeMean = mean(INCOME),
                      wealthMedian = median(NETWORTH),
                      wealthMean = mean(NETWORTH))

scfEducationHs <- ddply(scf[scf$EDUC == 8 & !is.na(scf$EDUC),], .(YEAR, RACE), summarize,
                          incomeMedian = median(INCOME),
                          incomeMean = mean(INCOME),
                          wealthMedian = median(NETWORTH),
                          wealthMean = mean(NETWORTH))


# EDCL 	
# Education category of head of household
# 1. no high school diploma/GED
# 2. high school diploma or GED
# 3. some college
# 4. college degree

# EDUC 	
# Highest completed grade by head of household
# -1 	LESS THAN 1ST GRADE
# 1 	1ST, 2ND, 3RD, OR 4TH GRADE
# 2 	5TH OR 6TH GRADE
# 3 	7TH OR 8TH GRADE
# 4 	9TH GRADE
# 5 	10TH GRADE
# 6 	11TH GRADE
# 7 	12TH GRADE, NO DIPLOMA
# 8 	HIGH SCHOOL GRADUATE - HIGH SCHOOL DIPLOMA OR EQUIVALENT
# 9 	SOME COLLEGE BUT NO DEGREE
# 10 	ASSOCIATE DEGREE IN COLLEGE - OCCUPATION/VOCATION PROGRAM
# 11 	ASSOCIATE DEGREE IN COLLEGE - ACADEMIC PROGRAM
# 12 	BACHELOR'S DEGREE (FOR EXAMPLE: BA, AB, BS)
# 13 	MASTER'S DEGREE
# 14 	DOCTORATE OR PROFESSIONAL SCHOOL DEGREE

# RACE 	
# Race/ethnicity of respondent
# 1 = white non-Hispanic
# 2 = black / African American
# 3 = Hispanic
# 5 = Other

# NETWORTH 	
# Total net worth of household, 2016 dollars
# The difference between assets and debt.

# INCOME 	
# Total amount of income of household, 2016 dollars
# Household income for previous calendar year.  Inlcudes wages, 
# self-employment and business income, taxable and tax-exempt 
# interest, dividends, realized capital gains, food stamps and 
# other support programs provided by the government, pension income 
# and withdrawals from retirement accounts, Social Security income, 
# alimony and other support payments, and miscellaneous sources of 
# income.

library(highcharter)

scfEducationIncomePlot <- highchart() %>%
  hc_add_series(scfEducation[scfEducation$EDUC == 3,], type = "line", name = "Bachelor's", hcaes(x = YEAR, y = incomeMedian)) %>%
  hc_add_series(scfEducation[scfEducation$EDUC == 2,], type = "line", name = "High School", hcaes(x = YEAR, y = incomeMedian)) %>%
  hc_add_series(scfEducation[scfEducation$EDUC == 1,], type = "line", name = "< High School", hcaes(x = YEAR, y = incomeMedian)) %>%
  hc_title(text = "More education, more income") %>%
  hc_xAxis(title = list(text = "Year")) %>%
  hc_yAxis(title = list(text = "Median Income (2016 USD)")) %>%
  hc_tooltip(valueDecimals = 0, shared = TRUE, shape = "rectangle") %>%
  hc_legend(enabled = TRUE) %>%
  hc_credits(enabled = TRUE, text = "SCF", href = "https://www.federalreserve.gov/econres/scfindex.htm") %>%
  hc_plotOptions(series = list(marker = list(enabled = TRUE)))

educationRaceIncome <- highchart() %>%
  hc_add_series(scfEducationCollege[scfEducationCollege$RACE == 1,], type = "line", name = "White, College", hcaes(x = YEAR, y = incomeMedian)) %>%
  hc_add_series(scfEducationCollege[scfEducationCollege$RACE == 2,], type = "line", name = "Black, College", hcaes(x = YEAR, y = incomeMedian)) %>%
  hc_add_series(scfEducationHs[scfEducationCollege$RACE == 1,], type = "line", name = "White, High School", hcaes(x = YEAR, y = incomeMedian)) %>%
  hc_add_series(scfEducationHs[scfEducationCollege$RACE == 2,], type = "line", name = "Black, High School", hcaes(x = YEAR, y = incomeMedian)) %>%
  hc_title(text = "Income disparity favors college educated whites") %>%
  hc_xAxis(title = list(text = "Year")) %>%
  hc_yAxis(title = list(text = "Median Wealth (2016 USD)")) %>%
  hc_tooltip(valueDecimals = 0, shape = "rectangle", shared = TRUE) %>%
  hc_legend(enabled = TRUE) %>%
  hc_credits(enabled = TRUE, text = "SCF", href = "https://www.federalreserve.gov/econres/scfindex.htm") %>%
  hc_plotOptions(series = list(marker = list(enabled = TRUE)))

educationRaceWealth <- highchart() %>%
  hc_add_series(scfEducationCollege[scfEducationCollege$RACE == 1,], type = "line", name = "White, College", hcaes(x = YEAR, y = wealthMedian)) %>%
  hc_add_series(scfEducationCollege[scfEducationCollege$RACE == 2,], type = "line", name = "Black, College", hcaes(x = YEAR, y = wealthMedian)) %>%
  hc_add_series(scfEducationHs[scfEducationCollege$RACE == 1,], type = "line", name = "White, High School", hcaes(x = YEAR, y = wealthMedian)) %>%
  hc_add_series(scfEducationHs[scfEducationCollege$RACE == 2,], type = "line", name = "Black, High School", hcaes(x = YEAR, y = wealthMedian)) %>%
  hc_title(text = "White college graduates are their own class") %>%
  hc_xAxis(title = list(text = "Year")) %>%
  hc_yAxis(title = list(text = "Median Wealth (2016 USD)")) %>%
  hc_tooltip(valueDecimals = 0, shape = "rectangle", shared = TRUE) %>%
  hc_legend(enabled = TRUE) %>%
  hc_credits(enabled = TRUE, text = "SCF", href = "https://www.federalreserve.gov/econres/scfindex.htm") %>%
  hc_plotOptions(series = list(marker = list(enabled = TRUE)))

scfRacePlot <- highchart() %>%
  hc_add_series(scfRace, type = "line", name = "Income Gap", hcaes(x = YEAR, y = incomeGap)) %>%
  hc_add_series(scfRace, type = "line", name = "Wealth Gap", hcaes(x = YEAR, y = wealthGap)) %>%
  hc_title(text = "Black-white economic gap consistent") %>%
  hc_xAxis(title = list(text = "Year")) %>%
  hc_yAxis(title = list(text = "Gap Size (%)"), min = 0, max = 100) %>%
  hc_tooltip(valueDecimals = 1, shared = TRUE, shape = "rectangle") %>%
  hc_legend(enabled = TRUE) %>%
  hc_credits(enabled = TRUE, text = "SCF", href = "https://www.federalreserve.gov/econres/scfindex.htm") %>%
  hc_plotOptions(series = list(marker = list(enabled = TRUE)))