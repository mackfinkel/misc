# download and run r script that downloads the scf
download.file("https://raw.githubusercontent.com/mackfinkel/misc/master/econ/downloadSCF.R", fileName, mode="wb")
source('downloadSCF.R')

scf <- read.csv("scf.csv")[,-1]

# codebook for summary variables available at http://sda.berkeley.edu/sdaweb/docs/scfcomb/DOC/hcbk.htm

# percentage of black-white wealth disparity accounted for in real estate
# RACE 	
# Race/ethnicity of respondent
# 1 = white non-Hispanic
# 2 = black / African American
# 3 = Hispanic
# 5 = Other

library(plyr)
library(highcharter)

scfRaceEdLoans2016 <- ddply(scfEd[scf$YEAR == 2016,], .(RACE), summarize,
                            wealthMedian = median(NETWORTH),
                            edLoanMedian = median(EDN_INST),
                            wealthNoEdLoanMedian = median(NETWORTH+EDN_INST))

scfRaceEdLoans2016$RACE <- as.character(scfRaceEdLoans2016$RACE)
scfRaceEdLoans2016$RACE[scfRaceEdLoans2016$RACE == 1] <- "1"
scfRaceEdLoans2016$RACE[scfRaceEdLoans2016$RACE == 2] <- "2"
scfRaceEdLoans2016$RACE[scfRaceEdLoans2016$RACE == 3] <- "3"

scfRaceEdLoans2016$wealthEdLoanDiff <- 100*scfRaceEdLoans2016$wealthNoEdLoanMedian/scfRaceEdLoans2016$wealthMedian-100

noEdLoansMedianIncrease <- highchart() %>%
  hc_add_series(scfRaceEdLoans2016[scfRaceEdLoans2016$RACE != 5,], type = "column", name = "Wealth Change", hcaes(x = c("White", "Black", "Hispanic"), y = wealthNoEdLoanMedian - wealthMedian )) %>%
  hc_title(text = "Loan forgiveness increases all median wealth") %>%
  hc_xAxis(title = list(text = "Race"), categories = c("White", "Black", "Hispanic")) %>%
  hc_yAxis(title = list(text = "Median Wealth Change (2016 USD)")) %>%
  hc_tooltip(shared = TRUE, valueDecimals = 0, shape = "rectangle", pointFormat = '<b>${point.y}</b>') %>%
  hc_legend(enabled = FALSE) %>%
  hc_credits(enabled = TRUE, text = "Survey of Consumer Finances 2016", href = "https://www.federalreserve.gov/econres/scfindex.htm") %>%
  hc_plotOptions(series = list(marker = list(enabled = TRUE)))

noEdLoansPctIncrease <- highchart() %>%
  hc_add_series(scfRaceEdLoans2016[scfRaceEdLoans2016$RACE != 5,], type = "column", name = "Wealth Change", hcaes(x = c("White", "Black", "Hispanic"), y = wealthEdLoanDiff)) %>%
  hc_title(text = "Loan forgiveness benefits minorities") %>%
  hc_xAxis(title = list(text = "Race"), categories = c("White", "Black", "Hispanic")) %>%
  hc_yAxis(title = list(text = "Median Wealth Change (%)")) %>%
  hc_tooltip(shared = TRUE, valueDecimals = 1, shape = "rectangle", pointFormat = '<b>{point.y}%</b>') %>%
  hc_legend(enabled = FALSE) %>%
  hc_credits(enabled = TRUE, text = "Survey of Consumer Finances 2016", href = "https://www.federalreserve.gov/econres/scfindex.htm") %>%
  hc_plotOptions(series = list(marker = list(enabled = TRUE)))

# repare to get gap data
wealthGaps <- reshape(scfRaceEdLoans2016, timevar = "RACE", idvar = "edLoanMedian", direction = "wide")

# get gap data
wealthGaps$blackChange <- 100*(wealthGaps$wealthNoEdLoanMedian.2/wealthGaps$wealthNoEdLoanMedian.1 - wealthGaps$wealthMedian.2/wealthGaps$wealthMedian.1)
wealthGaps$hispanicChange <- 100*(wealthGaps$wealthNoEdLoanMedian.3/wealthGaps$wealthNoEdLoanMedian.1 - wealthGaps$wealthMedian.3/wealthGaps$wealthMedian.1)

# prepare gap data to chart
gapData <- data.frame(RACE = c("Black", "Hispanic"), GAP = c(wealthGaps$blackChange,wealthGaps$hispanicChange))

noEdLoansWealthGap <- highchart() %>%
  hc_add_series(gapData, type = "column", name = "", hcaes(x = RACE, y = GAP)) %>%
  hc_title(text = "Loan forgiveness barely reduces wealth inequality") %>%
  hc_xAxis(title = list(text = "Race"), categories = c("Black", "Hispanic")) %>%
  hc_yAxis(title = list(text = "Change in wealth relative to white wealth (%)"), min = 0, max = 100) %>%
  hc_tooltip(shared = TRUE, valueDecimals = 1, shape = "rectangle", pointFormat = '<b>{point.y}%</b>') %>%
  hc_legend(enabled = FALSE) %>%
  hc_credits(enabled = TRUE, text = "Survey of Consumer Finances 2016", href = "https://www.federalreserve.gov/econres/scfindex.htm") %>%
  hc_plotOptions(series = list(marker = list(enabled = TRUE)))
