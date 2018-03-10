# get filename
url <- "https://raw.githubusercontent.com/mackfinkel/misc/master/education/downloadTabn104.20.R"
slashIdx <- gregexpr("/", url)[[1]]
fileName <- substring(url, slashIdx[length(slashIdx)]+1)

# download and run
download.file(url, fileName, mode="wb")
source('downloadTabn104.20.R')
rowsCollege <- tabn104.20$Group == "Total" & tabn104.20$Education == "  Bachelor's or higher \n     degree"
rowsHighSchool <-  tabn104.20$Group == "Total" & tabn104.20$Education == "  High school completion\n     or higher"

# get college stats, calculate gap
collegeBlackWhite <- tabn104.20[rowsCollege,c("Year", "Black", "White")]
collegeBlackWhite$Gap <- 100 - 100*collegeBlackWhite$Black/collegeBlackWhite$White

# get high school stats, calculate gap
hsBlackWhite <- tabn104.20[rowsHighSchool,c("Year", "Black", "White")]
hsBlackWhite$Gap <- 100 - 100*hsBlackWhite$Black/hsBlackWhite$White

#make charts
require(highcharter)

collegeGapPlot <- highchart() %>%
  hc_add_series(collegeBlackWhite, type = "line", color = "#434348", name = "Gap", hcaes(x = as.numeric(Year), y = Gap)) %>%
  hc_title(text = "Stagnant race gap in college graduation") %>%
  hc_xAxis(title = list(text = "Year")) %>%
  hc_yAxis(title = list(text = "Gap Size (%)"), min = 0, max = 100) %>%
  hc_tooltip(valueDecimals = 1, shape = "rectangle", pointFormat = '<b>{point.y}%</b>') %>%
  hc_legend(enabled = FALSE) %>%
  hc_credits(enabled = TRUE, text = "NCES", href = "https://nces.ed.gov/programs/digest/d17/tables/dt17_104.20.asp") %>%
  hc_plotOptions(series = list(marker = list(enabled = TRUE)))

collegeGradRatesPlot <- highchart() %>%
  hc_add_series(collegeBlackWhite, type = "line", name = "White", hcaes(x = as.numeric(Year), y = White)) %>%
  hc_add_series(collegeBlackWhite, type = "line", name = "Black", hcaes(x = as.numeric(Year), y = Black)) %>%
  hc_title(text = "Black-White Rates in College Graduation") %>%
  hc_xAxis(title = list(text = "Year")) %>%
  hc_yAxis(title = list(text = "Graduation Rate (%)"), min = 0, max = 50) %>%
  hc_tooltip(valueDecimals = 1, shape = "rectangle", shared = TRUE) %>%
  hc_legend(enabled = TRUE) %>%
  hc_credits(enabled = TRUE, text = "NCES", href = "https://nces.ed.gov/programs/digest/d17/tables/dt17_104.20.asp") %>%
  hc_plotOptions(series = list(marker = list(enabled = TRUE)))

hsGapPlot <- highchart() %>%
  hc_add_series(hsBlackWhite, type = "line", color = "#434348", name = "Gap", hcaes(x = as.numeric(Year), y = Gap)) %>%
  hc_title(text = "Decreasing black-white gap in high school graduation") %>%
  hc_xAxis(title = list(text = "Year")) %>%
  hc_yAxis(title = list(text = "Gap Size (%)"), min = 0, max = 100) %>%
  hc_tooltip(valueDecimals = 1, shape = "rectangle", pointFormat = '<b>{point.y}%</b>') %>%
  hc_legend(enabled = FALSE) %>%
  hc_credits(enabled = TRUE, text = "NCES", href = "https://nces.ed.gov/programs/digest/d17/tables/dt17_104.20.asp") %>%
  hc_plotOptions(series = list(marker = list(enabled = TRUE)))

hsGradRatesPlot <- highchart() %>%
  hc_add_series(hsBlackWhite, type = "line", name = "White", hcaes(x = as.numeric(Year), y = White)) %>%
  hc_add_series(hsBlackWhite, type = "line", name = "Black", hcaes(x = as.numeric(Year), y = Black)) %>%
  hc_title(text = "Black-white rates of high school graduation") %>%
  hc_xAxis(title = list(text = "Year")) %>%
  hc_yAxis(title = list(text = "Graduation Rate (%)"), min = 0, max = 100) %>%
  hc_tooltip(valueDecimals = 1, shape = "rectangle", shared = TRUE) %>%
  hc_legend(enabled = TRUE) %>%
  hc_credits(enabled = TRUE, text = "NCES", href = "https://nces.ed.gov/programs/digest/d17/tables/dt17_104.20.asp") %>%
  hc_plotOptions(series = list(marker = list(enabled = TRUE)))

# scf RACE EDUC NETWORTH EDCL