

dir.create("scf")
setwd('scf/')

# Using years that SCF data exists in excel format (since 1989)
# All dollar variables have been inflation-adjusted to 2016 dollars.
years <- as.character(seq(1989, 2016, 3))
scfURLs <- paste0("https://www.federalreserve.gov/econres/files/scfp", years, "excel.zip")
slashIdx <- data.frame(gregexpr("/", scfURLs))

scf <- data.frame()

library(plyr)

# Download and combine them
for (i in 1:length(scfURLs)){
  fileName <- substring(scfURLs[i],max(slashIdx[i]+1))
  download.file(scfURLs[i], fileName, mode="wb")
  unzip(paste0('scfp', years[i], 'excel.zip'))
  
  # Note: command line tool is from app Gnumeric, installed through brew on macOS. Used because 
  system(paste0("ssconvert SCFP", years[i], ".xlsx SCFP", years[i], ".csv"))
  
  scfTemp <- read.csv(paste0("SCFP", years[i], ".csv"))
  scfTemp$YEAR <- years[i]
  scf <- rbind.fill(scf, scfTemp)
}

# Save
write.csv(scf, "scf.csv")

# codebook for summary variables available at http://sda.berkeley.edu/sdaweb/docs/scfcomb/DOC/hcbk.htm