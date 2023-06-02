library(SWMPr)
library(ggmap)

## qaqc, combine
wq <- qaqc(apadbwq)
met <- qaqc(apaebmet) 
nut <- qaqc(apacpnut)
dat <- comb(wq, met)

## estimate metabolism
resmet <- ecometab(dat)

swmpr_in <- subset(wq, rem_cols = TRUE)
