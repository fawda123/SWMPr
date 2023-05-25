library(SWMPr)

## import water quality and weather datadata(apadbwq)
data(apaebmet)

## qaqc, combine
wq <- qaqc(apadbwq)
met <- qaqc(apaebmet)
dat <- comb(wq, met)

## estimate metabolism
resmet <- ecometab(dat)
