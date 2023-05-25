library(SWMPr)

## qaqc, combine
wq <- qaqc(apadbwq)
met <- qaqc(apaebmet)
dat <- comb(wq, met)

## estimate metabolism
resmet <- ecometab(dat)

swmpr_in <- subset(wq, rem_cols = TRUE)
