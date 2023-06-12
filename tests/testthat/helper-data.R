library(SWMPr)
library(ggmap)

## qaqc, combine
wq <- qaqc(apadbwq)
met <- qaqc(apaebmet) 
nut <- qaqc(apacpnut)
dat <- comb(wq, met)

# for decomp tests
dcmdat <- subset(apadbwq, subset = c('2013-07-01 00:00', '2013-07-31 00:00'))

## estimate metabolism
resmet <- ecometab(dat)
resmetgrms <- ecometab(dat, metab_units = 'grams')

swmpr_in <- subset(wq, rem_cols = TRUE)
