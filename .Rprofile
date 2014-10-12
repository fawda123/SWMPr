# library path
.libPaths('C:\\Users\\mbeck\\R\\library')

# startup message
cat('SWMPr...\n')

# packages to use
# install.packages("SSOAP", repos="http://www.omegahat.org/R", dependencies = T, 
#   type =  "source")
suppressPackageStartupMessages({
  library(SSOAP, quietly = T)
  library(XML, quietly = T)
  library(ggplot2, quietly = T)
  library(plyr, quietly = T)
  library(data.table, quietly = T)
  library(devtools, quietly = T)
})

# load SWMPr package
load_all()
library(SWMPr)

# ggplot theme set
theme_set(theme_bw())