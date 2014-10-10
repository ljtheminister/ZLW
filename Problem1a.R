library(dplyr)
library(data.table)

# Set working directory and read data 
setwd('/home/lj/ML/Zillow/')
dat <- read.csv("Transactions.csv")

nrw <- nrow(dat)

# Get median and standard deviations by regionid
data_by_region <- group_by(dat, regionid)
medians <- summarise(data_by_region, transvalue=median(transvalue, na.rm=TRUE), bathroomcnt=median(bathroomcnt, na.rm=TRUE), bedroomcnt=median(bedroomcnt, na.rm=TRUE), finishedsquarefeet=median(finishedsquarefeet, na.rm=TRUE), lotsizesquarefeet=median(lotsizesquarefeet, na.rm=TRUE))
sds <- summarise(data_by_region, transvalue=sd(transvalue, na.rm=TRUE), bathroomcnt=sd(bathroomcnt, na.rm=TRUE), bedroomcnt=sd(bedroomcnt, na.rm=TRUE), finishedsquarefeet=sd(finishedsquarefeet, na.rm=TRUE), lotsizesquarefeet=sd(lotsizesquarefeet, na.rm=TRUE))
rownames(medians) <- medians$regionid
rownames(sds) <- sds$regionid
medians <- medians[, 2:6]
sds <- sds[,2:6]
col_names <- c('transvalue', 'bathroomcnt', 'bedroomcnt', 'finishedsquarefeet', 'lotsizesquarefeet')

# Check for Extreme Values
dat$isExtreme=FALSE # set everything to FALSE by default

# checkExtreme: a function that checks a row whether one of its values is extreme aka more than 2 standard deviations away
checkExtreme <- function(x) {  
  regionid <- as.character(x['regionid'])
  diff <- abs(x[col_names] - medians[regionid,])
  extreme = diff > 2*sds[regionid,]
  if (any(is.na(extreme))) {
    x$isExtreme <- NA
  }
  else if (any(extreme==TRUE)) {
    x$isExtreme <- TRUE
  }
  return(x)
}

#apply(dat, 1, checkExtreme) Apply doesn't work here, so I use the for loop
for (i in 1:nrw) {
  dat[i,] <- checkExtreme(dat[i,])
}

write.csv(dat[,c('propertyid', 'isExtreme')], file="yourPathHere2")

## TESTING TO SEE IT'S RIGHT
if (FALSE) {
r <- list()
for (i in 1:length(diff)) {
  if (is.na(diff[i])) {
    r <- c(r, i)
  }
  else if (diff[i]==FALSE) {
    r <- c(r, i)
  }
}

r <- list()

for (i in 1:length(diff)) {
  if (!is.na(diff[i]) & diff[i]!=TRUE) {
    print (i)
    r <- c(r, i)
    
  }
}

for (i in 1:length(r)) {
i = 1
  idx <- as.numeric(r[i])
  print (dat[idx,])
  print (a[idx,])
i = i + 1
}
regionid <- as.character(dat[idx, 'regionid'])
difference <- abs(d[col_names] - medians[regionid,])

d$transvalue > medians[regionid, 'transvalue'] + 2*sds[regionid, 'transvalue']
d$bathroomcnt > medians[regionid, 'bathroomcnt'] + 2*sds[regionid, 'bathroomcnt']
d$bedroomcnt > medians[regionid, 'bedroomcnt'] + 2*sds[regionid, 'bedroomcnt']
d$finishedsquarefeet > medians[regionid, 'finishedsquarefeet'] + 2*sds[regionid, 'finishedsquarefeet']
d$lotsizesquarefeet > medians[regionid, 'lotsizesquarefeet'] + 2*sds[regionid, 'lotsizesquarefeet']

d$transvalue < medians[regionid, 'transvalue'] - 2*sds[regionid, 'transvalue']
d$bathroomcnt < medians[regionid, 'bathroomcnt'] - 2*sds[regionid, 'bathroomcnt']
d$bedroomcnt < medians[regionid, 'bedroomcnt'] - 2*sds[regionid, 'bedroomcnt']
d$finishedsquarefeet < medians[regionid, 'finishedsquarefeet'] - 2*sds[regionid, 'finishedsquarefeet']
d$lotsizesquarefeet < medians[regionid, 'lotsizesquarefeet'] - 2*sds[regionid, 'lotsizesquarefeet']
}
