library(dplyr)
library(data.table)

setwd('/home/lj/ML/Zillow/')
dat <- read.csv("Transactions.csv")

nrw <- nrow(dat)
extremeVals <- list()

data_by_region <- group_by(dat, regionid)
means <- summarise(data_by_region, transvalue=mean(transvalue, na.rm=TRUE), bathroomcnt=mean(bathroomcnt, na.rm=TRUE), bedroomcnt=mean(bedroomcnt, na.rm=TRUE), finishedsquarefeet=mean(finishedsquarefeet, na.rm=TRUE), lotsizesquarefeet=mean(lotsizesquarefeet, na.rm=TRUE))
sds <- summarise(data_by_region, transvalue=sd(transvalue, na.rm=TRUE), bathroomcnt=sd(bathroomcnt, na.rm=TRUE), bedroomcnt=sd(bedroomcnt, na.rm=TRUE), finishedsquarefeet=sd(finishedsquarefeet, na.rm=TRUE), lotsizesquarefeet=sd(lotsizesquarefeet, na.rm=TRUE))
rownames(means) <- means$regionid
rownames(sds) <- sds$regionid
means <- means[, 2:6]
sds <- sds[,2:6]
col_names <- c('transvalue', 'bathroomcnt', 'bedroomcnt', 'finishedsquarefeet', 'lotsizesquarefeet')

dat$isExtreme=FALSE

checkExtreme <- function(x) {  
  regionid <- as.character(x['regionid'])
  diff <- abs(x[col_names] - means[regionid,])
  extreme = diff > 2*sds[regionid,]
  if (any(extreme==TRUE)) {
    x$isExtreme = TRUE
  }
}

checkExtreme(tmp)

apply(dat, 1, checkExtreme)

