setwd('/home/lj/ML/Zillow/')
dat <- read.csv("Transactions.csv")

nrw <- nrow(dat)
extremeVals <- list()

region_ids <- group_by(dat, regionid)

means <- summarise(region_ids, mean(transvalue, na.rm=TRUE), mean(bathroomcnt, na.rm=TRUE), mean(bedroomcnt, na.rm=TRUE), mean(finishedsquarefeet, na.rm=TRUE), mean(lotsizesquarefeet, na.rm=TRUE))
sds <- summarise(region_ids, std(transvalue, na.rm=TRUE), std(bathroomcnt, na.rm=TRUE), std(bedroomcnt, na.rm=TRUE), std(finishedsquarefeet, na.rm=TRUE), std(lotsizesquarefeet, na.rm=TRUE))
