# Problem 1: 
#  A) Summarize what this code is doing in a comment
#  B) Improve the code. 'Improvement' may be defined in a number of ways: code clarity, speed improvement, improved methodology, bug fixes, etc.
#  C) Document and explain your changes (May be done in comments throughout your code or in comment at end of code)


########################################
# (A) Summary of code's functionality #
########################################

#For each observation/row, the code checks to see if any of the following values - sale amount, bathroom count, bedroom count, finished square feet, and lot size in square feet - are 'extreme values', which is defined as being more than 2 standard deviations away from the respective means.

#For each observation, the code writes out row number, propertyid, and TRUE/FALSE depending on whether one of the values is or is not extreme.

#My code changes involves vectorizing the computation of the median and standard deviation and fixing the bug with respect to the saving of the isExtreme variable using extremeProps and writing the code more concisely with the use of less for loops.

###############################
dat <- read.csv("Transactions.csv")

nrw <- nrow(dat)
extremeVals <- list()


region_ids <- group_by(dat, regionid)
length(region_ids)

for (i in 1:nrw) {
  temp <- dat[i, ]
  region <- temp$regionid
  
  saleAmt <- temp$transvalue
  bathroomCnt <- temp$bathroomcnt
  bedroomCnt <- temp$bedroomcnt
  finishedSqrFeet <- temp$finishedsquarefeet
  lotSizeSquareFeet <- temp$lotsizesquarefeet
  
  nm1 <- paste0(region, '-1')
  nm2 <- paste0(region, '-2')
  nm3 <- paste0(region, '-3')
  nm4 <- paste0(region, '-4')
  nm5 <- paste0(region, '-5')
  
  current1 <- extremeVals[[nm1]]
  current2 <- extremeVals[[nm2]]
  current3 <- extremeVals[[nm3]]
  current4 <- extremeVals[[nm4]]
  current5 <- extremeVals[[nm5]]
  
  new1 <- c(current1, saleAmt)
  new2 <- c(current2, bathroomCnt)
  new3 <- c(current3, bedroomCnt)
  new4 <- c(current4, finishedSqrFeet)
  new5 <- c(current5, lotSizeSquareFeet)
  
  extremeVals[[nm1]] <- new1
  extremeVals[[nm2]] <- new2
  extremeVals[[nm3]] <- new3
  extremeVals[[nm4]] <- new4
  extremeVals[[nm5]] <- new5
  
}  

regions <- unique(dat$regionid)

summary <- data.frame('Region'=regions, 
                      'Var1Med'=rep(NA, length(regions)), 
                      'Var1SD'=rep(NA, length(regions)), 
                      'Var2Med'=rep(NA, length(regions)), 
                      'Var2SD'=rep(NA, length(regions)),
                      'Var3Med'=rep(NA, length(regions)), 
                      'Var3SD'=rep(NA, length(regions)),
                      'Var4Med'=rep(NA, length(regions)), 
                      'Var4SD'=rep(NA, length(regions)),
                      'Var5Med'=rep(NA, length(regions)), 
                      'Var5SD'=rep(NA, length(regions)))

for (region in regions) {
  
  
  var1 <- paste0(region, '-1')  
  var2 <- paste0(region, '-2')
  var3 <- paste0(region, '-3')
  var4 <- paste0(region, '-4')
  var5 <- paste0(region, '-5')
  
  var1 <- extremeVals[[var1]]
  var2 <- extremeVals[[var2]]
  var3 <- extremeVals[[var3]]
  var4 <- extremeVals[[var4]]
  var5 <- extremeVals[[var5]]
  
  var1Med <- median(var1, na.rm=TRUE)
  var1SD <- sd(var1, na.rm=TRUE)
  
  var2Med <- median(var2, na.rm=TRUE)
  var2SD <- sd(var2, na.rm=TRUE)
  
  var3Med <- median(var3, na.rm=TRUE)
  var3SD <- sd(var3, na.rm=TRUE)
  
  var4Med <- median(var4, na.rm=TRUE)
  var4SD <- sd(var4, na.rm=TRUE)
  
  var5Med <- median(var5, na.rm=TRUE)
  var5SD <- sd(var5, na.rm=TRUE)
  
  summary[summary$Region == region, ]$Var1Med <- var1Med
  summary[summary$Region == region, ]$Var1SD <- var1SD
  
  summary[summary$Region == region, ]$Var2Med <- var2Med
  summary[summary$Region == region, ]$Var2SD <- var2SD
  
  summary[summary$Region == region, ]$Var3Med <- var3Med
  summary[summary$Region == region, ]$Var3SD <- var3SD
  
  summary[summary$Region == region, ]$Var4Med <- var4Med
  summary[summary$Region == region, ]$Var4SD <- var4SD
  
  summary[summary$Region == region, ]$Var5Med <- var5Med
  summary[summary$Region == region, ]$Var5SD <- var5SD
}  

extremeProps <- data.frame('propertyid'=dat$propertyid, 'isExtreme'=FALSE)

for (i in 1:nrow(dat)) {
  temp <- dat[i, ]  
  region <- temp$regionid
  summaryTemp <- summary[summary$Region == region, ]
  
  
  val1 <- temp$transvalue
  val2 <- temp$bathroomcnt
  val3 <- temp$bedroomcnt
  val4 <- temp$finishedsquarefeet
  val5 <- temp$lotsizesquarefeet
  
  extreme1 <- val1 > (summaryTemp$Var1Med + 2*summaryTemp$Var1SD) | val1 < (summaryTemp$Var1Med - 2*summaryTemp$Var1SD)
  extreme2 <- val2 > (summaryTemp$Var2Med + 2*summaryTemp$Var2SD) | val2 < (summaryTemp$Var2Med - 2*summaryTemp$Var2SD)
  extreme3 <- val3 > (summaryTemp$Var3Med + 2*summaryTemp$Var3SD) | val3 < (summaryTemp$Var3Med - 2*summaryTemp$Var3SD)
  extreme4 <- val4 > (summaryTemp$Var4Med + 2*summaryTemp$Var4SD) | val4 < (summaryTemp$Var4Med - 2*summaryTemp$Var4SD)
  extreme5 <- val5 > (summaryTemp$Var5Med + 2*summaryTemp$Var5SD) | val5 < (summaryTemp$Var5Med - 2*summaryTemp$Var5SD)
  
  extremeProps[extremeProps$propertyid == temp$propertyid, ]$isExtreme <- (extreme1 || extreme2 || extreme3 || extreme4 || extreme5)
}  

write.csv(extremeProps, file = "YourPathHere")

###########################################
# (B) Code Improvement
###########################################

library(dplyr)
library(data.table)

# Set working directory and read data 
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

write.csv(dat[,c('propertyid', 'isExtreme')], file="YourPathHere")

######################################
# (C) Code Comment/Documentation
######################################

# I chose to use the group_by function to more concisely compute the medians and standard deviations without for loops, all vectorized operations
# I wrote the checkExtreme file which reads a row and checks to see if it contains extreme values in a more concise and efficient manner (it doesn't have to compute two checks for each variable by computing the absolute value of the difference as opposed to the || statement in the last loop. 
# I was planning on using the apply function and apply checkExtreme to each row, but I ran into errors there and liked how the checkExtreme function did the check, and so I decided to use a for loop through all the rows.
# There was a bug in the original code which was that while it checked to see if the row had extreme values correctly, extremeProps was incorrectly saving which observations had extreme values or not. The way extremeProps is saving the isExtreme variable is that the last observation of propertyid is determining the isExtreme value for the entire subset of data with that propertyid, instead of doing it row-by-row. 
