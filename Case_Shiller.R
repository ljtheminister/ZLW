# Modeling Problem: 
#  Using the given data set create a repeat sales index using the Case and Shiller methodology as described in the attached paper.
#  NOTE: This dataset was created in the following way from a database of property transactions.
#		Every transaction is matched to prior sales subject to two things:
#                        That the PropertyIDs are match
#                        and that the matched transaction occured before the current observation.
#                This causes a problem in that every transaction is matched not just to the transaction immediately prior, 
#                but to transactions prior to that. 
#   Cleaning may be required.
#
#   Comment and structure your code to be easily followed.
#

setwd('/home/lj/ML/Zillow/')
data <- read.csv("Transactions.csv")


repsales <- data[duplicated(data$propertyid),]
