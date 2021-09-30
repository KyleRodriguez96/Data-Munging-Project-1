library(dplyr)

ninetyNine2 <- read.csv("/Users/gustavobarbosa/Desktop/Project/MERGED1999_00_PP.csv")
filteredList2<-ninetyNine2[,1:2392]
filteredList2

#Finding the total number of cells so we can find the ratio
#of uncomplete date
numCells2<-(NCOL(filteredList2))*(NROW(filteredList2))

#By default, the dataset's "NULL" values are set
#as text "NULL", this turns them to NA so R can recognize them more easily
filteredList2[filteredList2=="NULL"]<-NA

#Setting the numerical cells with values of 0
#to NA
zeroTest2<-filteredList2[filteredList2==0]<-NA

#Finding the number of NA instances so we can find the
#complete/incomplete data ratio
numNull2<-sum(is.na(filteredList2))

#Getting the percentage of incomplete to complete cells.
completeRatio2<-(numNull2/numCells2)*100

#Printing out our results for the ratio
cat("Out of a total ", numCells2, " cells in this dataset, ", numNull2, " are incomplete. Therefore, the percentage of incomplete entries in this dataset is ", completeRatio2, " percent.")


