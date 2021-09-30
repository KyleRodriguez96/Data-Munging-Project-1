library(dplyr)
library(data.frame)
library(readr)

allyears <- read_csv("/Users/gustavobarbosa/Desktop/Project/all_years.csv")
filteredYears<-allyears[,1:2392]
filteredYears

#Finding the total number of cells so we can find the ratio
#of uncomplete date
cellNum<-(NCOL(filteredYears))*(NROW(filteredYears))

#By default, the dataset's "NULL" values are set
#as text "NULL", this turns them to NA so R can recognize them more easily
filteredYears[filteredYears=="NULL"]<-NA

#Setting the numerical cells with values of 0
#to NA
#setZero<-filteredYears[filteredYears==0]<-NA

#Finding the number of NA instances so we can find the
#complete/incomplete data ratio
numNA<-sum(is.na(filteredYears))

#Getting the percentage of incomplete to complete cells.
incompleteRatio<-(numNA/cellNum)*100

#Printing out our results for the ratio
cat("Out of a total ", cellNum, " cells in this dataset, ", numNA, " are incomplete. Therefore, the percentage of incomplete entries in this dataset is ", incompleteRatio, " percent.")


