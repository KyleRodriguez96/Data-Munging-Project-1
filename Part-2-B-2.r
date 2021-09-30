#Import dplyr and tidyverse for pipe functionality
library(dplyr)
library(tidyverse)
#import skimr to streamline data quality checking
library(skimr)
library(readr)
options(scipen=999)


all.years <- read_csv("/Users/gustavobarbosa/Desktop/Project/all_years.csv")

#Narrow to graduation rate, region and percentage of
#students who filed for federal student loans

selectedTable<-all.years %>%
  select(C150_4, REGION, PCTFLOAN)

#Change column names to make the data easier to work with
#since the original dataset's labels were hard to read

colnames(selectedTable)<-c("Graduation.Rate", "Region", "Loan.Rate")

#Now, skim() does the entire job for us. But just submitting 
#that is cheating so we'll just use its results as a baseline
#to compare my own results to in order to check for correctness.
selectedTable %>% skim()

#First of all, let's use our NA assigning code from
#part 1.
selectedTable[selectedTable=="NULL"]<-NA

#I think turning 0s into NAs in this case might be irresponsible.
#If the Loan Rate and Grad Rate column both have nulls (they both do) 
#then it implies that in some cases, the loan and grad rates are zero.
#This would be an interesting question to look into in the future, but
#for now I wanted to make a point about why it's bad to impulsively make large
#changes to datasets indiscriminateley without first understanding the data.

#Find total number of columns and rows
numColumn<-NCOL(selectedTable)
numRows<-NROW(selectedTable)

#Number of NA cells in each column
numMissing<-colSums(is.na(selectedTable))

percentMissing<-(numMissing/numRows)*100
percentMissing

cat("The following values indicate the percentage of missing values : ")
percentMissing

#We can see that our results are consistent with the skim() function's results.
#The only difference is that ours measures in the percentage of missing records
#whereas skim() measures in the percentage of complete records. 

###Uncomment the skim() function below to check, but it says graduation rate is 29.1% complete, loan rate
#is 43.1% complete, and Region is nearly 100% complete. 

#skim(selectedTable)


