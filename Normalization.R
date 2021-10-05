library(dplyr)
library(data.table)
options(digits = 9)

file.location <- 'C:\\Users\\rodri\\Desktop\\NCFS1\\Munging\\Project_1\\Data-Munging-Project-1\\data\\all_years.csv'
all_data <- fread(file.location)

#Changing all Null vals to NA
all_data[all_data=="NULL"]<-NA

##Defining our variables##

#Completion rate of 4 year and less institutions 
completion_rate <- select(all_data,c('C150_4','C150_L4'))

#Percent of grads who have student loans 
student_loans <- select(all_data,c('LOAN_COMP_ORIG_YR8_RT',
                                   'LOAN_COMP_4YR_TRANS_YR8_RT',
                                   'LOAN_COMP_2YR_TRANS_YR8_RT'
                                   
))

#Percent of grads who never had student loans
no_student_loans <- select(all_data,c('NOLOAN_COMP_ORIG_YR8_RT',
                                      'NOLOAN_COMP_4YR_TRANS_YR8_RT',
                                      'NOLOAN_COMP_2YR_TRANS_YR8_RT'
                                      
))

#region of school
schools_region <- select(all_data, c('REGION'))

#Changing the columns to integers since they are being read in as characters
completion_rate$C150_4 <- as.numeric(completion_rate$C150_4)
completion_rate$C150_L4 <- as.numeric(completion_rate$C150_L4)


student_loans$LOAN_COMP_ORIG_YR8_RT <- as.numeric(student_loans$LOAN_COMP_ORIG_YR8_RT)
student_loans$LOAN_COMP_4YR_TRANS_YR8_RT <- as.numeric(student_loans$LOAN_COMP_4YR_TRANS_YR8_RT)
student_loans$LOAN_COMP_2YR_TRANS_YR8_RT <- as.numeric(student_loans$LOAN_COMP_2YR_TRANS_YR8_RT)

no_student_loans$NOLOAN_COMP_ORIG_YR8_RT <- as.numeric(no_student_loans$NOLOAN_COMP_ORIG_YR8_RT)
no_student_loans$NOLOAN_COMP_2YR_TRANS_YR8_RT <- as.numeric(no_student_loans$NOLOAN_COMP_2YR_TRANS_YR8_RT)
no_student_loans$NOLOAN_COMP_4YR_TRANS_YR8_RT <- as.numeric(no_student_loans$NOLOAN_COMP_4YR_TRANS_YR8_RT)


#Z-scores 

completion_rate$C150_4.z <- (completion_rate$C150_4 - mean(completion_rate$C150_4,na.rm = T))/sd(completion_rate$C150_4, na.rm=TRUE)
completion_rate$C150_L4.z <- (completion_rate$C150_L4 - mean(completion_rate$C150_L4,na.rm = T))/sd(completion_rate$C150_L4, na.rm=TRUE)

student_loans$LOAN_COMP_ORIG_YR8_RT.z <- (student_loans$LOAN_COMP_ORIG_YR8_RT - mean(student_loans$LOAN_COMP_ORIG_YR8_RT,na.rm = T))/sd(student_loans$LOAN_COMP_ORIG_YR8_RT, na.rm=TRUE)
student_loans$LOAN_COMP_4YR_TRANS_YR8_RT.z <- (student_loans$LOAN_COMP_ORIG_YR8_RT - mean(student_loans$LOAN_COMP_ORIG_YR8_RT,na.rm = T))/sd(student_loans$LOAN_COMP_ORIG_YR8_RT, na.rm=TRUE)
student_loans$LOAN_COMP_2YR_TRANS_YR8_RT.z <- (student_loans$LOAN_COMP_2YR_TRANS_YR8_RT - mean(student_loans$LOAN_COMP_2YR_TRANS_YR8_RT,na.rm = T))/sd(student_loans$LOAN_COMP_2YR_TRANS_YR8_RT, na.rm=TRUE)

no_student_loans$NOLOAN_COMP_ORIG_YR8_RT.z <- (no_student_loans$NOLOAN_COMP_ORIG_YR8_RT- mean(no_student_loans$NOLOAN_COMP_ORIG_YR8_RT,na.rm = T))/sd(no_student_loans$NOLOAN_COMP_ORIG_YR8_RT, na.rm=TRUE)
no_student_loans$NOLOAN_COMP_4YR_TRANS_YR8_RT.z <- (no_student_loans$NOLOAN_COMP_4YR_TRANS_YR8_RT - mean(no_student_loans$NOLOAN_COMP_4YR_TRANS_YR8_RT,na.rm = T))/sd(no_student_loans$NOLOAN_COMP_4YR_TRANS_YR8_RT, na.rm=TRUE)
no_student_loans$NOLOAN_COMP_2YR_TRANS_YR8_RT.z <- (no_student_loans$NOLOAN_COMP_2YR_TRANS_YR8_RT - mean(no_student_loans$NOLOAN_COMP_2YR_TRANS_YR8_RT,na.rm = T))/sd(no_student_loans$NOLOAN_COMP_2YR_TRANS_YR8_RT, na.rm=TRUE)

#summary stats and z plots

#completion
par(mfrow=c(2,1))
plot(completion_rate$C150_4.z, col='grey',pch=19)
plot(completion_rate$C150_L4.z, col='grey',pch=19)
fivenum(completion_rate$C150_4,na.rm = T)
fivenum(completion_rate$C150_L4,na.rm = T)
summary(completion_rate$C150_4)
summary(completion_rate$C150_L4)

#student loans
par(mfrow=c(3,1))
plot(student_loans$LOAN_COMP_ORIG_YR8_RT.z, ylim = range(-2,3), col='grey',pch=19)
plot(student_loans$LOAN_COMP_4YR_TRANS_YR8_RT.z, ylim = range(-2,3), col='grey',pch=19)
plot(student_loans$LOAN_COMP_2YR_TRANS_YR8_RT.z, ylim = range(-2,5), col='grey',pch=19)
fivenum(student_loans$LOAN_COMP_ORIG_YR8_RT,na.rm = T)
summary(student_loans$LOAN_COMP_ORIG_YR8_RT)
fivenum(student_loans$LOAN_COMP_4YR_TRANS_YR8_RT,na.rm = T)
summary(student_loans$LOAN_COMP_4YR_TRANS_YR8_RT)
fivenum(student_loans$LOAN_COMP_2YR_TRANS_YR8_RT,na.rm = T)
summary(student_loans$LOAN_COMP_2YR_TRANS_YR8_RT)

#no student loans 
par(mfrow=c(3,1))
plot(no_student_loans$NOLOAN_COMP_ORIG_YR8_RT.z , ylim = range(-2,8), col='grey',pch=19)
plot(no_student_loans$NOLOAN_COMP_2YR_TRANS_YR8_RT.z,ylim = range(-2,8), col='grey',pch=19)
plot(no_student_loans$NOLOAN_COMP_4YR_TRANS_YR8_RT.z,ylim = range(-2,8), col='grey',pch=19)
fivenum(no_student_loans$NOLOAN_COMP_ORIG_YR8_RT,na.rm = T)
summary(no_student_loans$NOLOAN_COMP_ORIG_YR8_RT)
fivenum(no_student_loans$NOLOAN_COMP_2YR_TRANS_YR8_RT,na.rm = T)
summary(no_student_loans$NOLOAN_COMP_2YR_TRANS_YR8_RT)
fivenum(no_student_loans$NOLOAN_COMP_4YR_TRANS_YR8_RT.z,na.rm = T)
summary(no_student_loans$NOLOAN_COMP_4YR_TRANS_YR8_RT.z)


#cutting data more
completion_rate <- tail(completion_rate,-100000)
student_loans <- tail(student_loans,-100000)
no_student_loans <-tail(no_student_loans,-100000) 
