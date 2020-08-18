View(cmpdata)
str(cmpdata) # In data set variables which data type
install.packages("C50")
library(C50)
install.packages("caret")
library(caret)
### as per given problem firstly Dependent variable sales convert into categorical by using ifelse condition 
high=ifelse(cmpdata$Sales<10,"no","yes")

### Creating the dummy variable for column Shelveloc ,Urban,US
library(dummies)
cmpdata.new=dummy.data.frame(cmpdata)
dummy[ShelveLoc=="Bad"]==0
dummy[ShelveLoc=="Medium"]==1
dummy[ShelveLoc=="Good"]==2
dummy[Urban=="Yes"]==1
dummy[Urban=="No"]==0
dummy[US=="No"]==0
dummy[US=="Yes"]==1
View(cmpdata.new)


## Clubing the both dependent and independent variable 
finaldata=data.frame(cmpdata.new,high)
View(finaldata)
finaldata1=finaldata[-1]
View(finaldata1)

# table or proportions with more informative labels
round(prop.table(table(finaldata1$high)) * 100, digits = 1)
### In above code digit =1 means, in output in result after the point i want only 1 value so only digit =1

 ## in finaldata1 set independent variables have different units so we have to normalize 

# create normalization function (custom define normalization function)
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}

# normalize the finaldata1
finaldata1_n <- as.data.frame(lapply(finaldata1[1:14], normalize))
View(finaldata1_n)

# Data partion for model building and testing
inTraininglocal <- createDataPartition(finaldata1$high,p=.80, list=F) # Here List= false becoz we want in matrix format,not in list format so only 
## training =80% and testing= 20% we divided the data set  

training <- finaldata1[inTraininglocal,]
View(training)
testing <- finaldata1[-inTraininglocal,]
View(testing)
#model1 building
model1 <- C5.0(training$high~.,data = training,trails = 1000)
?C5.0
# Generating the model1 summary
summary(model1)
pred <- predict.C5.0(model1,testing)
a <- table(testing$high,pred)
View(a)
confusionMatrix(a)
## the Accuracy is 83.54%
plot(model1)
