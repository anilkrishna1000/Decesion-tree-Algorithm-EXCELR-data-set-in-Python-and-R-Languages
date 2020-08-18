View(fcheck)
## Required packages 
install.packages("C50")
library(C50)
install.packages("caret")
library(caret)

###  ifelse condtion apply if taxable income <=30000 Risky ,else good 
Risky_good=ifelse(fcheck$Taxable.Income<=30000,"Risky","good")
 

### Creating the dummy variables for Undergrand,Marital status ,urban columns 
library(dummies)
fcheck.new=dummy.data.frame(fcheck)
dummy[Undergrad=="Yes"]=1
dummy[Undergrad=="No"]=0
dummy[Marital.Status=="Single"]=0
dummy[Marital.Status=="Divorced"]=1
dummy[Marital.Status=="Married"]=2
dummy[Urban=="Yes"]=1
dummy[Urban=="No"]=0
View(fcheck.new)
fc=fcheck.new[-6]## Deleting the taxable.income column  bocz we applied ifelse condition to that column 
### and we considering the taxable.income  is dependent variable  ,if <=30000 risky ,otherwise good

View(fc)
View(Risky_good)
     

## Clubing the both dependent and independent variable 
finaldata=data.frame(fc,Risky_good)
View(finaldata)


# Data partion for model building and testing
inTraininglocal <- createDataPartition(finaldata$Risky_good,p=.80, list=F) # Here List= false becoz we want in matrix format,not in list format so only 
## training =80% and testing= 20% we divided the data set  

training <- finaldata[inTraininglocal,]
View(training)
testing <- finaldata[-inTraininglocal,]
View(testing)
#model building
model <- C5.0(training$Risky_good~.,data = training,trails = 1000)
?C5.0
# Generating the model summary
summary(model)
pred <- predict.C5.0(model,testing[,-10])
a <- table(testing$Risky_good,pred)
View(a)
confusionMatrix(a)
## the Accuracy is 79.88%
plot(model)
