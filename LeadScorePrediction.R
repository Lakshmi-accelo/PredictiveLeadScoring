#Load Required Packages
install.packages('Boruta')
install.packages('ISLR')
install.packages('party')
install.packages("e1071")
install.packages("caTools")
install.packages("caret")

#Load the required packages
library(Boruta)
library(ISLR)
library(party)
library(e1071)
library(caTools)
library(caret)

#Load the data
salesData <- read.csv("*******")

#Data Visualization
barplot(table(salesData$Sale.Type))
barplot(table(salesData$Sale.Country))
barplot(table(salesData$Sale.Industry))

########------ Data Pre Processing ------########

#Deal with missing values

summary(salesData)

salesData = subset(salesData,select=-c(Sale.Valuation,Sale...user.month,Sale.Business.Size....of.seats..1,Sale...user.month.1,Sale.Business.Size....of.seats.))
salesData = subset(salesData,select=-c(Sale.Forerunner,Sale.MRR,Sale.Status,Sale.Created.Date))
salesData = subset(salesData,select=-c(Sale.Partner,Sale.Partner.Lead.Type,Sale.Incumbent.Provider,Sale.Implementation.Package,Sale.Gaps.of.Current.Solution,Sale.Event,Sale.About.the.Business,Sale.Introduced.By,Sale.Due.Date))
salesData = subset(salesData,select=-c(Sale.Tags,Sale.Title,Sale.Company.Name))
summary(salesData)
sum(is.na(salesData))
salesData <- na.omit(salesData) 
nrow(salesData)

#Convert Categorical features into factor data type 
convert <- c(2,3,4,5,6,7,10,9,11,12)
salesData[,convert] <-data.frame(lapply(salesData[convert],factor))
str(salesData)

#Separate active and lost/won

salesDataActive <-salesData[salesData$Sale.Standing=="active",]
str(salesDataActive)
salesData<-salesData[salesData$Sale.Standing!="active",]
convert <- c(1)
salesData[,convert] <-data.frame(lapply(salesData[convert],factor))
str(salesData)
salesDataActive[,convert] <-data.frame(lapply(salesDataActive[convert],factor))
str(salesDataActive)


#Feature Selection
boruta_output <- Boruta(Sale.Standing ~ ., data=salesData, doTrace=0)
names(boruta_output)

# Get significant variables including tentatives
boruta_signif <- getSelectedAttributes(boruta_output, withTentative = FALSE)
print(boruta_signif)  

# Do a tentative rough fix
roughFixMod <- TentativeRoughFix(boruta_output)
boruta_signif <- getSelectedAttributes(roughFixMod)
print(boruta_signif)

# Variable Importance Scores
imps <- attStats(boruta_output)
imps2 = imps[imps$decision != 'Rejected', c('meanImp', 'decision')]
head(imps2[order(-imps2$meanImp), ])  # descending sort

# Plot variable importance
plot(boruta_output, cex.axis=.7, las=2, xlab="", main="Variable Importance")  

#Split into training and test data set in a 70/30 split
dt = sort(sample(nrow(salesData), nrow(salesData)*.7))
train<-salesData[dt,]
test<-salesData[-dt,]



#Use a naive baye's classifier to classify the data
set.seed(120)  # Setting Seed
classifier_cl <- naiveBayes(Sale.Standing ~ ., data = train)
classifier_cl

# Predicting on test data
y_pred <- predict(classifier_cl, newdata = test)

# Confusion Matrix
cm <- table(test$Sale.Standing, y_pred)
cm

# Model Evaluation
confusionMatrix(cm)

#####------------Classification using support vector machine------------------#######
svmfit = svm(Sale.Standing  ~ ., data = salesData, kernel = "linear", cost = 10, scale = FALSE)
print(svmfit)

plot(svmfit, salesData,Sale.Tier~Sale.Source)
y_pred_svm = predict(svmfit, test)

# Confusion Matrix
cm_svm <- table(test$Sale.Standing, y_pred_svm)
cm_svm

# Model Evaluation
confusionMatrix(cm_svm)
