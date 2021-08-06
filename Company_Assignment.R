# Installing the required packages 
install.packages("C50", dependencies = T)
library("C50")
install.packages("caret")
library("caret")
install.packages("psych")
library("psych")
install.packages("fastDummies")
library("fastDummies")

# Reading the dataset
data <- read.csv(file.choose())
View(data)
attach(data)

# Exploring the dataset
colnames(data)
str(data)
range(data$Sales)
data$Urban <- ifelse(data$Urban=='Yes',1,0)
data$US <- ifelse(data$US=='Yes',1,0)
data <- fastDummies::dummy_cols(data,select_columns = "ShelveLoc",remove_selected_columns = TRUE)
CatSales <- cut(data$Sales,breaks = c(0,4,8,12,17),labels = c("Bad","Medium","Good","Best"))
CatSales
new_data <- cbind(data,CatSales)
View(new_data)
New <- new_data[,-1]
View(New)

# Making training and testing dataset
ind <- sample(2, nrow(New),
              replace = TRUE,
              prob = c(0.8, 0.2))
training <- New[ind==1,]
testing <- New[ind==2,]
View(training)

# Decision Tree Model Making
model <- C5.0(training$CatSales~.,data = training,trails=1000)
summary(model)
pred <- predict.C5.0(model,training[,-ncol(training)])
pred
pred1 <- predict.C5.0(model,testing[,-ncol(testing)])
a <- table(training$CatSales,pred)
b <- table(testing$CatSales,pred1)
a
b
sum(diag(a)/sum(a))      # Training Accuracy
sum(diag(b)/sum(b))      # Testing Accuracy

plot(model)
pairs.panels(New)
