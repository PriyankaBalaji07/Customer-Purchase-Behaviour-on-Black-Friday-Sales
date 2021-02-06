#Read and Load the BlackFriday dataset
mydata <- read.csv(file.choose(), sep=",", header = TRUE ) #Dataset attached under the name Black Friday 

#Structure of the data
str(mydata) 

#Clean the data
mydata$User_ID <- NULL #As User and Product ID provide unique numbers
mydata$Product_ID <- NULL
mydata$Marital_Status <- as.factor(mydata$Marital_Status) #convert from integer to factor 
mydata$Occupation <- as.factor(mydata$Occupation) #convert from integer to factor 
index <- is.na(mydata)
mydata[index] <- 0 # replace all the na`s with 0`
mydata$Product_Category_2 <- as.integer(mydata$Product_Category_2) #convert to integer
mydata$Product_Category_3 <- as.integer(mydata$Product_Category_3) #convert to integer
str(mydata) #check structure of the data after changes

#Using the stepwise regression forward slection technique 
#analyze variables as adding progressively in order to improve model
fitall = lm(Purchase~., data=mydata) #Use all the predictor variables
formula(fitall) 
fitstart = lm(Purchase ~ 1, data=mydata) #fit model intercept only
head(mydata)
summary(fitstart) #Summary of the dateaset
step(fitstart, direction = "forward", formula(fitall)) #Add all predicted variables progressively in order to get best model with lowest AIC
# model <- lm(formula = Purchase ~ Product_Category_1 + Product_Category_3 + 
              #City_Category + Occupation + Gender + Age + Product_Category_2 + 
              #Marital_Status, data = mydata)

# Splitdataset into train (80%) and test (20%) of data 
set.seed(1234)
ind <- sample(2, nrow(mydata), replace = TRUE, prob = c(0.8,0.2))
tdata <- mydata[ind==1,]
test <- mydata[ind==2,]

#Multiple linear regression model
result <- lm(Purchase ~ Product_Category_1 + Product_Category_3 + 
               City_Category + Occupation + Gender + Age + Product_Category_2 + 
               Marital_Status, tdata) #got model based on stepwise regression result
result
summary(result) #get a summary of the model

#Prediction
pred <- predict(result, test)
head(pred)
head(test$Purchase)

