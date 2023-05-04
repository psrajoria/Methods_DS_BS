# Quelle für Analyse:
#https://www.kaggle.com/code/marcinrutecki/best-techniques-and-metrics-for-imbalanced-dataset

#Data analysis
install.packages("dplyr")
##install.packages("car")
install.packages("ggplot2")
install.packages("PerformanceAnalytics")
install.packages("stargazer")
library(dplyr)
library(stargazer)
library(PerformanceAnalytics) 
##library(car)
library(ggplot2)
install.packages("openxlsx")
library(openxlsx)


## Option 1
mydata <- read.xlsx("C:/Users/kdtam/Desktop/DS_for_Bus/Data_TSOs.xlsx")#, col_types = c("numeric", "character", "numeric"))
View(mydata)
mydata[2, "Efficiency"] <- "Efficiency"
colnames(mydata) <- mydata[2,]
mydata <- mydata[-1,-1]
mydata <- mydata[-1,]
rownames(mydata) <- NULL # remove the existing row names
rownames(mydata) <- 1:nrow(mydata) # set the new row names starting from 1 to the number of rows in the data frame

## Option 2
mydata <- read.xlsx("C:/Users/kdtam/Desktop/DS_for_Bus/Data_TSOs_edited.xlsx")

dim(mydata)
# Check the data type of each column
str(mydata)


stargazer(mydata, type ="text")   #descriptive statistics.


#PERFORMANCE ANALYTICS
opex_data <- mydata[, c(2,3,4, 5, 6,7, 8, 9, 10, 11)] 
efficiency_data <- mydata[, c(2,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26)] 
chart.Correlation(opex_data, histogram=TRUE, pch=19)
chart.Correlation(efficiency_data, histogram=TRUE, pch=19)

# EXCLUDE BASED ON PERFORMANCE ANALYTICS PLOT
df_excludeCorVars <- df_excludeCorVars %>% select( -msw, -msw_un, -pden, -finance)
#df_excludeCorVars <- df_excludeCorVars %>% select(  -finance)
#------------------------


# OUTLIERS/HAT VALUES/ LEVERAGE PART1
#--------------------------------------------------------------------------------------------------------------------------------------------------

colnames(opex_data)

mlm1 = lm(OPEX ~year+ `Network.extension.<.230.kV`+`Network.extension.≥.230.kV`+`Number.of.power.transformers.and.reactors.<.230.kV`+`Number.of.power.transformers.and.reactors.<.230.kV.1`+
`Number.of.switch.modes.<.230.kV`+`Number.of.switch.modes.≥.230.kV`+`Transformation.capacity.(MVA)`+`Reactive.power.(Mvar)`, data = opex_data)
stargazer(mlm1, type = "text")

# Use of linear regression model as learned in lecture to find the most influential and severe outliers
mlm1 = lm(tc ~ cres + csor + area + pop + alt + isle + sea  + wden + d_fee + sample + paper + glass + metal + plastic + raee + other + msw_so + sor 
          + s_wteregio + s_landfill + gdp +  urb+ geo + wage
          + region,
          data = df_excludeCorVars_10)
mlm1_quadratic = lm(tc ~ cres+ I(cres^2) + csor+I(csor^2) + area + I(area^2) + pop + I(pop^2) + alt + I(alt^2) + isle + sea + wden + I(wden^2) + d_fee + sample + paper+ I(paper^2) 
                    + glass + I(glass^2) + metal + I(metal^2) + plastic +I(plastic^2) + raee +I(raee^2)+ other + I(other^2)+ msw_so + I(msw_so^2) + sor + I(sor^2)+s_wteregio 
                    +I(s_wteregio^2) +s_landfill + I(s_landfill^2)+ gdp + I(gdp^2) + urb + geo + region + wage + I(wage^2), data = df_excludeCorVars_10)
stargazer(mlm1,mlm1_quadratic, type = "text")
par(mfrow = c(2,2))
plot(mlm1)     

#FIND OUTLIERS
plot(residuals(mlm1))
plot(rstudent(mlm1))
which(rstudent(mlm1) >= 8) # 633, 546
which(rstudent(mlm1) <= -8) #74, 88

# FIND HIGH LEVERAGE OBSERVATIONS UNSIG hatvalues()
plot(hatvalues(mlm1))
which(hatvalues(mlm1) >= 0.3) # Wir machen Treshhold basierend auf Plot. Formeln ergeben zu starken treshhold.
                              # 1412, 1201, 2911, 3624
par(mfrow = c(1,1))
influencePlot(mlm1, id.method = "identify")        # Identify Values with Cooks´D >~ 0.05
which(cooks.distance(mlm1)>0.05) # NEW OUTLIER: 4148
# OUTLIER UND HAT VALUES LÖSCHEN
#rows_to_delete <- c(7,   20,   26,   60,   94,  104,  109,  132,  157,  175,  193,  210,  214,  218,  542,  758,  834, 1007, 1062, 1204, 1294, 1299, 1388, 1501, 1799, 1822, 2058, 2711, 2728, 3003, 3294, 3595, 3883, 3, 15, 21,   49,   79,   87,   90,  107,  129,  143,  159,  171,  175,  179,  465,  651,  713,  846,  891, 1022, 1099, 1103, 1180, 1281, 1530, 1549, 1751, 2274, 2287, 2482, 2680, 2882, 3065, 3889, 3992, 4066, 4095, 4184, 4238, 4244, 3069, 3130, 3171, 3192, 3259, 3298, 3299, 1890, 1604, 141, 378, 115, 318, 378)
rows_to_delete_corrected <- c(633, 546, 88, 74, 1412, 3624, 1201, 2911, 4148 )
                             
                             
# deleting the outliers
#df_excludeCorVars_10<- df_excludeCorVars_10[-rows_to_delete, ]
df_excludeCorVars_10<- df_excludeCorVars_10[-rows_to_delete_corrected, ]
# Correct row numbers
rownames(df_excludeCorVars_10) <- NULL # remove the existing row names
rownames(df_excludeCorVars_10) <- 1:nrow(df_excludeCorVars_10) # set the new row names starting from 1 to the number of rows in the data frame

# resulting data set for imputation

df_prepared <- df_excludeCorVars_10
str(df_prepared)





#Final data

final_data <- imputed_data 

#---------------------------------------------------------------------------------------------  

#SET TRAIN AND TEST SET FOR SUBSET SELECTION
#If you want to compare models with similar structure in terms of predictive accuracy, use test error.
# For Subset selection use BIC, CP, ADJR2 and for their comparison use test error

#SUBSET SELECTION
install.packages("leaps")
library(leaps)
#?regsubsets
# Quelle zeigt, dass Variablen >10 nicht total unplausibel: https://pubmed.ncbi.nlm.nih.gov/26319135/

#Trainings und Validierungsset 
# If you want to compare models with similar structure in terms of predictive accuracy, use test error.
#final_data <- final_data %>% select( -cres, -sor, -plastic, -pop, -area, alt, other )
best1 = regsubsets(tc ~., data = final_data, nvmax=46)
# no linear dependencies after data set clean up


# BEST SUBSET SELECTION
summary(best1)$rss
summary(best1)$adjr2

which.min(summary(best1)$cp) #31
which.min(summary(best1)$bic) #19
which.min(summary(best1)$rss) #43
which.max(summary(best1)$adjr2) #34
par(mfrow= c(1, 1))


par(mfrow=c(2,3))
plot(summary(best1)$cp, xlab = "No. of predictors", ylab = "Cp", type = "l")
points(31, summary(best1)$cp[31], col = "red", cex = 2, pch = 20)
plot(summary(best1)$bic, xlab = "No. of predictors", ylab = "BIC", type = "l")
points(19, summary(best1)$bic[19], col = "red", cex = 2, pch = 20)
plot(summary(best1)$adjr2, xlab = "No. of predictors", ylab = "adjr2", type = "l")
points(34, summary(best1)$adjr2[34], col = "red", cex = 2, pch = 20)
plot(summary(best1)$rss, xlab = "No. of predictors", ylab = "adjr2", type = "l")
points(43, summary(best1)$adjr2[43], col = "red", cex = 2, pch = 20)

coef(best1 ,41)
# best model comes from best subset selection with 20 variables

# As we discussed in Section 6.1.1, the model containing all of the predictors will always have the smallest RSS and the largest R2, 
# since these quantities are related to the training error. Instead, we wish to choose a model with a low test error. As is evident here, 
# and as we show in Chapter 2, the training error can be a poor estimate of the test error. Therefore, RSS and R2 are not suitable for 
#,selecting the best model among a collection of models with different numbers of predictors.
# We can indirectly estimate test error by making an adjustment to the training error to account for the bias due to overfitting.

# In subset selection for statistical models, the Bayesian Information Criterion (BIC) and the Mallows' Cp statistic are commonly used as measures of model fit. 
# The test error can also be used as a measure of model fit, but this is typically reserved for assessing the performance of the selected model 
# rather than for selecting among candidate models.
str(final_data)
set.seed (5)
train_subset=sample(c(TRUE,FALSE), nrow(final_data),rep=TRUE)
test_subset =(! train_subset )

# perform best subset on training set
regfit.best_val = regsubsets(tc ~.,data=final_data[train_subset,], nvmax = 48)
summary(regfit.best_val)$adjr2 #34
summary(regfit.best_val)
#compute the validation set error
test.mat=model.matrix(tc ~.,data=final_data[test_subset,])

val.errors=rep(NA,48)
for(i in 1:43){                     # die 43 bekommt man durch summmary(regfit) als Anzahl der Variablen
  coefi=coef(regfit.best_val,id=i)
  pred=test.mat[,names(coefi)]%*%coefi
  val.errors[i]=mean((final_data$tc[test_subset]-pred)^2)
}

val.errors     
which.min(val.errors)     # 42 (3), 16(5), 28(7), 34(9)
val.errors[43]            #  1862.899 (3), 1674.624 (5), 1876.31(7), 1827.361(9)
sd(val.errors, na.rm = TRUE) # 273.47(3),249.8241 (5), 251.9972(7), 267.2783(9)
sd(val.errors, na.rm = TRUE)
coefs<-coef(regfit.best_val ,43) # Koeffizienten des besten Modells 
coef(regfit.best_val ,43)
coefs_ordered <- coefs[order(abs(coefs), decreasing = TRUE)]
coefs_ordered
# Create a data frame with the coefficients
coef_df <- data.frame(variable = names(coefs_ordered), coefficient = coefs_ordered)

# Write the data frame to a file
write.table(coef_df, file = "coefs_table.csv", sep = ",", row.names = FALSE)
summary(regfit.best_val)



#perform best subset selection on full data set and select best 33 Variable model. Check if the same. 
regfit.best_full = regsubsets(tc ~.,data=final_data, nvmax =43)
coefs_2<-coef(regfit.best_full ,43) # Koeffizienten des besten Modells mit 33 Var
coefs_ordered_2 <- coefs_2[order(abs(coefs_2), decreasing = TRUE)]
coefs_ordered_2

coefs<-coef(regfit.best_val ,42) # Koeffizienten des besten Modells mit 33 Var
coefs_ordered <- coefs[order(abs(coefs), decreasing = TRUE)]
coefs_ordered



summary_best = summary(regfit.best_full)
# Extract the R-squared value from the summary output
rsq = summary_best$rsq[which.max(summary_best$adjr2)]

# Print the R-squared value
cat("R-squared of the best model:", round(rsq, 3))







#--------------------------------------------------------
str(final_data, text = "text")

final_data$isle <- as.factor(final_data$isle)
final_data$sea <- as.factor(final_data$sea)
final_data$urb <- as.factor(final_data$urb)
final_data$d_fee <- as.factor(final_data$d_fee)
final_data$sample <- as.factor(final_data$sample)
#final_data$s_wteregio <- as.factor(final_data$s_wteregio) #schlechtere ergebnisse mit factoring
#final_data$s_landfill <- as.factor(final_data$s_landfill) #schlechtere ergebnisse mit factoring
final_data$region <- as.factor(final_data$region)

str(final_data, text = "text")


# PREDICTOR MATRIX
x = model.matrix(tc ~ ., data = final_data)[,-1] #response vector # -1 to delete the intercept
y = final_data$tc


# DEFINE TRAIN AND TEST SET FOR TREES
set.seed (5)
train = sample(1:nrow(x), nrow(x)*0.5) # erst nur 50:50 ohne CV
test=final_data[-train,]
y.test=y[test]

# Bagging

#install.packages("randomForest")
#install.packages("caret")
library(caret)
library(randomForest)

# Ohne CV um ein richtiges Maß für ntrees zu finden, damit die for schleife nicht zu lange dauert im CV Schritt
set.seed(5)
bag.df_imputed10 = randomForest(tc~., data = final_data, subset = train, mtry = 25, ntree = 100, importance = TRUE)
bag.df_imputed10

yhat.bag = predict(bag.df_imputed10, newdata = final_data[-train,])
df_imputed10.test = final_data[-train, "tc"]
mean((yhat.bag - df_imputed10.test)^2)
plot(yhat.bag , df_imputed10.test)
abline (0,1, col = "red")
mean((yhat.bag - df_imputed10.test)^2)
R2_Bag <- 1 - sum((y.test - yhat.bag)^2) / sum((y.test - mean(y.test))^2)
R2_Bag
cat("R2 Bagging:", mean(R2_Bag))

importance(bag.df_imputed10)
importance_Bagging <- importance(bag.df_imputed10)
importance_Bagging <- data.frame(Variables = row.names(importance_Bagging), MSE = importance_Bagging[,1])
importance_Bagging <- importance_Bagging[order(importance_Bagging$MSE, decreasing = TRUE),]
ggplot(importance_Bagging[1:24,], aes(x=reorder(Variables, MSE), y=MSE, fill=MSE)) + geom_bar(stat = 'identity') + labs(x = 'Variables', y= '% increase MSE if variable is randomly permuted') + coord_flip() + theme(legend.position="none")


# Mit CV
# REDEFINE TRAIN AND TEST SET FOR TREES
x = model.matrix(tc ~ ., data = final_data)[,-1] #response vector # -1 to delete the intercept
y = final_data$tc
set.seed (5)
train = sample(1:nrow(x), nrow(x)*0.8) #test mit 1/5 test set, 4/5 train set
test=final_data[-train,]

set.seed(5)
folds <- createFolds(train, k = 5)

# Initialize vectors to store MSE and R2 values for training and test sets for each fold
train_mse_bag <- numeric(5)
test_mse_bag <- numeric(5)
train_r2_bag <- numeric(5)
test_r2_bag <- numeric(5)

for (i in 1:5) {
  # Split data into train and test sets
  train_bag <- unlist(folds[-i])
  test_bag <- folds[[i]]
  
  # Fit random forest model using train set
  Bagging_model <- randomForest(tc ~ ., data = final_data[train_bag,], mtry = 25, ntree = 500)
  
  # Predict target variable for train and test sets
  y.hat_train <- predict(Bagging_model, newdata = final_data[train_bag,])
  y.hat_test <- predict(Bagging_model, newdata = final_data[test_bag,])
  
  # Calculate MSE and R2 for train and test sets
  train_mse_bag[i] <- mean((final_data$tc[train_bag] - y.hat_train)^2)
  test_mse_bag[i] <- mean((final_data$tc[test_bag] - y.hat_test)^2)
  train_r2_bag[i] <- cor(y.hat_train, final_data$tc[train_bag])^2
  test_r2_bag[i] <- cor(y.hat_test, final_data$tc[test_bag])^2
}

# Print mean MSE and R2 values for train and test sets for all folds
cat("Train set - Mean MSE:", mean(train_mse_bag), "Mean R2:", mean(train_r2_bag), "\n")
cat("Test set - Mean MSE:", mean(test_mse_bag), "Mean R2:", mean(test_r2_bag))


# Random Forest without CV
x = model.matrix(tc ~ ., data = final_data)[,-1] #response vector # -1 to delete the intercept
y = final_data$tc
set.seed (3)
train = sample(1:nrow(x), nrow(x)*0.5) # erst nur 50:50 ohne CV
test = (-train)
y.test=y[test]

set.seed(3)
RF.df_imputed10 = randomForest(tc~., data = final_data, subset = train, mtry = 12, ntree = 500, importance = TRUE)
RF.df_imputed10

yhat.bag = predict (RF.df_imputed10 , newdata=final_data[-train,])
df_imputed10.test = final_data[-train, "tc"]
#plot(yhat.bag , df_imputed10.test)
#abline (0,1, col = "red")
mean((yhat.bag - df_imputed10.test)^2)
R2_RF <- 1 - sum((y.test - yhat.bag)^2) / sum((y.test - mean(y.test))^2)
R2_RF
importance(RF.df_imputed10)
importance_RF <- importance(RF.df_imputed10)
importance_RF <- data.frame(Variables = row.names(importance_RF), MSE = importance_RF[,1])
importance_RF <- importance_RF[order(importance_RF$MSE, decreasing = TRUE),]
ggplot(importance_RF[1:24,], aes(x=reorder(Variables, MSE), y=MSE, fill=MSE)) + geom_bar(stat = 'identity') + labs(x = 'Variables', y= '% increase MSE if variable is randomly permuted') + coord_flip() + theme(legend.position="none")

# Random Forest with CV

# REDEFINE TRAIN AND TEST SET FOR TREES
x = model.matrix(tc ~ ., data = final_data)[,-1] #response vector # -1 to delete the intercept
y = final_data$tc
set.seed (5)
train = sample(1:nrow(x), nrow(x)*0.8) #test mit 1/5 test set, 4/5 train set
test=final_data[-train,]

set.seed(5)
folds <- createFolds(train, k = 5)

# Initialize vectors to store MSE and R2 values for training and test sets for each fold
train_mse_RF <- numeric(5)
test_mse_RF <- numeric(5)
train_r2_RF <- numeric(5)
test_r2_RF <- numeric(5)

for (i in 1:5) {
  # Split data into train and test sets
  train_RF <- unlist(folds[-i])
  test_RF <- folds[[i]]
  
  # Fit random forest model using train set
  RandomForest_model <- randomForest(tc ~ ., data = final_data[train_RF,], mtry = 12, ntree = 500)
  
  # Predict target variable for train and test sets
  y.hat_train_RF <- predict(RandomForest_model, newdata = final_data[train_RF,])
  y.hat_test_RF <- predict(RandomForest_model, newdata = final_data[test_RF,])
  
  # Calculate MSE and R2 for train and test sets
  train_mse_RF[i] <- mean((final_data$tc[train_RF] - y.hat_train_RF)^2)
  test_mse_RF[i] <- mean((final_data$tc[test_RF] - y.hat_test_RF)^2)
  train_r2_RF[i] <- cor(y.hat_train_RF, final_data$tc[train_RF])^2
  test_r2_RF[i] <- cor(y.hat_test_RF, final_data$tc[test_RF])^2
}

# Print mean MSE and R2 values for train and test sets for all folds
cat("Train set - Mean MSE:", mean(train_mse_RF), "Mean R2:", mean(train_r2_RF), "\n")
cat("Test set - Mean MSE:", mean(test_mse_RF), "Mean R2:", mean(test_r2_RF))




# Boosting

#install.packages("gbm")
library(gbm)

# ohne CV
x = model.matrix(tc ~ ., data = final_data)[,-1] #response vector # -1 to delete the intercept
y = final_data$tc

set.seed (5)
train = sample(1:nrow(x), nrow(x)*0.8) #test mit 1/5 test set, 4/5 train set
test=final_data[-train,]

set.seed(5)
boosting_model = gbm(tc~., data = final_data[train,], distribution = "gaussian", n.trees = 10000, interaction.depth = 20, shrinkage = 0.0001)
yhat.boost.train <- predict(boosting_model, newdata = final_data[train,], n.trees = 10000)
Boosting_test_MSE <- mean((yhat.boost.train - final_data[train, "tc"])^2)
Boosting_test_R2 <- cor(yhat.boost.train, final_data[train, "tc"])^2
cat("MSE for training set:", Boosting_test_MSE, "\n")
cat("R-squared for training set:", Boosting_test_R2, "\n")

# test set values

yhat.boost_test=predict(boosting_model ,newdata = final_data[-train,], n.trees = 10000)
Boosting_MSE <- mean((yhat.boost_test - final_data[-train, "tc"])^2)
Boosting_R2 <- cor(yhat.boost_test, final_data[-train, "tc"])^2
# train set values
cat("MSE for test set:", Boosting_MSE, "\n")
cat("R-squared for test set:", Boosting_R2, "\n")



# mit CV
set.seed(5)
folds <- createFolds(train, k = 5)

# Initialize vectors to store MSE and R2 values for training and test sets for each fold
train_mse_Boost <- numeric(5)
test_mse_Boost <- numeric(5)
train_r2_Boost <- numeric(5)
test_r2_Boost <- numeric(5)

for (i in 1:5) {
  # Split data into train and test sets
  train_Boost <- unlist(folds[-i])
  test_Boost <- folds[[i]]
  
  # Fit boosting model using train set
  boost_model <- gbm(tc ~ ., data = final_data[train_Boost,], distribution = "gaussian", n.trees = 10000, interaction.depth = 5, shrinkage = 0.0005)
  
  # Predict target variable for train and test sets
  yhat.boost.train <- predict(boost_model, newdata = final_data[train_Boost,], n.trees = 10000)
  yhat.boost_test <- predict(boost_model, newdata = final_data[test_Boost,], n.trees = 10000)
  
  # Calculate MSE and R2 for train and test sets
  train_mse_values[i] <- mean((final_data$tc[train_Boost] - yhat.boost.train)^2)
  test_mse_values[i] <- mean((final_data$tc[test_Boost] - yhat.boost_test)^2)
  train_r2_values[i] <- cor(yhat.boost.train, final_data$tc[train_Boost])^2
  test_r2_values[i] <- cor(yhat.boost_test, final_data$tc[test_Boost])^2
}

# Print mean MSE and R2 values for train and test sets for all folds
cat("Train set - Mean MSE:", mean(train_mse_values), "Mean R2:", mean(train_r2_values), "\n")
cat("Test set - Mean MSE:", mean(test_mse_values), "Mean R2:", mean(test_r2_values))
summary(boost_model)

#----------------------------------------------------------------



# beide Modelle mit R^2 gegeneinander plotten. Optimal wäre es auch den Bias zu visualisieren (Buch S. 224) 

#We now check whether there is any benefit to performing ridge regression with λ = 4 instead of just performing least squares regression. Recall that least squares is simply ridge regression with λ = 0.5
# Frage: Müssen wir dann bei der Wahl des lambdas auch den Trade-Off zu Bias und Variance bedenken?
#Prüfen wir ob poly Modell besser? --> Ridge Regression mit Poly (nicht im Buch)



"""
The error message "linear dependencies found" in the R package "leaps" indicates that there are some variables in the input data that are highly correlated, leading to multicollinearity in the regression analysis. Multicollinearity can cause instability in the estimated regression coefficients and lead to incorrect inferences about the relationships between the independent variables and the response variable.

To find out which variables are the linear dependencies, you can try the following methods:
  
  Variance Inflation Factor (VIF): Calculate the VIF for each variable and look for the variables with the highest VIF scores. A VIF score close to or greater than 10 indicates a high degree of multicollinearity.
Correlation matrix: Create a correlation matrix of all the independent variables and look for variables that have high correlations with each other. A correlation coefficient close to 1 indicates a high degree of multicollinearity.
Eigenvalues and eigenvectors: Perform an eigenvalue decomposition of the variance-covariance matrix of the independent variables and look for small eigenvalues. Small eigenvalues indicate that there are linearly dependent variables in the data.
Once you have identified the linear dependencies, you can either remove one of the highly correlated variables or combine the variables into a single composite variable through principal component analysis or factor analysis.