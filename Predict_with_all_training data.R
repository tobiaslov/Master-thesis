
#This takes all the training data and produces a logistic regression

full_data_function <- function(df) {
  
  #Input: Data file where first column is ID and last column is outcome variable
  #Output: Accuracy and MAD
  
#Create parameter vector (length is model matrix -2 because of ID and Y)
b0 <- rep(0, ncol(model.matrix(~ ., df)) - 2)

#testing data set
data_testing <- df %>%
  head(nrow(df)/2)

#training data set
data_training <- df %>% 
  tail(nrow(df)/2)


#When taking a sample from the trianing set, initial parameters may not be estimatable
#Solving with a tryCatch function to always get working data
res <- simpleError("Fake error")
f <- 1

#While res contains some error
while(inherits(res, "error")) {
  
  #Create a new entity res that resets the loop every time the functions returns errors.
  #Performs both the sample of the data and the first estimation 
  res <- tryCatch({ 
  
########### Creating x  model matrix and Y vector #######################

# Creating model matrix from the starting sample

X <- as.data.frame(model.matrix(~ ., data_training))

#Creating true Y vector for labeled data
Y <- X[ ,ncol(X)]
#Creating model matrix (withouth Y and ID)
X <- as.matrix(X[,-c(2,ncol(X))])

#########################################################################

################## parameter estimation ######################

#Generating empty B vector to start iterations in scoring method 

#print("estimate initial parameters")
b_start <- b0
b_new <- b_start + 1
error <- b_new - b_start
# print("estimating parameters")
while(all(abs(error) >= 1e-10)) 
{
  #This computation is for logistic regression only
  W <- diag(as.vector(exp(X %*% b_start)/(1 + exp(X %*% b_start))^2))
  z <- X %*%  b_start + Y * ( (1 + exp( X %*% b_start ))^2 / (exp(X %*% b_start)) ) - ( 1 + exp(X %*% b_start))
  I <- t(X) %*% W %*% X
  XWz <- t(X) %*% W %*% z 
  b_new <- solve(I) %*% XWz
  error <- b_new - b_start
  b_start <- b_new
}
# Create covariance matrix
I_trained <- solve(I)
# Create beta vector
b_trained <- b_start

################ Testing accuracy of initial model #############################
# print("measuring initial accuracy")
#Creating model matrix for testing data set (X and Y)
X_test <- as.data.frame(model.matrix(~ ., data_testing))

X_test <- X_test %>% 
  select(-ID)

# Creating true Y vector to compare yhat_init with
Y_test_predict <- X_test[ ,ncol(X_test)]

# Creating initial model matrix (without Y) for training data
X_test_predict <- as.matrix(X_test[,-ncol(X_test)])

# Estimating unlabeled dataset, U with logistic regression model
yhat_init <- exp(X_test_predict %*% b_trained)/(1 + exp(X_test_predict %*% b_trained))

#restarting simulation if estimates cant handle outliers resulting in yhat = NaN
if(any(is.na(yhat_init))){
  print('yhat_init is NA, restart sim')
  stop('yhat is NA, restart sim')
}

#Used for mad below
pihat_init <- yhat_init

# creating accuracy 
yhat_init <- yhat_init %>% 
  as.data.frame() %>% 
  rename(yhat = V1) %>%
  # Binding ture Y vector to data frame to compare
  cbind(Y_test_predict) %>% 
  # All observations where yhat > 0.5 is 1.
  mutate(yhat = ifelse(yhat >= 0.5, 1, 0)) %>%
  mutate(same = ifelse(yhat == Y_test_predict, 1, 0)) %>%
  select(same) %>% 
  summarise(sum(same)/length(same)) %>% 
  as.numeric()


#Creating  mad
mad_init <- sum(abs(pihat_init - Y_test_predict))/length(Y_test_predict)


  }, 

#Error function for tryCatch
error = function(e) e)
  
  # TryCatch ends here
  
  # Counter information for while loop about res inheriting an error message
  # print(paste0("TryCath nr ",f))
  f <- f + 1
}

    print(yhat_init)
    print(mad_init)
}

full_data_function(df = rain_data)

setwd("C:/Users/tobia/Dropbox/Master Thesis/Sims/")
bank_data <- read.csv("bank_data_rearranged.csv", sep = ";")
rain_data <- read.csv("rain_data_rearranged.csv", sep = ";")
