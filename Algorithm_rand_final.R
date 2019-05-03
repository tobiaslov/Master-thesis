library("tidyverse")
library("gridExtra")
library("psych")



random_function <- function (sim, 
                             instances,
                             batch,
                             sample_size, 
                             df) {
  
  ######################## INPUT #########################################
  #sim: Number of simulations
  #sample_size: Number of observation to create initial model
  #instances: #Number of new learning instances
  #Batch: How many observations to augment with in each instansce
  
  ######################### OUTPUT ######################################
  #Files  and/or graphs of:
    #Accuracy
    #MAD
    #Determinant and Trace of covariance matrix
    #Parameter distribution
  
  ######################################################################

  start_time <- Sys.time()
  
  
  ##############################################################
  #Create parameter vector (length is model matrix -2 because of ID and Y)
  b0 <- rep(0, ncol(model.matrix(~ ., df)) - 2)
  #Matrix to store parameter estimates in in with nrow = #simulations
  store_b <- matrix(0, nrow = sim, ncol = length(b0))
  # store determinant and trace of covariance matrix
  store_dt <- matrix(0, nrow = sim, ncol = 2)
  #Store accuracy of predictions which will be average of each simulation
  store_acc <- matrix(0, nrow = sim, ncol = instances + 1)
  #store Mean squared error (mad) which will be presented as average of the simulation
  store_mad <- matrix(0, nrow = sim, ncol = instances + 1)
  
  #Split data in to training and testing
  
  #testing data set
  data_testing <- df %>%
    head(nrow(df)/2)
  
  #training data set
  data_training <- df %>% 
    tail(nrow(df)/2)
  
  #Starting simulations
  for(i in 1:sim) {
    print(paste0("Simulation nr ", i))
    
    
    #When taking a sample from the trianing set, initial parameters may not be estimatable
    #Solving with a tryCatch function to always get working data
    res <- simpleError("Fake error")
    f <- 1
    
    #While res contains some error
    while(inherits(res, "error")) {
      
      #Create a new entity res that resets the loop every time the functions returns errors.
      #Performs both the sample of the data and the first estimation 
      res <- tryCatch({ 
        #TryCatch starts
        
        #Sampling initial data set
        #labeled_data <- data_training %>% 
        #  sample_n(., sample_size, replace = F)
        
        #Taking fixed sample
        labeled_data <- data_training %>% 
          slice(1:sample_size)
        
        #Remove observations from training data set
        #Creating new object so that data_train does not shrink for each tryCatch
        suppressMessages(
          data_training1 <- data_training %>% 
            anti_join(labeled_data)
        )
        
        
        #######################################################################
        
        ########### Creating x  model matrix and Y vector #######################
        
        # Creating model matrix from the starting sample
        
        X <- as.data.frame(model.matrix(~ ., labeled_data))
        
        #Creating true Y vector for labeled data
        Y <- X[ ,ncol(X)]
        #Creating model matrix (withouth Y and ID)
        X <- as.matrix(X[,-c(2,ncol(X))])
        
        #########################################################################
        
        ##################  Initial parameter estimation ######################
        
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
        
        
        #Creating initial mad
        mad_init <- sum(abs(pihat_init - Y_test_predict))/length(Y_test_predict)
        
        #########################################################################
        
        ####################### Augment with new data ############################
        g <- 2
        
        # accuracy and mad in each simulation is number of 
        # extra observations + initial accuracy estimate
        acc <- rep(0, instances + 1)
        mad <- rep(0, instances + 1)
        
        acc[1] <- yhat_init
        mad[1] <- mad_init
        
        
        repeat{
          obs <- data_training1 %>%
            # Take 'batch' new observation randomly and tell which on it is
            sample_n(., batch, replace = F)
          
          #Remove new observations from training data set
          suppressMessages(
            data_training1 <- data_training1 %>% 
              anti_join(obs)
          )
          
          # Transform into new design matrix ROW
          X_new <- as.data.frame(model.matrix(~ ., obs))
          
          # Append to new Y value to labeled Y vector 
          # This is done by the Oracle in real projects. 
          Y <- append(Y, X_new[ ,ncol(X_new)])
          
          #Tidy new observation and bind to model matrix (removing Y and ID)
          X_new <- as.matrix(X_new[,-c(2,ncol(X_new))])
          X <- rbind(X,X_new)
          
          # Perform scoring method with new observation included 
          # using b0 as starting values. 
          # print("estimate new parameters")
          b_start <- b0
          b_new <- b_start + 1
          error <- b_new - b_start
          
          while(all(abs(error) >= 1e-10)) 
          {
            #This computation is done for logistic regression only
            W <- diag(as.vector(exp(X %*% b_start)/(1 + exp(X %*% b_start))^2))
            z <- X %*%  b_start + Y * ( (1 + exp(X %*% b_start ))^2 / (exp(X %*% b_start)) ) - ( 1 + exp(X %*% b_start))
            I <- t(X) %*% W %*% X
            XWz <- t(X) %*% W %*% z 
            b_new <- solve(I) %*% XWz
            error <- b_new - b_start
            b_start <- b_new
            
          }
          #Storing results for new parameter
          b_trained <- b_new
          I_trained <- solve(I)
          
          ############## testing acccuracy for each new observation #################
          
          
          #Predict unlabeled data
          yhat_g <- exp(X_test_predict %*% b_trained)/(1 + exp(X_test_predict %*% b_trained))
          
          
          #restarting simulation if estimates cant handle outliers resulting in yhat = NaN
          if(any(is.na(yhat_g))){
            print('yhat_g is NA, restart sim')
            stop('yhat is NA, restart sim')
          }
          
          # gth pihat to calculate gth mad
          pihat_g <- yhat_g
          
          # creating eaccuracy measure
          yhat_g <- yhat_g %>% 
            as.data.frame() %>% 
            rename(yhat = V1) %>%
            cbind(Y_test_predict) %>% 
            # All observations where yhat > 0.5 is 1 IS THIS NAIVE?
            mutate(yhat = ifelse(yhat >= 0.5, 1, 0)) %>%
            mutate(same = ifelse(yhat == Y_test_predict, 1, 0)) %>% 
            select(same) %>% 
            summarise(sum(same)/length(same)) %>% 
            as.numeric()
          
          
          mad_g <- sum(abs(pihat_g - Y_test_predict))/length(Y_test_predict)
          
          #Store gth yhat accuracy
          acc[g] <- yhat_g
          mad[g] <- mad_g
          
          #Counting iterations
          g <- g + 1
          
          # When number of instances has been reached, stop
          if(nrow(X) == nrow(labeled_data) + instances*batch ){
            break
          }
          
        }
        #Storing ith simulation parameter vector, determinant, trace and accuracy
        store_b[i, ] <- b_trained
        store_dt[i,1] <- det(I_trained)
        store_dt[i,2] <- sum(diag(I_trained))
        store_acc[i, ] <- acc
        store_mad[i, ] <- mad
        

      }, 
      
      #Error function for tryCatch
      error = function(e) e)
      
      # TryCatch ends here
      
      
      # Counter information for while loop about res inheriting an error message
      # print(paste0("TryCath nr ",f))
      f <- f + 1
    }
  }
  
  #################### Displaying metrics ###################
  
  #Saving as data frame
  store_b <- as.data.frame(store_b)
  store_dt <- as.data.frame(store_dt)
  store_acc <- as.data.frame(store_acc)
  store_mad <- as.data.frame(store_mad)
  
  #Naming parameter estimates, determinant and trace
  colnames(store_b) <- rownames(b_trained)
  colnames(store_dt) <- c("Determinant", "Trace")
  
  #Writing acc table as csv.file
  write.table(store_acc, file = paste0("bank_data_algorithm1_acc_",format(Sys.time(), "%Y-%m-%d-%H.%M.%S"),".csv"), 
              sep =";", row.names = F)
  
  #Plotting the mean for each new instance selection of of store_acc
  store_acc <- colMeans(store_acc) %>% 
    as.data.frame() %>% 
    rename(Accuracy = '.') %>%
    # Create new vector to plot  mean accuracy measure with
    mutate(Instance = c(0:instances))
  
  #Writing mad table as csv.file
  write.table(store_mad, file = paste0("bank_data_algorithm1_mad_",format(Sys.time(), "%Y-%m-%d-%H.%M.%S"),".csv"), 
              sep =";", row.names = F)
  
  #Plotting the mean for each new instance selection of of store_mad
  store_mad <- colMeans(store_mad) %>% 
    as.data.frame() %>% 
    rename(mad = '.') %>%
    # Create new vector to plot  mean mad measure with
    mutate(Instance = c(0:instances))
  
  #Writing parameter table as csv.file
  write.table(store_b, file = paste0("bank_data_algorithm1_parameters_",format(Sys.time(), "%Y-%m-%d-%H.%M.%S"),".csv"), 
              sep =";", row.names = F)
  
  #Writing determinant and trace table as csv.file
  write.table(store_dt, file = paste0("bank_data_algorithm1_dt_",format(Sys.time(), "%Y-%m-%d-%H.%M.%S"),".csv"), 
              sep =";", row.names = F)
  
  #writing design matrix as csv.file
  write.table(X, file = paste0("bank_data_rand_X",format(Sys.time(), "%Y-%m-%d-%H.%M.%S"),".csv"), 
             sep =";", row.names = F)
  
  
  #Plotting historgrams of parameters, determinant, trace and accuracy
  
  p_plot <-  store_b %>%
    #gather() turns data frame in to 1 2x(15*100) data frame for ggplot
    gather() %>% 
    ggplot( aes(x = value)) + 
    facet_wrap(~ key, scales = "free_x") + 
    geom_histogram(bins = 30)
  
  #Plotting historgrams of determinant and trace
  dt_plot <- store_dt %>%
    gather() %>% 
    ggplot( aes(x = value)) + 
    facet_wrap(~ key, scales = "free_x") + 
    geom_histogram(bins = 30)
  
  #Plotting accuracy for each iteration (average over simulations)
  acc_plot <- store_acc %>% 
    ggplot( aes(x = Instance , y = Accuracy)) +
    geom_point() +
    geom_line()
  
  #Plotting mad for each iteration (average over simulations)
  mad_plot <- store_mad %>% 
    ggplot( aes(x = Instance , y = mad)) +
    geom_point() +
    geom_line()
  
  grid.arrange(p_plot, mad_plot, acc_plot, widths = 1:1)
  
  stop_time <- Sys.time()
  stop_time - start_time
}

random_function(sim = 1,
                instances = 20,
                sample_size = 50,
                batch = 5,
                df = bank_data)

# For a data set to be compatible with the program:
# The first column must be "ID"
# The last column has to be the y-variable
setwd("C:/Users/tobia/Dropbox/Master Thesis/Sims/")
bank_data <- read.csv("bank_data_rearranged.csv", sep = ";")


