<<<<<<< HEAD



criterion_function <- function (sim, 
                                instances,
                                batch,
                                start_sample,
                                criterion,
                                df) {
  
  ######################## INPUT #########################################
  #sim: Number of simulations
  #sample_size: Number of observation to create initial model
  #instances: #Number of new learning instances
  #Batch: How many observations to augment with in each instansce
  
    #Note that output files has to be renamed if using multiple optimal criteria
  
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
  #Store accuracy of predictions which will be presented as average of the simulation
  store_acc <- matrix(0, nrow = sim, ncol = instances + 1)
  #store Mean absolute discrepancy  which will be presented as average of the simulation
  store_mad <- matrix(0, nrow = sim, ncol = instances + 1)
  
  
  #Split data in to training and testing
  
  #testing data set
  data_testing <- df %>%
    head(nrow(df)/2)
  
  #training data set
  data_training <- df %>% 
    tail(nrow(df)/2)
  
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
        labeled_data <- data_training %>% 
          sample_n(., start_sample, replace = F)
        
        #Taking fixed sample - Used for investigating MAD
        #labeled_data <- data_training %>% 
        # slice(1:start_sample)
        
        #Remove observations from training data set
        #Creating new object so that data_train does not shrink for each tryCatch
        suppressMessages(
          data_training1 <- data_training %>% 
            anti_join(labeled_data)
        )
        
        
        #######################################################################
        
        ########### Creating initial X model matrix and Y vector #########
        
        X <- as.data.frame(model.matrix(~ ., labeled_data))
        
        
        #Creating true Y vector for labeled data
        Y <- X[ ,ncol(X)]
        
        #Creating model matrix (withouth Y and ID)
        X <- as.matrix(X[,-c(2,ncol(X))])
        
        #########################################################################
        
        ##################  Initial parameter estimation ######################
        #Generating empty B vector to start iterations in scoring method 
        # print("Estimating initial parameters")
        b_start <- b0
        b_new <- b_start + 1
        error <- b_new - b_start
        while(all(abs(error) >= 1e-10)) {
          #This computation is done for logistic regression only
          W <- diag(as.vector(exp(X %*% b_start)/(1 + exp(X %*% b_start))^2))
          z <- X %*%  b_start + Y * ( (1 + exp( X %*% b_start ))^2 / (exp(X %*% b_start)) ) - ( 1 + exp(X %*% b_start))
          tXW <-  t(X) %*% W
          I <- tXW %*% X
          XWz <- tXW %*% z 
          b_new <- solve(I) %*% XWz
          error <- b_new - b_start
          b_start <- b_new
        }
        #create new paramter vector 
        b_trained <- b_start
        #   }, 
        #  
        #  #Error function for tryCatch
        #   error = function(e) e)
        #  
        #  # TryCatch ends here
        #  
        #  
        #  # Counter information for while loop about res inheriting an error message
        #  # print(paste0("TryCath nr ",f))
        #   f <- f + 1
        # }
        
        ################ Testing accuracy of initial model #############################
        # print("measuring initial accuracy")
        #Creating model matrix for testing data set (X and Y)
        X_test <- as.data.frame(model.matrix(~ ., data_testing))
        
        X_test <- X_test %>% 
          select(-ID)
        
        # Creating true Y vector to compare yhat_init with
        Y_test_predict <- X_test[ ,ncol(X_test)]
        
        # Creating model matrix (without Y) of training data
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
        
        
        # creating initial eaccuracy 
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
        
        # accuracy and mad in each simulation is number of extra observations + initial accuracy estimate
        acc <- rep(0, instances + 1)
        mad <- rep(0, instances + 1)
        
        acc[1] <- yhat_init
        mad[1] <- mad_init
        ############################### DETERMINANT/TRACE #####################
        
        # Creating X_crit_full model matrix, for whole training data set
        X_crit_full <- as.data.frame(model.matrix(~ ., data_training1))
        
        X_crit_full <- X_crit_full %>% 
          select(-ID)
        
        # Remove Y from X_crit_full
        X_crit <- X_crit_full[, -ncol(X_crit_full)] %>% 
          as.matrix()
        
        
        # Below is the exchange algorithm.
        
        repeat{
          
          # This is done batch * [number of observations] times
          # We have to it seperately because determinants may affect each other
          # Each time there is one less observation in the inlabeled data
          for (b in 1:batch){
          
            
            crit_func <- function(a){
              
              #Function that calculates the information criterion 
              #for every observation in the training data set
              
              # New data set with ath observation included in training data
              X_temp_crit <- rbind(X, a)
              
              # calculate GLM weights
              W_crit_func <-  diag(as.vector(exp(X_temp_crit %*% b_trained)/(1 + exp(X_temp_crit %*% b_trained))^2))
              
              # Calculate Information matrix
              I_crit_func <- t(X_temp_crit) %*% W_crit_func %*% X_temp_crit
              
              # Calculate criterion
              crit_func_solve <- solve(I_crit_func)
              criterion(crit_func_solve)
              
            }
            
            #Apply criterion_function to X_crit row-wise
            criterion_vector <- apply(X_crit, 1, crit_func)
            
            #Minimizing criterion
            best_crit_obs <- criterion_vector %>% 
              as.data.frame() %>% 
              mutate(ID = 1:length(criterion_vector)) %>% 
              rename(crit_value = '.') %>% 
              arrange(crit_value) %>% 
              slice(1) %>% 
              select(ID) %>% 
              as.numeric()
            
            
            # Append X with new chosen observation from training data set
            X <- rbind(X, X_crit[best_crit_obs, ])
            
            # Append Y with new chosen correct label
            Y <- append(Y, X_crit_full[best_crit_obs, ncol(X_crit_full)]) %>% 
              as.vector()
            
            # Remove chosen observation from complete unlabeled training data set (with Y)
            # (They are now in L)
            X_crit_full <- X_crit_full[-best_crit_obs, ]
            
            # Remove chosen observation from unlabeled training data set
            # (They are now in L)
            X_crit <- X_crit[-best_crit_obs, ]
          }
          
          
          # New object for testing current observations
          
          #Only X-matrix
          X_exchange <- X_crit
          
          #Full matirx with Y
          X_exchange_full <- X_crit_full
          
        
          # Calculating inverse of Information matrix based on 
          # the new "batch" observations which will change
          W_change <- diag(as.vector(exp(X %*% b_trained)/(1 + exp(X %*% b_trained))^2))
          I_change <- t(X) %*% W_change %*% X
          I_change_solve <- solve(I_change)
          
         
          
           p <- 1
          
          #Criterion to keep algorithm searching for new observation
          I_diff <- -1
        
          # Create new matrix to not interfere with chosen X 
          X_change_model <- X
          
          while(I_diff <= 0 ){
            
            # Storing information used in loop
            best_value <- matrix(0, ncol = 3, nrow = batch)
            
            # print(p)
            
            I_test <- criterion(I_change_solve)
            X_temp_exchange <- X_change_model
            
            for(h in 1:batch){
              
              
              exchange_func <- function(u){
                
                # Input: Unlabeled data
                # Replaces observations in selected data set with new observation
                # Output: vector of criterion values for each observation 
                
                #replace the uth row in X_temp
                X_temp_exchange[(nrow(X_temp_exchange) - batch + h), ] <- u
                
                # Calculate temporary exchange information matrix
                W_temp <- diag(as.vector(exp(X_temp_exchange %*% b_trained)/(1 + exp(X_temp_exchange %*% b_trained))^2))
                
                I_temp <- t(X_temp_exchange) %*% W_temp %*% X_temp_exchange
                
                #Inverse of temporary information matrix
                I_temp_solve <- solve(I_temp)
                
                #Apply criterion to temporary information matrix
                criterion(I_temp_solve)

              }
              
              #Apply exchange function to X_exchange row-wise

              exchange_vector <- apply(X_exchange, 1, exchange_func)
              
              # Selecting the smallest value in each iteration to test
              # the minimization of crit(Information)^-1
              ex_v <- exchange_vector %>% 
                as.data.frame() %>% 
                mutate(ID = 1:length(exchange_vector)) %>% 
                rename(best = '.') %>% 
                arrange(best) %>% 
                slice(1) %>% 
                as.numeric()

              #storing best value, ID and iteration number for every iteration
              best_value[h, 1:2] <- ex_v
              best_value[h, 3] <- h
            }
            
            # When all batch added observations has been compared,
            # replace only the smallest one if it yields a smaller
            # criterion of the information matrix
            
            # print(best_value)
            
            bv_final <- best_value %>%
              as.data.frame() %>% 
              arrange(V1) %>% 
              slice(1) %>% 
              as.numeric()
  
            # print("best value")
            # print(best_value[1])
            # print("I_test")
            # print(I_test)
            
            #Comparing criterion best value in exchange algorithm with model covariance matrix
            if(bv_final[1] < I_test) {
              
              
              #select the observation that is going to be replaced.
              # From labeled to unlabeled data 
              X_change <- X_change_model[(nrow(X_change_model) - batch + bv_final[3]), ]
              
              #Replace the observation in X with the new best value
              X_change_model[(nrow(X_change_model) - batch + bv_final[3]), ] <- X_exchange[bv_final[2], ]
              
              #Put the exchanged value back in to unlabeled data
              X_exchange[bv_final[2], ] <- X_change
              
              #Replacing Y as well (Same method as for X)
              Y_change <- Y[(nrow(X_change_model) - batch + bv_final[3])]
              Y[(nrow(X_change_model) - batch + bv_final[3])] <- X_exchange_full[bv_final[2], ncol(X_exchange_full)]
              X_exchange_full[bv_final[2], ncol(X_exchange_full)] <- Y_change
            
              
              #Calculate new information matrix with new data
              W_change_new <- diag(as.vector(exp(X_change_model %*% b_trained)/(1 + exp(X_change_model %*% b_trained))^2)) 
              I_change_new <- t(X_change_model) %*% W_change_new %*% X_change_model
              I_change_new_solve <- solve(I_change_new)
              
              # Comparing information matrices
              I_diff <- criterion(I_change_new_solve) - criterion(I_change_solve)
              
              I_change_solve <- I_change_new_solve
              
              # print("I_diff")
              # print(I_diff)
  
              p <- p + 1
              
            }
            else{
               # print("smallest possible citerion achieved, break loop")
              break
            }
            
          }
          
          #X_change_model is returned as X
          X <- X_change_model
          
          # Perform scoring method with new observation included 
          # using b0 as starting values.
          b_start <- b0
          b_new <- b_start + 1
          error <- b_new - b_start
          # print(paste0("Estimating new paramter ",g-1))
          while(all(abs(error) >= 1e-10)) {
            #This computation is done for logistic regression only
            W <- diag(as.vector(exp(X %*% b_start)/(1 + exp(X %*% b_start))^2))
            z <- X %*%  b_start + Y * ( (1 + exp( X %*% b_start ))^2 / (exp(X %*% b_start)) ) - ( 1 + exp(X %*% b_start))
            tXW <-  t(X) %*% W
            I <- tXW %*% X
            XWz <- tXW %*% z 
            b_new <- solve(I) %*% XWz
            error <- b_new - b_start
            b_start <- b_new
          }
          #Storing results for new parameter
          b_trained <- b_new
          I_trained <- solve(I)
          
          
          ############## testing acccuracy for each new batch of observations #################
          
          
          #Predict unlabeled testing data data
          yhat_g <- exp(X_test_predict %*% b_trained)/(1 + exp(X_test_predict %*% b_trained))
          
          #Restart simulation if estimates cant handle outliers resulting in yhat = NaN
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
          
          #Calculating mad
          mad_g <- sum(abs(pihat_g - Y_test_predict))/length(Y_test_predict)
          
          #Store gth yhat accuracy and mad
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
  
  ################### #Displaying metrics ###################
  #Saving as data frame
  store_b <- as.data.frame(store_b)
  store_dt <- as.data.frame(store_dt)
  store_acc <- as.data.frame(store_acc)
  store_mad <- as.data.frame(store_mad)
  
  #Naming parameter estimates, determinant and trace
  colnames(store_b) <- rownames(b_trained)
  colnames(store_dt) <- c("Determinant", "Trace")
  
  #Writing acc table as csv.file
  write.table(store_acc, file = paste0("bank_data_algorithm2_det_acc_",format(Sys.time(), "%Y-%m-%d-%H.%M.%S"),".csv"), 
              sep =";", row.names = F)
  
  #We should plot the mean for each new instance selection of of store_acc
  store_acc <- colMeans(store_acc) %>% 
    as.data.frame() %>% 
    rename(Accuracy = '.') %>%
    # Create new vector to plot  mean accuracy measure with
    mutate(Instance = c(0:instances))
  
  
  #Writing mad table as csv.file
  write.table(store_mad, file = paste0("bank_data_algorithm2_det_mad_",format(Sys.time(), "%Y-%m-%d-%H.%M.%S"),".csv"), 
              sep =";", row.names = F)
  
  #Plotting the mean for each new instance selection of of store_mad
  store_mad <- colMeans(store_mad) %>% 
    as.data.frame() %>% 
    rename(mad = '.') %>%
    # Create new vector to plot  mean mad measure with
    mutate(Instance = c(0:instances))
  
  
  #Writing parameter table as csv.file
  write.table(store_b, file = paste0("bank_data_algorithm2_det_parameters_",format(Sys.time(), "%Y-%m-%d-%H.%M.%S"),".csv"), 
              sep =";", row.names = F)
  
  #Writing determinant and trace table as csv.file
  write.table(store_dt, file = paste0("bank_data_algorithm2_det_dt_",format(Sys.time(), "%Y-%m-%d-%H.%M.%S"),".csv"), 
              sep =";", row.names = F)
  
  #writing design matrix as csv.file
  write.table(X, file = paste0("bank_data_trace_X_",format(Sys.time(), "%Y-%m-%d-%H.%M.%S"),".csv"), 
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
  
  #Plotting accuracy for each iteration 
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

criterion_function(sim = 1,
                   instances = 20,
                   start_sample = 50,
                   batch = 5,
                   criterion = tr,
                   df = bank_data)

# For a data set to be compatible with the program:
# The first column must be "ID"
# The last column has to be the y-variable

setwd("C:/Users/tobia/Dropbox/Master Thesis/Sims/")
bank_data <- read.csv("bank_data_rearranged.csv", sep = ";")


library("tidyverse")
library("gridExtra")
library("psych")


=======



criterion_function <- function (sim, 
                                instances,
                                batch,
                                start_sample,
                                criterion,
                                df) {
  
  ######################## INPUT #########################################
  #sim: Number of simulations
  #sample_size: Number of observation to create initial model
  #instances: #Number of new learning instances
  #Batch: How many observations to augment with in each instansce
  
    #Note that output files has to be renamed if using multiple optimal criteria
  
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
  #Store accuracy of predictions which will be presented as average of the simulation
  store_acc <- matrix(0, nrow = sim, ncol = instances + 1)
  #store Mean absolute discrepancy  which will be presented as average of the simulation
  store_mad <- matrix(0, nrow = sim, ncol = instances + 1)
  
  
  #Split data in to training and testing
  
  #testing data set
  data_testing <- df %>%
    head(nrow(df)/2)
  
  #training data set
  data_training <- df %>% 
    tail(nrow(df)/2)
  
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
       # labeled_data <- data_training %>% 
        #  sample_n(., start_sample, replace = F)
        
        #Taking fixed sample
        labeled_data <- data_training %>% 
          slice(1:start_sample)
        
        #Remove observations from training data set
        #Creating new object so that data_train does not shrink for each tryCatch
        suppressMessages(
          data_training1 <- data_training %>% 
            anti_join(labeled_data)
        )
        
        
        #######################################################################
        
        ########### Creating initial X model matrix and Y vector #########
        
        X <- as.data.frame(model.matrix(~ ., labeled_data))
        
        
        #Creating true Y vector for labeled data
        Y <- X[ ,ncol(X)]
        
        #Creating model matrix (withouth Y and ID)
        X <- as.matrix(X[,-c(2,ncol(X))])
        
        #########################################################################
        
        ##################  Initial parameter estimation ######################
        #Generating empty B vector to start iterations in scoring method 
        # print("Estimating initial parameters")
        b_start <- b0
        b_new <- b_start + 1
        error <- b_new - b_start
        while(all(abs(error) >= 1e-10)) {
          #This computation is done for logistic regression only
          W <- diag(as.vector(exp(X %*% b_start)/(1 + exp(X %*% b_start))^2))
          z <- X %*%  b_start + Y * ( (1 + exp( X %*% b_start ))^2 / (exp(X %*% b_start)) ) - ( 1 + exp(X %*% b_start))
          tXW <-  t(X) %*% W
          I <- tXW %*% X
          XWz <- tXW %*% z 
          b_new <- solve(I) %*% XWz
          error <- b_new - b_start
          b_start <- b_new
        }
        #create new paramter vector 
        b_trained <- b_start
        #   }, 
        #  
        #  #Error function for tryCatch
        #   error = function(e) e)
        #  
        #  # TryCatch ends here
        #  
        #  
        #  # Counter information for while loop about res inheriting an error message
        #  # print(paste0("TryCath nr ",f))
        #   f <- f + 1
        # }
        
        ################ Testing accuracy of initial model #############################
        # print("measuring initial accuracy")
        #Creating model matrix for testing data set (X and Y)
        X_test <- as.data.frame(model.matrix(~ ., data_testing))
        
        X_test <- X_test %>% 
          select(-ID)
        
        # Creating true Y vector to compare yhat_init with
        Y_test_predict <- X_test[ ,ncol(X_test)]
        
        # Creating model matrix (without Y) of training data
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
        
        
        # creating initial eaccuracy 
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
        
        # accuracy and mad in each simulation is number of extra observations + initial accuracy estimate
        acc <- rep(0, instances + 1)
        mad <- rep(0, instances + 1)
        
        acc[1] <- yhat_init
        mad[1] <- mad_init
        ############################### DETERMINANT/TRACE #####################
        
        # Creating X_crit_full model matrix, for whole training data set
        X_crit_full <- as.data.frame(model.matrix(~ ., data_training1))
        
        X_crit_full <- X_crit_full %>% 
          select(-ID)
        
        # Remove Y from X_crit_full
        X_crit <- X_crit_full[, -ncol(X_crit_full)] %>% 
          as.matrix()
        
        
        # Below is the exchange algorithm.
        
        repeat{
          
          # This is done batch * [number of observations] times
          # We have to it seperately because determinants may affect each other
          # Each time there is one less observation in the inlabeled data
          for (b in 1:batch){
          
            
            crit_func <- function(a){
              
              #Function that calculates the information criterion 
              #for every observation in the training data set
              
              # New data set with ath observation included in training data
              X_temp_crit <- rbind(X, a)
              
              # calculate GLM weights
              W_crit_func <-  diag(as.vector(exp(X_temp_crit %*% b_trained)/(1 + exp(X_temp_crit %*% b_trained))^2))
              
              # Calculate Information matrix
              I_crit_func <- t(X_temp_crit) %*% W_crit_func %*% X_temp_crit
              
              # Calculate criterion
              crit_func_solve <- solve(I_crit_func)
              criterion(crit_func_solve)
              
            }
            
            #Apply criterion_function to X_crit row-wise
            criterion_vector <- apply(X_crit, 1, crit_func)
            
            #Minimizing criterion
            best_crit_obs <- criterion_vector %>% 
              as.data.frame() %>% 
              mutate(ID = 1:length(criterion_vector)) %>% 
              rename(crit_value = '.') %>% 
              arrange(crit_value) %>% 
              slice(1) %>% 
              select(ID) %>% 
              as.numeric()
            
            
            # Append X with new chosen observation from training data set
            X <- rbind(X, X_crit[best_crit_obs, ])
            
            # Append Y with new chosen correct label
            Y <- append(Y, X_crit_full[best_crit_obs, ncol(X_crit_full)]) %>% 
              as.vector()
            
            # Remove chosen observation from complete unlabeled training data set (with Y)
            # (They are now in L)
            X_crit_full <- X_crit_full[-best_crit_obs, ]
            
            # Remove chosen observation from unlabeled training data set
            # (They are now in L)
            X_crit <- X_crit[-best_crit_obs, ]
          }
          
          
          # New object for testing current observations
          
          #Only X-matrix
          X_exchange <- X_crit
          
          #Full matirx with Y
          X_exchange_full <- X_crit_full
          
        
          # Calculating inverse of Information matrix based on 
          # the new "batch" observations which will change
          W_change <- diag(as.vector(exp(X %*% b_trained)/(1 + exp(X %*% b_trained))^2))
          I_change <- t(X) %*% W_change %*% X
          I_change_solve <- solve(I_change)
          
         
          
           p <- 1
          
          #Criterion to keep algorithm searching for new observation
          I_diff <- -1
        
          # Create new matrix to not interfere with chosen X 
          X_change_model <- X
          
          while(I_diff <= 0 ){
            
            # Storing information used in loop
            best_value <- matrix(0, ncol = 3, nrow = batch)
            
            # print(p)
            
            I_test <- criterion(I_change_solve)
            X_temp_exchange <- X_change_model
            
            for(h in 1:batch){
              
              
              exchange_func <- function(u){
                
                # Input: Unlabeled data
                # Replaces observations in selected data set with new observation
                # Output: vector of criterion values for each observation 
                
                #replace the uth row in X_temp
                X_temp_exchange[(nrow(X_temp_exchange) - batch + h), ] <- u
                
                # Calculate temporary exchange information matrix
                W_temp <- diag(as.vector(exp(X_temp_exchange %*% b_trained)/(1 + exp(X_temp_exchange %*% b_trained))^2))
                
                I_temp <- t(X_temp_exchange) %*% W_temp %*% X_temp_exchange
                
                #Inverse of temporary information matrix
                I_temp_solve <- solve(I_temp)
                
                #Apply criterion to temporary information matrix
                criterion(I_temp_solve)

              }
              
              #Apply exchange function to X_exchange row-wise

              exchange_vector <- apply(X_exchange, 1, exchange_func)
              
              # Selecting the smallest value in each iteration to test
              # the minimization of crit(Information)^-1
              ex_v <- exchange_vector %>% 
                as.data.frame() %>% 
                mutate(ID = 1:length(exchange_vector)) %>% 
                rename(best = '.') %>% 
                arrange(best) %>% 
                slice(1) %>% 
                as.numeric()

              #storing best value, ID and iteration number for every iteration
              best_value[h, 1:2] <- ex_v
              best_value[h, 3] <- h
            }
            
            # When all batch added observations has been compared,
            # replace only the smallest one if it yields a smaller
            # criterion of the information matrix
            
            # print(best_value)
            
            bv_final <- best_value %>%
              as.data.frame() %>% 
              arrange(V1) %>% 
              slice(1) %>% 
              as.numeric()
  
            # print("best value")
            # print(best_value[1])
            # print("I_test")
            # print(I_test)
            
            #Comparing criterion best value in exchange algorithm with model covariance matrix
            if(bv_final[1] < I_test) {
              
              
              #select the observation that is going to be replaced.
              # From labeled to unlabeled data 
              X_change <- X_change_model[(nrow(X_change_model) - batch + bv_final[3]), ]
              
              #Replace the observation in X with the new best value
              X_change_model[(nrow(X_change_model) - batch + bv_final[3]), ] <- X_exchange[bv_final[2], ]
              
              #Put the exchanged value back in to unlabeled data
              X_exchange[bv_final[2], ] <- X_change
              
              #Replacing Y as well (Same method as for X)
              Y_change <- Y[(nrow(X_change_model) - batch + bv_final[3])]
              Y[(nrow(X_change_model) - batch + bv_final[3])] <- X_exchange_full[bv_final[2], ncol(X_exchange_full)]
              X_exchange_full[bv_final[2], ncol(X_exchange_full)] <- Y_change
            
              
              #Calculate new information matrix with new data
              W_change_new <- diag(as.vector(exp(X_change_model %*% b_trained)/(1 + exp(X_change_model %*% b_trained))^2)) 
              I_change_new <- t(X_change_model) %*% W_change_new %*% X_change_model
              I_change_new_solve <- solve(I_change_new)
              
              # Comparing information matrices
              I_diff <- criterion(I_change_new_solve) - criterion(I_change_solve)
              
              I_change_solve <- I_change_new_solve
              
              # print("I_diff")
              # print(I_diff)
  
              p <- p + 1
              
            }
            else{
               # print("smallest possible citerion achieved, break loop")
              break
            }
            
          }
          
          #X_change_model is returned as X
          X <- X_change_model
          
          # Perform scoring method with new observation included 
          # using b0 as starting values.
          b_start <- b0
          b_new <- b_start + 1
          error <- b_new - b_start
          # print(paste0("Estimating new paramter ",g-1))
          while(all(abs(error) >= 1e-10)) {
            #This computation is done for logistic regression only
            W <- diag(as.vector(exp(X %*% b_start)/(1 + exp(X %*% b_start))^2))
            z <- X %*%  b_start + Y * ( (1 + exp( X %*% b_start ))^2 / (exp(X %*% b_start)) ) - ( 1 + exp(X %*% b_start))
            tXW <-  t(X) %*% W
            I <- tXW %*% X
            XWz <- tXW %*% z 
            b_new <- solve(I) %*% XWz
            error <- b_new - b_start
            b_start <- b_new
          }
          #Storing results for new parameter
          b_trained <- b_new
          I_trained <- solve(I)
          
          
          ############## testing acccuracy for each new batch of observations #################
          
          
          #Predict unlabeled testing data data
          yhat_g <- exp(X_test_predict %*% b_trained)/(1 + exp(X_test_predict %*% b_trained))
          
          #Restart simulation if estimates cant handle outliers resulting in yhat = NaN
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
          
          #Calculating mad
          mad_g <- sum(abs(pihat_g - Y_test_predict))/length(Y_test_predict)
          
          #Store gth yhat accuracy and mad
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
  
  ################### #Displaying metrics ###################
  #Saving as data frame
  store_b <- as.data.frame(store_b)
  store_dt <- as.data.frame(store_dt)
  store_acc <- as.data.frame(store_acc)
  store_mad <- as.data.frame(store_mad)
  
  #Naming parameter estimates, determinant and trace
  colnames(store_b) <- rownames(b_trained)
  colnames(store_dt) <- c("Determinant", "Trace")
  
  #Writing acc table as csv.file
  write.table(store_acc, file = paste0("bank_data_algorithm2_det_acc_",format(Sys.time(), "%Y-%m-%d-%H.%M.%S"),".csv"), 
              sep =";", row.names = F)
  
  #We should plot the mean for each new instance selection of of store_acc
  store_acc <- colMeans(store_acc) %>% 
    as.data.frame() %>% 
    rename(Accuracy = '.') %>%
    # Create new vector to plot  mean accuracy measure with
    mutate(Instance = c(0:instances))
  
  
  #Writing mad table as csv.file
  write.table(store_mad, file = paste0("bank_data_algorithm2_det_mad_",format(Sys.time(), "%Y-%m-%d-%H.%M.%S"),".csv"), 
              sep =";", row.names = F)
  
  #Plotting the mean for each new instance selection of of store_mad
  store_mad <- colMeans(store_mad) %>% 
    as.data.frame() %>% 
    rename(mad = '.') %>%
    # Create new vector to plot  mean mad measure with
    mutate(Instance = c(0:instances))
  
  
  #Writing parameter table as csv.file
  write.table(store_b, file = paste0("bank_data_algorithm2_det_parameters_",format(Sys.time(), "%Y-%m-%d-%H.%M.%S"),".csv"), 
              sep =";", row.names = F)
  
  #Writing determinant and trace table as csv.file
  write.table(store_dt, file = paste0("bank_data_algorithm2_det_dt_",format(Sys.time(), "%Y-%m-%d-%H.%M.%S"),".csv"), 
              sep =";", row.names = F)
  
  #writing design matrix as csv.file
  write.table(X, file = paste0("bank_data_trace_X_",format(Sys.time(), "%Y-%m-%d-%H.%M.%S"),".csv"), 
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
  
  #Plotting accuracy for each iteration 
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

criterion_function(sim = 1,
                   instances = 20,
                   start_sample = 50,
                   batch = 5,
                   criterion = tr,
                   df = bank_data)

# For a data set to be compatible with the program:
# The first column must be "ID"
# The last column has to be the y-variable

setwd("C:/Users/tobia/Dropbox/Master Thesis/Sims/")
bank_data <- read.csv("bank_data_rearranged.csv", sep = ";")


library("tidyverse")
library("gridExtra")
library("psych")


>>>>>>> 4d54b236575cd5b24eb5d4079283aeb42a839d51
