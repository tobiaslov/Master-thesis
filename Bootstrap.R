library(tidyverse)


#Bootstrap function

Bootstrap_function <- function(df,
                               sample_size,
                               sims) {
  #Input: 
         #df: Data input: Must be 1 dimension
         #sample_size: Size of resample. Same length as nrow(data input)
         #sims: How man times to do resample
  
  #Output:
        #File with simulations of bootstrap means
        #Histogram of bootstrap means
  data <- df
  
  #Matrix to store bootstrap simulations in
  store_sample <- matrix(0, nrow = nrow(data), ncol = sims)
  
  #Vector for ith sample
  s <- rep(0, nrow(data))
  
  for (i in 1:sims) {
    
    #Take ith sample
    s <- sample_n(data, sample_size, replace = T)
    s <- as.numeric(unlist(s))
  
    #Store ith sample
    store_sample[ ,i] <- s
  }
  
  #Take means of each simulated sample
  store_sample <- colMeans(store_sample) %>%
    as.data.frame()
  
  #Output as table sample - Change name
  write.table(store_sample, file = "bank_data_A1_trace_bootstrap.csv", sep = ";")
  
  #Histogram of means
  store_sample %>% 
    rename(means = ".") %>% 
    ggplot(aes (x = means)) + 
    geom_histogram(bins = 30, position = "identity")
    
}

Bootstrap_function(df = bank_mad_trace_data,
                   sample_size = 100,
                   sims = 100000)


####################################### MAD #############################
# D-optimality algorithm file
setwd("C:/Users/tobia/Dropbox/Master Thesis/Sims/Bank_data_results/MAD/A2_Det")

bank_mad_det_data <- read.csv("bank_data_A2_det_mad.csv", sep = ";") %>%
  select(V51)

# A-optimality algorithm file
setwd("C:/Users/tobia/Dropbox/Master Thesis/Sims/Bank_data_results/MAD/A2_Trace")
bank_mad_trace_data <- read.csv("bank_data_A2_trace_mad.csv", sep = ";") %>% 
  select(V51)

# Random algorithm  file
setwd("C:/Users/tobia/Dropbox/Master Thesis/Sims/Bank_data_results/MAD/A1")

bank_mad_rand_data <- read.csv("bank_data_A1_mad.csv", sep = ";") %>% 
  select(V51)



###################################### ACCURACY #########################
# D-optimality algorithm file
setwd("C:/Users/tobia/Dropbox/Master Thesis/Sims/Bank_data_results/Accuracy/A2_det")

bank_acc_det_data <- read.csv("bank_data_A2_det_acc.csv", sep = ";") %>%
  select(V51)

# A-optimality algorithm file
setwd("C:/Users/tobia/Dropbox/Master Thesis/Sims/Bank_data_results/Accuracy/A2_trace")
bank_acc_trace_data <- read.csv("bank_data_A2_trace_acc.csv", sep = ";") %>% 
  select(V51)

# Random algorithm  file
setwd("C:/Users/tobia/Dropbox/Master Thesis/Sims/Bank_data_results/Accuracy/A1")

bank_acc_rand_data <- read.csv("bank_data_A1_acc.csv", sep = ";") %>% 
  select(V51)

  
  
  ##################################################################*

setwd("C:/Users/tobia/Dropbox/Master Thesis/Sims/Bank_data_results/Accuracy")

bank_data_acc_G_full <- bank_acc_rand_data %>% 
  rbind(bank_acc_trace_data, bank_acc_det_data)


write.table(bank_data_acc_G_full, file = "bank_data_acc_G_full", sep = ";", row.names = F)

bank_data_acc_G_full <- read.csv("bank_data_acc_G_full", sep = ";") %>% 
  rename(Accuracy = V51)
