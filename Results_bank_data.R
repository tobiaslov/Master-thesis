library(tidyverse)
library(GGally)
library("gridExtra")


############################################################
#                                                          #
#                              Bank data                   #
#                                                          #
############################################################

################################### Accuracy #######################################

# Reading D-optimality accuracy files
setwd("C:/Users/tobia/Dropbox/Master Thesis/Sims/Bank_data_results/Accuracy/A2_det")
a2_det_list <- list.files()

#Combining all simulations into one file for D-optimality
a2_det_data <- do.call(rbind, lapply(a2_det_list, function(x) read.csv(file = x, sep = ";")))

write.table(a2_det_data, file = "bank_data_A2_det_acc.csv", sep = ";")


# Reading A-optimality accuracy files
setwd("C:/Users/tobia/Dropbox/Master Thesis/Sims/Bank_data_results/Accuracy/A2_trace")
a2_trace_list <- list.files()

#Combining all simulations into one file for A-optimality
a2_trace_data <- do.call(rbind, lapply(a2_trace_list, function(x) read.csv(file = x, sep = ";")))

write.table(a2_trace_data, file = "bank_data_A2_trace_acc.csv", sep = ";")


# Reading Random algorithm files
setwd("C:/Users/tobia/Dropbox/Master Thesis/Sims/Bank_data_results/Accuracy/A1")

a1_data <- read.csv("bank_data_algorithm1_acc_2019-04-06-09.47.39.csv", sep = ";")

write.table(a1_data, file = "bank_data_A1_acc.csv", sep = ";", row.names = F)


################ Summarizing ACCURACY #############
# Random algorithm Accuracy
setwd("C:/Users/tobia/Dropbox/Master Thesis/Sims/Bank_data_results/Accuracy/A1")
a1_acc_summary <- read.csv("bank_data_A1_acc.csv", sep = ";") %>% 
  colMeans() %>% 
  as.data.frame() %>%
  rename(Accuracy = ".") %>% 
  mutate(Group = "Random",
         Instance = c(0:50))


# D-optimality Accuracy
setwd("C:/Users/tobia/Dropbox/Master Thesis/Sims/Bank_data_results/Accuracy/A2_det")
a2_det_acc_summary <-  read.csv("bank_data_A2_det_acc.csv", sep = ";") %>% 
  colMeans() %>% 
  as.data.frame() %>%
  rename(Accuracy = ".") %>% 
  mutate(Group = "D-optimality",
         Instance = c(0:50))


# A-optimality Accuracy
setwd("C:/Users/tobia/Dropbox/Master Thesis/Sims/Bank_data_results/Accuracy/A2_trace")
a2_trace_acc_summary <-  read.csv("bank_data_A2_trace_acc.csv", sep = ";")  %>% 
  colMeans() %>% 
  as.data.frame() %>%
  rename(Accuracy = ".") %>% 
  mutate(Group = "A-optimality",
         Instance = c(0:50))

#Combining all three data sets
acc_data_summary <- a1_acc_summary %>% 
  rbind(a2_trace_acc_summary,a2_det_acc_summary) 

#Writing combined accuracy table
setwd("C:/Users/tobia/Dropbox/Master Thesis/Sims/Bank_data_results/Accuracy/")
write.table(acc_data_summary, file = "acc_data_summary.csv", sep = ";", row.names = F)


acc_data_summary <- read.csv("acc_data_summary.csv", sep = ";")


#Plotting Accuracy for both algorithms
acc_data_summary %>% 
  ggplot(aes ( x = Instance, y = Accuracy, group = Group, 
               color = Group, shape = Group)) +
  geom_point() +
  geom_line() +
  geom_hline(yintercept = .8268102)


################ TUKEY TEST ######################################### Did not use 
bank_data_acc_G_full <- read.csv("bank_data_acc_G_full", sep = ";") %>% 
  rename(Accuracy = V51)
  
bank_data_acc_G_full %>% 
ggplot(aes ( x = Accuracy, fill = Group)) +
  geom_histogram(position = "identity")

a1 <- aov(Accuracy ~ Group, data = bank_data_acc_G_full)
summary(a1)

TukeyHSD(a1)

###################### SHAPIRO ############################# Did not use

b_det <- bank_data_acc_G_full %>% 
  filter(Group == "Det")
shapiro.test(b_det[ ,1])

b_tr <- bank_data_acc_G_full %>% 
  filter(Group == "Trace")
shapiro.test(b_tr[ ,1])

b_rand <- bank_data_acc_G_full %>% 
  filter(Group == "Random")
shapiro.test(b_rand[ ,1])


############### PROPORTION TEST #################### Did not use

bank_data_acc_G_full_means <- bank_data_acc_G_full %>% 
  group_by(Group) %>% 
  summarise(prop = mean(Accuracy)) %>% 
  mutate(Correct = round(prop*3067, 0), 
         Incorrect = round(3067 - Correct, 0),
         Total = Correct + Incorrect)

Count_data <- bank_data_acc_G_full_means[1:3,3:4] %>% 
  as.data.frame()

  rownames(Count_data)  <- c("Det", "Random", "Trace")

  Count_data <- as.matrix(Count_data)

chisq.test(Count_data)
fisher.test(Count_data)

prop.test(Count_data)


################################## Mann-Whitney ############# Did not use

wilcox.test(b_tr[,1],b_det[ ,1])


################################### Kruskal-Wallis test ############## Did not use

kruskal.test(Accuracy ~ Group, data = bank_data_acc_G_full)


############################# Histogram of Accuracy at Last instance #################

############################ WITH BOOTSTRAP #########################################


# Random algorithm Accuracy Bootstrap
setwd("C:/Users/tobia/Dropbox/Master Thesis/Sims/Bank_data_results/Accuracy/A1")
a1_acc_bootstrap <- read.csv("bank_data_A1_bootstrap.csv", sep = ";") %>% 
  mutate(Group = "Random") %>% 
  rename(Accuracy = ".")

#Taking 95% of data simulated
a1_acc_bootstrap_95 <- a1_acc_bootstrap %>%
  arrange(desc(Accuracy)) %>% 
  slice(-1:-2500) %>% 
  slice(-95001:-97500)
  
# A-optimality Accuracy Bootstrap
setwd("C:/Users/tobia/Dropbox/Master Thesis/Sims/Bank_data_results/Accuracy/A2_trace")
a2_trace_acc_bootstrap <-  read.csv("bank_data_A2_trace_bootstrap.csv", sep = ";") %>% 
  mutate(Group = "A-optimality") %>% 
  rename(Accuracy = ".")

#Taking 95% of data simulated
a2_trace_acc_bootstrap_95 <- a2_trace_acc_bootstrap %>% 
  arrange(desc(Accuracy)) %>% 
  slice(-1:-2500) %>% 
  slice(-95001:-97500)

#D-optimality Accuracy Bootstrap
setwd("C:/Users/tobia/Dropbox/Master Thesis/Sims/Bank_data_results/Accuracy/A2_det")
a2_det_acc_bootstrap <-  read.csv("bank_data_A2_det_bootstrap.csv", sep = ";") %>% 
  mutate(Group = "D-optimality") %>% 
  rename(Accuracy = ".")


#Taking 95% of data simulated
a2_det_acc_bootstrap_95 <- a2_det_acc_bootstrap %>% 
  arrange(desc(Accuracy)) %>% 
  slice(-1:-2500) %>% 
  slice(-95001:-97500)


############################# Boostrap long format #########################

# Combining Bootstrap means of last instance
Acc_bootstrap_G <- a1_acc_bootstrap %>%
  rbind(a2_trace_acc_bootstrap,a2_det_acc_bootstrap)

#Combining 95 % CI Bootstrap means of last instance
Acc_bootstrap_G_95 <- a1_acc_bootstrap_95 %>%
  rbind(a2_trace_acc_bootstrap_95,a2_det_acc_bootstrap_95)

#Write tables
setwd("C:/Users/tobia/Dropbox/Master Thesis/Sims/Bank_data_results/Accuracy")

#Full bootstrap file
write.table(Acc_bootstrap_G, file = "acc_data_summary_bootstrap.csv", sep = ";", row.names = F)
#95 % bootstrap file
write.table(Acc_bootstrap_G_95, file = "acc_data_summary_bootstrap_95.csv", sep = ";", row.names = F)

###################### Bootrap wide format ###################### Did not use

#Combining files by cbind instead
Acc_bootstrap_G_wide <-  a1_acc_bootstrap %>%
  cbind(a2_trace_acc_bootstrap,a2_det_acc_bootstrap)

#Greating new column names
colnames(Acc_bootstrap_G_wide)[c(1,3,5)] <- c("Random", "Trace", "Det")
colnames(Acc_bootstrap_G_wide)[c(2,4,6)] <- c("g1", "g2", "g3")

#Removing unnecessary columns
Acc_bootstrap_G_wide <- Acc_bootstrap_G_wide %>%
  select(-c(g1, g2, g3))
         
#Writing table
write.table(Acc_bootstrap_G_wide, file = "acc_data_summary_bootstrap_wide.csv", sep = ";", row.names = F)


######################## Summary of Boostrap long format ###########################

Acc_bootstrap_G <- read.csv("acc_data_summary_bootstrap.csv", sep = ";")

#Histogram of bootstrap means
Acc_bootstrap_G %>%
  ggplot(aes (x = Accuracy, fill = Group)) + 
  geom_histogram(bins = 30, alpha = .9, position = "identity")

#95 distributions
Acc_bootstrap_G_95 <- read.csv("acc_data_summary_bootstrap_95.csv", sep = ";")

#Getting more decimals
apply(Acc_bootstrap_G_95, 2, range, digits = 4)

Acc_bootstrap_G_95 %>% 
  group_by(Group) %>% 
  summarise(minacc = sprintf("%1f", min(Accuracy)),
            maxacc = sprintf("%1f", max(Accuracy)))

#Plotting 95% distributions of bootstrap means
Acc_bootstrap_G_95 %>%
  ggplot(aes (x = Accuracy, fill = Group)) + 
  geom_histogram(bins = 30, alpha = .9, position = "identity")


################################### MAD ###########################################

# D-optimality mad files
setwd("C:/Users/tobia/Dropbox/Master Thesis/Sims/Bank_data_results/MAD/A2_Det")
a2_det_list <- list.files()

#Combining all simulations into one file for D-optimality
a2_det_data <- do.call(rbind, lapply(a2_det_list, function(x) read.csv(file = x, sep = ";")))
write.table(a2_det_data, file = "bank_data_A2_det_mad.csv", sep = ";")


#A-optimality mad files
setwd("C:/Users/tobia/Dropbox/Master Thesis/Sims/Bank_data_results/MAD/A2_trace")
a2_trace_list <- list.files()

#Combining all simulations into one file for A-optimality
a2_trace_data <- do.call(rbind, lapply(a2_trace_list, function(x) read.csv(file = x, sep = ";")))
write.table(a2_trace_data, file = "bank_data_A2_trace_mad.csv", sep = ";")


#Random algorithm mad files
setwd("C:/Users/tobia/Dropbox/Master Thesis/Sims/Bank_data_results/MAD/A1")
a1_data <- read.csv("bank_data_algorithm1_mad_2019-04-06-09.47.39.csv", sep = ";")

write.table(a1_data, file = "bank_data_A1_mad.csv", sep = ";", row.names = F)


################ Summarizing MAD ##############################

# andom algorithm file
setwd("C:/Users/tobia/Dropbox/Master Thesis/Sims/Bank_data_results/MAD/A1")
a1_mad_summary <- read.csv("bank_data_A1_mad.csv", sep = ";") %>% 
  colMeans() %>% 
  as.data.frame() %>%
  rename(MAD = ".") %>% 
  mutate(Group = "Random",
         Instance = c(0:50))

# D-optimality file
setwd("C:/Users/tobia/Dropbox/Master Thesis/Sims/Bank_data_results/MAD/A2_det")
a2_det_mad_summary <-  read.csv("bank_data_A2_det_mad.csv", sep = ";") %>% 
  colMeans() %>% 
  as.data.frame() %>%
  rename(MAD = ".") %>% 
  mutate(Group = "D-optimality",
         Instance = c(0:50))

#A-optimality file
setwd("C:/Users/tobia/Dropbox/Master Thesis/Sims/Bank_data_results/MAD/A2_Trace")
a2_trace_mad_summary <-  read.csv("bank_data_A2_trace_mad.csv", sep = ";") %>% 
  colMeans() %>% 
  as.data.frame() %>%
  rename(MAD = ".") %>% 
  mutate(Group = "A-optimality",
         Instance = c(0:50))

#Combining MAD for all methods into one file
mad_data_summary <- a1_mad_summary %>% 
  rbind(a2_trace_mad_summary,a2_det_mad_summary) 

setwd("C:/Users/tobia/Dropbox/Master Thesis/Sims/Bank_data_results/MAD/")
write.table(mad_data_summary, file = "mad_data_summary.csv", sep = ";", row.names = F)

mad_data_summary <- read.csv("mad_data_summary.csv", sep = ";")

#Summarizing mad with plots.
mad_data_summary %>% 
  ggplot(aes ( x = Instance, y = MAD, group = Group, 
               color = Group, shape = Group)) +
  geom_point() +
  geom_line() +
  geom_hline(yintercept = 0.2603522)

mad_data_summary %>% 
  filter(Instance == 50)

########################################## MAD BOOTSTRAP ###############################

# Random algorithm Accuracy Bootstrap
setwd("C:/Users/tobia/Dropbox/Master Thesis/Sims/Bank_data_results/MAD/A1")
a1_mad_bootstrap <- read.csv("bank_data_A1_bootstrap.csv", sep = ";") %>% 
  mutate(Group = "Random") %>% 
  rename(MAD = ".")

#Taking 95% of data simulated
a1_mad_bootstrap_95 <- a1_mad_bootstrap %>%
  arrange(desc(MAD)) %>% 
  slice(-1:-2500) %>% 
  slice(-95001:-97500)

# A-optimality Accuracy Bootstrap
setwd("C:/Users/tobia/Dropbox/Master Thesis/Sims/Bank_data_results/MAD/A2_Trace")
a2_trace_mad_bootstrap <-  read.csv("bank_data_A2_trace_bootstrap.csv", sep = ";") %>%
  mutate(Group = "A-optimality") %>% 
  rename(MAD = ".")

#Taking 95% of data simulated
a2_trace_mad_bootstrap_95 <- a2_trace_mad_bootstrap %>% 
  arrange(desc(MAD)) %>% 
  slice(-1:-2500) %>% 
  slice(-95001:-97500)

#D-optimality Accuracy Bootstrap
setwd("C:/Users/tobia/Dropbox/Master Thesis/Sims/Bank_data_results/MAD/A2_Det")
a2_det_mad_bootstrap <-  read.csv("bank_data_A2_det_bootstrap.csv", sep = ";") %>% 
  mutate(Group = "D-optimality") %>% 
  rename(MAD = ".")


#Taking 95% of data simulated
a2_det_mad_bootstrap_95 <- a2_det_mad_bootstrap %>% 
  arrange(desc(MAD)) %>% 
  slice(-1:-2500) %>% 
  slice(-95001:-97500)

# Combining Bootstrap means of last instance
mad_bootstrap_G <- a1_mad_bootstrap %>%
  rbind(a2_trace_mad_bootstrap,a2_det_mad_bootstrap)

#Combining 95 % CI Bootstrap means of last instance
mad_bootstrap_G_95 <- a1_mad_bootstrap_95 %>%
  rbind(a2_trace_mad_bootstrap_95,a2_det_mad_bootstrap_95)

#Write tables
setwd("C:/Users/tobia/Dropbox/Master Thesis/Sims/Bank_data_results/MAD")

#Full bootstrap file
write.table(mad_bootstrap_G, file = "mad_data_summary_bootstrap.csv", sep = ";", row.names = F)
#95 % bootstrap file
write.table(mad_bootstrap_G_95, file = "mad_data_summary_bootstrap_95.csv", sep = ";", row.names = F)


############################# MAD BOOTSTRAP VISUALIZATION ###################
setwd("C:/Users/tobia/Dropbox/Master Thesis/Sims/Bank_data_results/MAD")
mad_bootstrap_G <- read.csv("mad_data_summary_bootstrap.csv", sep = ";")

#Histogram of bootstrap means
mad_bootstrap_G %>%
  ggplot(aes (x = MAD, fill = Group)) + 
  geom_histogram(bins = 30, alpha = .9, position = "identity")

#95 distributions
mad_bootstrap_G_95 <- read.csv("mad_data_summary_bootstrap_95.csv", sep = ";")

#Getting more decimals
apply(mad_bootstrap_G_95, 2, range, digits = 4)

mad_bootstrap_G_95 %>% 
  group_by(Group) %>% 
  summarise(minacc = sprintf("%1f", min(MAD)),
            maxacc = sprintf("%1f", max(MAD)))

#Plotting 95% distributions of bootstrap means
Acc_bootstrap_G_95 %>%
  ggplot(aes (x = Accuracy, fill = Group)) + 
  geom_histogram(bins = 30, alpha = .9, position = "identity")



################################ Determinant / Trace Histograms ##########################
#Did not use


# A_2_det
setwd("C:/Users/tobia/Dropbox/Master Thesis/Sims/Bank_data_results/Det_Trace/A2_Det")

a2_det_list <- list.files()

a2_det_data <- do.call(rbind, lapply(a2_det_list, function(x) read.csv(file = x, sep = ";")))

write.table(a2_det_data, file = "bank_data_A2_det_dt.csv", sep = ";")

# A_2_trace
setwd("C:/Users/tobia/Dropbox/Master Thesis/Sims/Bank_data_results/Det_Trace/A2_Trace")

a2_trace_list <- list.files()

a2_trace_data <- do.call(rbind, lapply(a2_trace_list, function(x) read.csv(file = x, sep = ";")))

write.table(a2_trace_data, file = "bank_data_A2_trace_dt.csv", sep = ";")

#A1 
setwd("C:/Users/tobia/Dropbox/Master Thesis/Sims/Bank_data_results/Det_Trace/A1")

a1_data <- read.csv("bank_data_algorithm1_dt_2019-04-06-09.47.39.csv", sep = ";")

write.table(a1_data, file = "bank_data_A1_dt.csv", sep = ";", row.names = F)



################ Determinant / Trace Histograms ############################## 
#Did not use

# A1
setwd("C:/Users/tobia/Dropbox/Master Thesis/Sims/Bank_data_results/Det_Trace/A1")
a1_dt_summary <- read.csv("bank_data_A1_dt.csv", sep = ";") %>% 
  mutate(Group = "Random")

# A2 Det
setwd("C:/Users/tobia/Dropbox/Master Thesis/Sims/Bank_data_results/Det_Trace/A2_Det")
a2_det_dt_summary <-  read.csv("bank_data_A2_det_dt.csv", sep = ";") %>% 
  mutate(Group = "Det")

# A2 Trace
setwd("C:/Users/tobia/Dropbox/Master Thesis/Sims/Bank_data_results/Det_Trace/A2_Trace")
a2_trace_dt_summary <-  read.csv("bank_data_A2_trace_dt.csv", sep = ";") %>% 
  mutate(Group = "Trace")


dt_data_summary <- a1_dt_summary %>% 
  rbind(a2_trace_dt_summary,a2_det_dt_summary) 

setwd("C:/Users/tobia/Dropbox/Master Thesis/Sims/Bank_data_results/Det_Trace/")
write.table(dt_data_summary, file = "dt_data_summary.csv", sep = ";", row.names = F)


dt_data_summary <- read.csv("dt_data_summary.csv", sep = ";")
 dt_data_summary %>%
  # subset(., Trace < 200) %>% 
  # subset(., Determinant < 1e-20) %>%
  filter( Group == "Random") %>% 
  ggplot(aes (x = Trace)) + 
  geom_histogram(bins = 30, alpha = .5, position = "identity")
 
 
 ################ Investigating design matrix g=20 ###########################
 setwd("C:/Users/tobia/Dropbox/Master Thesis/Sims/Bank_data_results/MAD_g20/")
 
x_rand <- read.csv("bank_data_rand_X.csv", sep = ";")

x_plot_rand <- x_rand %>%
  mutate(Observation = as.factor(ifelse(row_number()<51, "Initial sample", "Added"))) %>% 
  ggplot(aes (x = age, y = balance, color = Observation)) + 
  geom_jitter() +
  scale_x_continuous(name = "Age") +
  scale_y_continuous(name = "Balance") + 
  ggtitle("Random")

x_det <- read.csv("bank_data_det_X.csv", sep = ";")

x_plot_det <- x_det %>% 
  mutate(Observation = as.factor(ifelse(row_number()<51, "Initial sample", "Added"))) %>% 
  ggplot(aes (x = age, y = balance, color = Observation)) +
  geom_jitter() +
  scale_x_continuous(name = "Age") +
  scale_y_continuous(name = "Balance")+ 
  ggtitle("D-optimality")


x_trace <- read.csv("bank_data_trace_X.csv", sep = ";")

x_plot_trace <- x_trace %>% 
  mutate(Observation = as.factor(ifelse(row_number()<51, "Initial sample", "Added"))) %>% 
  ggplot(aes (x = age, y = balance, color = Observation)) +
  geom_jitter() +
  scale_x_continuous(name = "Age") +
  scale_y_continuous(name = "Balance")+ 
  ggtitle("A-optimality")


grid.arrange(x_plot_det, x_plot_trace, x_plot_rand, nrow = 3)
  