library(tidyverse)

#Investing bank data set

setwd("C:/Users/tobia/Dropbox/Master Thesis/Sims/Bank_data_edit")
bank_data_read <- read.csv2("bank-full.csv")

#Removing unknown observations
bank_data_read3 <- bank_data_read %>%
  select(age, balance, previous, job, marital, education, default, housing, loan, poutcome, y)%>%
  filter_all(., all_vars(.!= "unknown")) %>% droplevels() %>%
  filter_all(., all_vars(.!= "other")) %>% droplevels() %>%
  rowid_to_column(., "ID")


#plotting data
par(mfrow = c(3,3))
barplot(table(bank_data_read3[, 5]), xlab = "", main = "Job",las=2)
barplot(table(bank_data_read3[, 6]), xlab = "", main = "Marital",las=2)
barplot(table(bank_data_read3[, 7]), xlab = "", main = "Education",las=2)
barplot(table(bank_data_read3[, 8]), xlab = "", main = "Default",las=2)
barplot(table(bank_data_read3[, 9]), xlab = "", main = "Housing",las=2)
barplot(table(bank_data_read3[, 10]), xlab = "", main = "Loan",las=2)
barplot(table(bank_data_read3[, 11]), xlab = "", main = "poutcome",las=2)
barplot(table(bank_data_read3[, 12]), xlab = "", main = "y",las=2)


#plotting data
par(mfrow =c(1,3))
hist(bank_data_read3[ ,2], 
     main = expression(Age), xlab = expression(), breaks = 30)
hist(bank_data_read3[ ,3],
     main = expression(Balance), xlab = expression(), breaks = 30)
hist(bank_data_read3[ ,4],
     main = expression(Previous), xlab = expression(), breaks = 30)

#Reformatting previous
bank_data_read32 <- bank_data_read3 %>% 
  mutate(previous = ifelse(previous == 1, 1, 2))  

#plotting data
par(mfrow = c(3,3))
barplot(table(bank_data_read32[, 4]), xlab = "", main = "Previous",las=2)
barplot(table(bank_data_read32[, 5]), xlab = "", main = "Job",las=2)
barplot(table(bank_data_read32[, 6]), xlab = "", main = "Marital",las=2)
barplot(table(bank_data_read32[, 7]), xlab = "", main = "Education",las=2)
barplot(table(bank_data_read32[, 8]), xlab = "", main = "Default",las=2)
barplot(table(bank_data_read32[, 9]), xlab = "", main = "Housing",las=2)
barplot(table(bank_data_read32[, 10]), xlab = "", main = "Loan",las=2)
barplot(table(bank_data_read32[, 11]), xlab = "", main = "poutcome",las=2)
barplot(table(bank_data_read32[, 12]), xlab = "", main = "y",las=2)

#plotting data
par(mfrow =c(1,2))
hist(bank_data_read3[ ,2], 
     main = expression(Age), xlab = expression(), breaks = 30)
hist(bank_data_read3[ ,3],
     main = expression(Balance), xlab = expression(), breaks = 30)

#Reformatting job and removing default
bank_data_read33 <- bank_data_read32 %>% 
  mutate(job = as.factor(ifelse(job == "admin" | job == "blue-collar" | job == "entrepeneur" |
                                  job == "housemaid" | job == "management" | job == "self-employed" |
                                  job == "services" | job == "technician", "employed", 
                                "unemployed"))) %>% 
  select(-default)

#plotting data
barplot(table(bank_data_theory2[, 5]), xlab = "", main = "Job")                     

#writing file
write.table(bank_data_theory2, file = "bank_data_alpha", sep=";")

#Plotting used variables
setwd("C:/Users/tobia/Dropbox/Master Thesis/Sims/Code")
bank_data<- read.csv2("bank.csv")

bank_data <- bank_data %>% 
  mutate(previous = ifelse(previous==2, "2:more than one", "1:one or less"))


par(mfrow = c(3,3))
hist(bank_data[ ,2], main = "Age", xlab = expression(), breaks = 30, las = 1)
hist(bank_data[ ,3], main = "Balance", xlab = expression(), breaks = 30, las = 1)
barplot(table(bank_data[, 4]), xlab = "", main = "Previous", las=1, cex.names=1.5)
barplot(table(bank_data[, 5]), xlab = "", main = "Job",las=1, cex.names=1.5)
barplot(table(bank_data[, 6]), xlab = "", main = "Marital",las=1, cex.names=1.5)
barplot(table(bank_data[, 7]), xlab = "", main = "Education",las=1, cex.names=1.5)
barplot(table(bank_data[, 8]), xlab = "", main = "Housing",las=1, cex.names=1.5)
barplot(table(bank_data[, 9]), xlab = "", main = "Loan",las=1, cex.names=1.5)
barplot(table(bank_data[, 10]), xlab = "", main = "Poutcome",las=1, cex.names=1.5)

barplot(table(bank_data[, 11]), xlab = "", main = "Interested",las=2)

summary(bank_data)


#### #Randomizing bank data - Used file produced above which was edited #######

bank_data <- read.csv("bank.csv", sep = ";")

#Rearrange data randmoly (which is split in algorithms)
bank_data_rearrange <- bank_data %>% 
  mutate(rand = runif(6133, 0, 999999)) %>%
  arrange(rand) %>% 
  select(-rand, ID) %>% 
  mutate(ID = 1:nrow(.))

write.table(bank_data_rearrange, file = "bank_data_rearranged.csv", sep = ";")


