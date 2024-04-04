
#install.packages("readxl")

# Loading the package
library(readxl)

# Importing excel file
#Sequence_of_arms <- read_excel("C:/Users/reicherta/Desktop/Data2.xlsx", range = cell_cols("M:T"),col_names = TRUE)
Sequence_of_arms <- read_excel("Data2.xlsx", col_names = TRUE)

levels(as.factor(Sequence_of_arms$first_arm))
levels(as.factor(Sequence_of_arms$subj))

library(tidyverse)
Sequence_of_arms2<-Sequence_of_arms %>%
  mutate(sec_pos = ifelse(first_arm==sec_arm, 0, 1),
      third_pos = ifelse(first_arm==third_arm | sec_arm==third_arm, 0,  1),
      fourth_pos = ifelse(first_arm==fourth_arm | sec_arm==fourth_arm | third_arm==fourth_arm, 0,  1),
      fifth_pos = ifelse(first_arm==fifth_arm | sec_arm==fifth_arm | third_arm==fifth_arm | fourth_arm==fifth_arm, 0,  1),
      sixth_pos = ifelse(first_arm==sixth_arm | sec_arm==sixth_arm | third_arm==sixth_arm | fourth_arm==sixth_arm | fifth_arm==sixth_arm, 0,  1),
      seventh_pos = ifelse(first_arm==sevth_arm | sec_arm==sevth_arm | third_arm==sevth_arm | fourth_arm==sevth_arm | fifth_arm==sevth_arm | sixth_arm==sevth_arm, 0,  1),
      eight_pos = ifelse(first_arm==eigth_arm | sec_arm==eigth_arm | third_arm==eigth_arm | fourth_arm==eigth_arm | fifth_arm==eigth_arm | sixth_arm==eigth_arm | sevth_arm==eigth_arm, 0,  1),
  ) 

Sequence_of_arms_long <- Sequence_of_arms2 %>%
  select(CDL_name, block, trial_num, condition, first_arm:eigth_arm) %>%
  pivot_longer(cols = first_arm:eigth_arm, values_to = "chosen_arm", names_to = "arm_order") %>%
  group_by(CDL_name, condition, block, trial_num) %>%
  mutate(duplicate=duplicated(chosen_arm),
         correct = ifelse(duplicated(chosen_arm) == TRUE, 0, 1))%>%
  mutate(lag_from_last_choice = ifelse(chosen_arm==lag(chosen_arm), 1,
                                      ifelse(chosen_arm==lag(chosen_arm,2), 2,
                                             ifelse(chosen_arm==lag(chosen_arm,3), 3,
                                                    ifelse(chosen_arm==lag(chosen_arm,4), 4,
                                                           ifelse(chosen_arm==lag(chosen_arm, 5), 5,
                                                                  ifelse(chosen_arm==lag(chosen_arm, 6), 6,
                                                                         ifelse(chosen_arm==lag(chosen_arm, 7), 7,
                                                                                NA)))))))) #this variable descibes how many choices ago they entered this arm the last time

Sequence_of_arms_long$arm_order_num <- rep(1:8, nrow(Sequence_of_arms_long)/8)

table(Sequence_of_arms_long$arm_order, Sequence_of_arms_long$arm_order_num) 


first_error <- Sequence_of_arms_long %>%
  filter(duplicate ==TRUE)%>%
  group_by(CDL_name, condition, block, trial_num) %>%
  summarise(first_error=min(arm_order_num))
  
Sequence_of_arms_long2 <- Sequence_of_arms_long %>%
  full_join(first_error)



#Initialization of variables
error_position <- matrix(nrow=nrow(Sequence_of_arms), ncol=8)

l <- 0

#print(Sequence_of_arms)
#Comparing number to numbers of previous positions 

for (k in 1:nrow(Sequence_of_arms)) {
  for (n in 2:8) {
    for (m in 1:8) {
      
       l = n - m
       
        value <- as.numeric(Sequence_of_arms[k,n])
        value2 <- as.numeric(Sequence_of_arms[k,l])
        
        if (identical(value, value2)) {
          error_position [k,n] = n 
      } #else {
          #error_position [k,n] = "-"
        #}
      
      }
    }
  }


print(error_position)

