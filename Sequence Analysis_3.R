
install.packages("readxl")

# Loading the package
library(readxl)

# Importing excel file
Sequence_of_arms <- read_excel("C:/Users/reicherta/Desktop/Data2.xlsx", range = cell_cols("M:T"),col_names = TRUE)

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

