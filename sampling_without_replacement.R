calculate_P <- function(n, k) {
  numerator <- (factorial(n) / factorial(n - (k - 1))) * (k - 1)
  denominator <- n^k
  P_m <- numerator / denominator
  return(P_m)
}
n <- 8
k <- 9
# Calculate P using the function
result <- calculate_P(n, k)
print(result)


# Function to calculate P_m
calculate_P_m <- function(n, m, k) {
  numerator <- (factorial(n - m) / factorial((n - m) - ((k - m) - 1))) * ((k - m) - 1)
  denominator <- (n - m)^(k - m)
  P_m <- numerator / denominator
  return(P_m)
}

# Provide values for n, m, and k
n <- 8 # number of arms
m <- 1 # memory size
k <- 2 # first error, 9 means no error

# Calculate P_m using the function
result <- calculate_P_m(n, m, k)
print(round(result,2))
