## Stat-S610 Homework 5
## testing how long the llr function takes to run and prints it out

source("llr_functions.R")
library(microbenchmark)

time = microbenchmark(llr, times = 1000)
print(time) 