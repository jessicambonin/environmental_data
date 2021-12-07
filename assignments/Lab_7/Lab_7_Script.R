# Create simulated data
dat = matrix(1:49, nrow = 7, ncol = 7)
print(dat)

apply(dat, MARGIN = 1, FUN = min)

apply(dat, MARGIN = 1, FUN = max)

apply(dat, MARGIN = 2, FUN = mean)

require(here)
moths = read.csv(here("data", "moths.csv"))
head(moths)


# A Simple Bootstrap Confidence Interval

m = 10000

# numeric() creates an vector of length m with all values initialized to zero
result = numeric(m)
head(result)

# Perform a Bootstrap
for(i in 1:m)
{
  result[i] = mean(sample(moths$anst, replace=TRUE))
}

mean(result)
quantile(result,c(0.025,0.975))

# Bootstrap Interval Using boot()
install.packages("boot")
require(boot)
boot(data, statistic, R)

# Custom Mean Function
boot_mean = function(x, i)
{
  return(mean(x[i], na.rm = TRUE))
}

myboot = 
  boot(
    data = moths$anst,
    statistic = boot_mean,
    R = 10000)
print(myboot)

str(myboot)

mean(moths$anst)
myboot$t0
mean(myboot$t) - myboot$t0
sd(myboot$t)

quantile(
  myboot$t,
  c(0.025, 0.975))

# Setting up the bootstrap
moth_dat = moths[,-1]
head(moth_dat)

n = nrow(moth_dat) #number of rows or sample observations
m = 100 #number of bootstrap iterations
moth_result = matrix(
  nrow = m,
  ncol = n)
data[sample(...), ]

n = nrow(moth_dat) #number of rows or sample observations

m = 100 #number of bootstrap iterations

moth_result = matrix(
  nrow = m,
  ncol = n)


# The outer loop: runs once for each bootstrap iteration.  index variable is i
for(i in 1:m)
{
  # The inner loop: simulates increasing sampling intensity
  # Sampling intensity ranges from 1 site to the complete count of sites (24)
  # index variable is j
  for(j in 1:n)
  {
    # sample the input data row indices, with replacement
    rows_j = sample(n, size = j, replace=TRUE)
    
    # Creates a new data matrix from the resampled rows.
    t1 = moth_dat[rows_j, ]
    
    # Calculates the column sums of the new data matrix.
    t2 = apply(t1, 2, sum)
    
    # Counts the number of columns in which any moths were observed
    moth_result[i, j] = sum(t2 > 0)
  }
}

head(moth_result)

# First Draft Function
rarefaction_sampler = function(input_dat, n_iterations)
{
  n = nrow(moth_dat) #number of rows or sample observations
  m = 100 #number of bootstrap iterations
  
  moth_result = matrix(
    nrow = m,
    ncol = n)
  
  # The outer loop: runs once for each bootstrap iteration.  index variable is i
  for(i in 1:m)
  {
    # The inner loop: simulates increasing sampling intensity
    # Sampling intensity ranges from 1 site to the complete count of sites (24)
    # index variable is j
    for(j in 1:n)
    {
      
      # sample the input data row indices, with replacement
      rows_j = sample(n, size = j, replace=TRUE)
      
      # Creates a new data matrix
      t1 = moth_dat[rows_j, ]
      
      # Calculates the column sums
      t2 = apply(t1, 2, sum)
      
      # Counts the number of columns in which any moths were observed
      moth_result[i, j] = sum(t2 > 0)
    }
  }
  
  return(moth_result)
}

rarefact = rarefaction_sampler(moth_dat, 100)
head(rarefact)

# Second Draft
rarefaction_sampler = function(input_dat, n_iterations)
{
  n_input_rows = nrow(input_dat)
  
  results_out = matrix(
    nrow = n_iterations,
    ncol = n_input_rows)
  
  # The outer loop: runs once for each bootstrap iteration.  index variable is i
  for(i in 1:n_iterations)
  {
    # The inner loop: simulates increasing sampling intensity
    # Sampling intensity ranges from 1 site to the complete count of
    # sites in the input data (n)
    # index variable is j
    for(j in 1:n)
    {
      # sample the input data row indices, with replacement
      rows_j = sample(n, size = j, replace=TRUE)
      
      # Creates a new data matrix
      t1 = input_dat[rows_j, ]
      
      # Calculates the column sums
      t2 = apply(t1, 2, sum)
      
      # Counts the number of columns in which any moths were observed
      results_out[i, j] = sum(t2 > 0)
    }
  }
  return(results_out)
}

rarefact = rarefaction_sampler(moth_dat, 100)
head(rarefact)

# Check in a fresh environment
# This clears the current R session's environment
rm(list = ls())

# Re-read my data:
moths = read.csv(here("data", "moths.csv"))
moth_dat = moths[,-1]

rarefaction_sampler = function(input_dat, n_iterations)
{
  n_input_rows = nrow(input_dat)
  
  results_out = matrix(
    nrow = n_iterations,
    ncol = n_input_rows)
  
  # The outer loop: runs once for each bootstrap iteration.  index variable is i
  for(i in 1:n_iterations)
  {
    # The inner loop: simulates increasing sampling intensity
    # Sampling intensity ranges from 1 site to the complete count of
    # sites in the input data (n)
    for(j in 1:n)
    {
      # sample the input data row indices, with replacement
      rows_j = sample(n, size = j, replace=TRUE)
      
      # Creates a new data matrix
      t1 = input_dat[rows_j, ]
      
      # Calculates the column sums
      t2 = apply(t1, 2, sum)
      
      # Counts the number of columns in which any moths were observed
      results_out[i, j] = sum(t2 > 0)
    }
  }
  return(results_out)
}

# rarefact = rarefaction_sampler(moth_dat, 100)
head(rarefact)

# Debugging template
# This clears the current R session's environment
rm(list = ls())

# Re-read my data:
moths = read.csv(here("data", "moths.csv"))


rarefaction_sampler = function(input_dat, n_iterations)
{
  n_input_rows = nrow(input_dat)
  moth_dat = moths[,-1]
  n = nrow(moth_dat)
  
  results_out = matrix(
    nrow = n_iterations,
    ncol = n_input_rows)
  
    
    for(i in 1:n_iterations)
    {
      for(j in 1:n)
      {
        rows_j = sample(n, size = j, replace=TRUE)
       
        t1 = input_dat[rows_j, ]
        
        t2 = apply(t1, 2, sum)
        
        results_out[i, j] = sum(t2>0)
      }
    }
    return(results_out)
  }
  
rarefact = rarefaction_sampler(moths[,-1], 100)
head(rarefact)


# Re-read my data:
moths = read.csv(here("data", "moths.csv"))
rarefact = rarefaction_sampler(moths[,-1], 10000)

rare_mean = apply(rarefact, 2, mean)
rare_quant = apply(rarefact, 2, quantile, probs=c(0.025, 0.975))
rare = t(rbind(rare_mean, rare_quant))

matplot(
  rare,
  type='l',
  xlab='Number of sampling plots',
  ylab='Species richness',
  main='Rarefaction Curve')

legend(
  'bottomright',
  legend=c('mean','2.5%','97.5%'),
  lty=c(1,2,3),col=c(1,2,3), inset=c(.1,.1))

# Question 1 - 5 Setup
require(palmerpenguins)
gentoo_penguins <- penguins[which(penguins$species=='Gentoo'), ]
gentoo_penguins_2 <- gentoo_penguins[complete.cases(gentoo_penguins$bill_length_mm), ]

sse_mean = function(x)
{
  sd(x, na.rm = TRUE)/(sqrt(length(x[!is.na(x)])))
}
sse_mean(gentoo_penguins_2$bill_length_mm)

# Question 1
# qt(p, df, ncp, lower.tail = TRUE, log.p = FALSE)
sd(gentoo_penguins_2$bill_length_mm)
t.test(gentoo_penguins_2$bill_length_mm)

quantile(gentoo_penguins_2$bill_length_mm, c(0.025,0.975))

# Question 3
qt(.975, 122, lower.tail = FALSE, log.p = FALSE)   
qt(.975, 122, lower.tail = TRUE, log.p = FALSE)

# Question 5
sse_mean(gentoo_penguins_2$bill_length_mm)*qt(.975, 122, lower.tail = TRUE, log.p = FALSE)
mean(gentoo_penguins_2$bill_length_mm)-
  (sse_mean(gentoo_penguins_2$bill_length_mm)*
     qt(.975, 122, lower.tail = TRUE, log.p = FALSE)) 
mean(gentoo_penguins_2$bill_length_mm)+
  (sse_mean(gentoo_penguins_2$bill_length_mm)*
     qt(.975, 122, lower.tail = TRUE, log.p = FALSE)) 

# Questions 6-8
require(boot)
# boot(data, statistic, R)
boot_mean = function(x, i)
{
  return(mean(x[i], na.rm = TRUE))
}
myboot = 
  boot(
    data = gentoo_penguins_2$bill_length_mm,
    statistic = boot_mean,
    R = 10000)
print(myboot)

str(myboot)

mean(gentoo_penguins_2$bill_length_mm)
myboot$t0
mean(myboot$t) - myboot$t0
sd(myboot$t)
quantile(myboot$t, c(0.025, 0.975))

# Questions 9 - 13
rm(list = ls())

require(here)
moths = read.csv(here("data", "moths.csv"))

rarefaction_sampler = function(input_dat, n_iterations)
{
  n_input_rows = nrow(input_dat)
  moth_dat = moths[,-1]
  n = nrow(moth_dat)
  
  results_out = matrix(
    nrow = n_iterations,
    ncol = n_input_rows)
  
  for(i in 1:n_iterations)
  {
    for(j in 1:n)
    {
      rows_j = sample(n, size = j, replace=TRUE)
      t1 = input_dat[rows_j, ]
      t2 = apply(t1, 2, sum)
      results_out[i, j] = sum(t2>0)
    }
  }
  return(results_out)
}

rarefact = rarefaction_sampler(moths[,-1], 10000)
head(rarefact)

# Question 11
rare_mean = apply(rarefact, 2, mean)
rare_quant = apply(rarefact, 2, quantile, probs=c(0.025, 0.975))
rare = t(rbind(rare_mean, rare_quant))

par(bg= "seashell")
matplot(
  rare,
  type='l',
  xlab='Number of Sampling Plots',
  ylab='Species Richness',
  main='Rarefaction Curve', 
  lty=c("solid", "dashed", "dotted"),
  col=c("black", "red4", "forestgreen"), 
  lwd = c(2,2,2))

legend(
  'bottomright',
  legend=c('Mean','2.5%','97.5%'), bg = "aliceblue", 
  lty=c("solid", "dashed", "dotted"),col=c("black", "red4", "forestgreen"), lwd = c(2,2,2), inset=c(.1,.1))

