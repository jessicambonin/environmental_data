# sample standard error of the mean
sse_mean = function(x)
{
  sd(x, na.rm = TRUE)/(sqrt(length(x[!is.na(x)])))
}

require(palmerpenguins)
sse_mean(penguins$bill_depth_mm)

# Using dropleves() to omit from data
# Remeber that an ! omits 
boxplot(flipper_length_mm ~ species, data = penguins)
dat_pen = subset(penguins, species != "Gentoo")
boxplot(flipper_length_mm ~ species, data = dat_pen)
dat_pen = droplevels(subset(penguins, species != "Gentoo"))
{
  par(mfrow = c(1, 2))
  boxplot(flipper_length_mm ~ species, data = penguins)
  boxplot(flipper_length_mm ~ species, data = dat_pen)
}

# Resampling with replacement
set.seed(123)

flipper_shuffled = sample(penguins$flipper_length_mm, replace = TRUE)
par(mfrow = c(1, 2))
boxplot(flipper_length_mm ~ species, data = penguins)
boxplot(flipper_shuffled ~ penguins$species, xlab = "species")


# t test
t.test(dat_pen$flipper_length_mm ~ dat_pen$species)

# Two sample resampling
set.seed(1)
flipper_shuffled = sample(dat_pen$flipper_length_mm)
boxplot(flipper_shuffled ~ dat_pen$species)
t_test_1 = t.test(flipper_shuffled ~ dat_pen$species)
t_test_1

# Difference of Means
t_test = t.test(dat_pen$flipper_length_mm ~ dat_pen$species)
t_test

t_test$estimate

diff_observed = round(diff(t_test$estimate), digits = 3)
print(diff_observed, digits = 3)

# Using aggregate()
agg_means = aggregate(
  flipper_length_mm ~ species, 
  data = dat_pen, 
  FUN = mean, 
  na.rm = TRUE)
diff_observed = diff(agg_means[, 2])

agg_means
diff_observed

# Sample sizes
table(dat_pen$species)

n_1 = 68
n_2 = 152

dat_1 = sample(dat_pen$flipper_length_mm, n_1, replace = TRUE)
dat_2 = sample(dat_pen$flipper_length_mm, n_2, replace = TRUE)

diff_simulated = 
  mean(dat_1, na.rm = TRUE) - mean(dat_2, na.rm = TRUE)

print(c(observed = diff_observed, simulated = diff_simulated))

# Simulation function
x = dat_pen$flipper_length_mm
n_1 = 68
n_2 = 152

dat_1 = sample(x, n_1, replace = TRUE)
dat_2 = sample(x, n_2, replace = TRUE)

diff_simulated = 
  mean(dat_1, na.rm = TRUE) - mean(dat_2, na.rm = TRUE)

# BYOF

two_group_resample = function(x, n_1, n_2)
{
  difference_in_means = mean(sample(x, n_1, replace = TRUE), na.rm = TRUE) - mean(sample(x, n_2, replace = TRUE), na.rm = TRUE)
  return(difference_in_means)
}

# Check BYOF
set.seed(54321)
two_group_resample(dat_pen$flipper_length_mm, 68, 152)

# Resampling experiment
n = 200
mean_differences = c()
for (i in 1:n)
{
  mean_differences = c(
    mean_differences,
    two_group_resample(dat_pen$flipper_length_mm, 68, 152)
  )
}
hist(mean_differences)

sum(abs(mean_differences) >= diff_observed)

# Retrieving named elements
t_test = t.test(flipper_shuffled ~ dat_pen$species)
str(t_test)
t_test$estimate

# Question 1
rm(list = ls())

sse_mean = function(x)
{
  sd(x, na.rm = TRUE)/(sqrt(length(x[!is.na(x)])))
}

sse_mean(penguins$body_mass_g)
sse_mean(mtcars$mpg)

# Question 2
two_group_resample = function(x, n_1, n_2)
{
  difference_in_means = mean(sample(x, n_1, replace = TRUE), na.rm = TRUE) - mean(sample(x, n_2, replace = TRUE), na.rm = TRUE)
  return(difference_in_means)
}

# Question 4
dat_pen = droplevels(subset(penguins, species != "Gentoo"))
x = dat_pen$flipper_length_mm
n_1 = 68
n_2 = 152
n = 2000
mean_differences = c()
for (i in 1:n)
{
  mean_differences = c(
    mean_differences,
    two_group_resample(dat_pen$flipper_length_mm, 68, 152)
  )
}
hist(mean_differences, main = "Resampled Differences of Means", xlab = "Mean Differences")
sum(abs(mean_differences) >= diff_observed)
print(mean_differences)

#Question 5
sum(abs(mean_differences) >= diff_observed)

# Question 7
boxplot(body_mass_g ~ species, data = dat_pen, main = "Body Mass for Adelie and Chinstrap Penguins", xlab = "Species", ylab = "Body Mass (g)")

agg_means = aggregate(
  body_mass_g ~ species, 
  data = dat_pen, 
  FUN = mean, 
  na.rm = TRUE)
diff_crit = diff(agg_means[, 2])

agg_means
diff_crit

t_test = t.test(dat_pen$body_mass_g ~ dat_pen$species)
t_test

x = dat_pen$body_mass_g
n_1 = 68
n_2 = 152
n = 1000
mean_differences = c()
for (i in 1:n)
{
  mean_differences = c(
    mean_differences,
    two_group_resample(dat_pen$body_mass_g, 68, 152)
  )
}
hist(mean_differences, main = "Resampled Differences of Means", xlab = "Mean Differences")
sum(abs(mean_differences) >= diff_crit)


