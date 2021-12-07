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

