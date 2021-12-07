# dnorm(): the probability density
# pnorm(): the cumulative probability density
# qnorm(): the quantile function
# rnorm(): function to generate random, normally-distributed numbers.

# Practice Exercise: name the distribution function
dnorm(-1.96, mean = 0, sd = 1, log = FALSE)
dnorm(-1, mean = 0, sd = 1, log = FALSE)
dnorm(0, mean = 0, sd = 1, log = FALSE)
dnorm(1.96, mean = 0, sd = 1, log = FALSE)

# Generate a vector of x-values
x = seq(-3, 3, length.out = 1000)
y = dnorm(x)

plot(x, y, main = "Normal PDF", type = "l")
abline(h = 0)

# Random deviates functions
require(palmerpenguins)
hist(
  penguins$body_mass_g,
  main = "Histogram of Penguin Body Mass",
  xlab = "Body Mass (g)")

mean(penguins$body_mass_g, na.rm = TRUE)
sd(penguins$body_mass_g, na.rm = TRUE)
nrow(penguins)

# Random Penguin Masses
dat_1 = rnorm(n = 344, mean = 4202, sd = 802)
dat_2 = rnorm(n = 344, mean = 4202, sd = 802)
dat_3 = rnorm(n = 344, mean = 4202, sd = 802)
dat_4 = rnorm(n = 344, mean = 4202, sd = 802)

par(mfrow = c(2, 2))

hist(dat_1)
hist(dat_2)
hist(dat_3)
hist(dat_4)

# Random Uniform Numbers
# The runif() function will generate random, uniformly-distributed numbers:

set.seed(12)
dat_unif = runif(n = 27, min = 0, max = 4)
hist(dat_unif)

dat_unif = runif(n = 270, min = 0, max = 4)
hist(dat_unif)

# Randomness and Replication: set.seed()
set.seed(1)
dat_unif_1 = runif(n = 270, min = 0, max = 4)
set.seed(1)
dat_unif_2 = runif(n = 270, min = 0, max = 4)

par(mfrow = c(1, 2))
hist(dat_unif_1)
hist(dat_unif_2)

# Calculates the value of y for a linear function, given the coordinates
# of a known point (x1, y1) and the slope of the line.
line_point_slope = function(x, x1, y1, slope)
{
  get_y_intercept = 
    function(x1, y1, slope) 
      return(-(x1 * slope) + y1)
  
  linear = 
    function(x, yint, slope) 
      return(yint + x * slope)
  
  return(linear(x, get_y_intercept(x1, y1, slope), slope))
}

# Measuring Error: claculating residuals
set.seed(123)
n = 17
slope = 0.7
intcp = 0.2

guess_x = 6
guess_y = 4
guess_slope = 0.72

x = runif(n = n, min = 1, max = 10)
y = rnorm(n = n, mean = slope * x + intcp)

plot(x, y, pch = 16)
curve(line_point_slope(x, guess_x, guess_y, guess_slope), add = T)

# Create the data
n_pts = 10
x_min = 1
x_max = 10
x = runif(n = n_pts, min = x_min, max = x_max)

dat = data.frame(x = x, y_observed = rnorm(n_pts))

plot(y_observed ~ x, data = dat, pch = 8)

# Setting the seed creates the same generated numbers every time the line is run
set.seed(123)
n_pts = 10
x_min = 1
x_max = 10
x = runif(n = n_pts, min = x_min, max = x_max)

dat = data.frame(x = x, y_observed = rnorm(n_pts))

plot(y_observed ~ x, data = dat, pch = 8)

# Fit a linear deterministic model- His model
set.seed(200)
n_pts = 10
x_min = 1
x_max = 10
x = runif(n = n_pts, min = x_min, max = x_max)

dat = data.frame(x = x, y_observed = rnorm(n_pts))

plot(y_observed ~ x, data = dat, pch = 8)

guess_x = 6
guess_y = 0
guess_slope = 0.1

plot(y_observed ~ x, data = dat, pch = 8)
curve(line_point_slope(x, guess_x, guess_y, guess_slope), add = T)

# Fit a linear deterministic model- My model
set.seed(200)
n_pts = 10
x_min = 1
x_max = 10
x = runif(n = n_pts, min = x_min, max = x_max)

dat = data.frame(x = x, y_observed = rnorm(n_pts))

plot(y_observed ~ x, data = dat, pch = 8)

guess_x = 6.5
guess_y = 0
guess_slope = 0.5

plot(y_observed ~ x, data = dat, pch = 8)
curve(line_point_slope(x, guess_x, guess_y, guess_slope), add = T)

# Add predicted values
set.seed(200)
n_pts = 10
x_min = 1
x_max = 10
x = runif(n = n_pts, min = x_min, max = x_max)

dat = data.frame(x = x, y_observed = rnorm(n_pts))

plot(y_observed ~ x, data = dat, pch = 8)

guess_x = 6.5
guess_y = 0
guess_slope = 0.5

plot(y_observed ~ x, data = dat, pch = 8)
curve(line_point_slope(x, guess_x, guess_y, guess_slope), add = T)
line_point_slope(dat$x, guess_x, guess_y, guess_slope)
dat = data.frame(x = x, y_observed = rnorm(n_pts), y_predicted = line_point_slope(dat$x, guess_x, guess_y, guess_slope))

# Calculate the residuals
dat = data.frame(x = x, y_observed = rnorm(n_pts), 
                 y_predicted = line_point_slope(dat$x, guess_x, guess_y, guess_slope), 
                 resids = dat$y_observed-dat$y_predicted)
sum(dat$resids)
sum(abs(dat$resids))

# Plot the residuals
plot(x = dat$y_observed, y = dat$resids,
     xlab = "Observed Values", ylab = "Model Residuals", main = "Residual Scatterplot: Linear Model")
hist(dat$resids,
  xlab = "Residual Values", ylab = "Frequency", main = "Histogram of Model Residuals: Linear Model")
