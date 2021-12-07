lab.mean = 10.4
lab.sd = 2.4

norm_17 = rnorm(n = 17, mean = lab.mean, sd = lab.sd)
norm_30 = rnorm(n = 30, mean = lab.mean, sd = lab.sd)
norm_300 = rnorm(n = 300, mean = lab.mean, sd = lab.sd)
norm_3000 = rnorm(n = 3000, mean = lab.mean, sd = lab.sd)

par(mfrow = c(2, 2))
hist(norm_17, xlab = "Random Data", ylab = "Frequency", main = "17 Data Points")
hist(norm_30, xlab = "Random Data", ylab = "Frequency", main = "30 Data Points")
hist(norm_300, xlab = "Random Data", ylab = "Frequency", main = "300 Data Points")
hist(norm_3000, xlab = "Random Data", ylab = "Frequency", main = "3000 Data Points")

# Saving PNG
require(here)
image_file = "lab_04_hist_01.png"
png(
  here("images", image_file),
  width = 1500, height = 1600,
  res = 180)

par(mfrow = c(2, 2))
hist(norm_17, xlab = "Random Data", ylab = "Frequency", main = "17 Data Points")
hist(norm_30, xlab = "Random Data", ylab = "Frequency", main = "30 Data Points")
hist(norm_300, xlab = "Random Data", ylab = "Frequency", main = "300 Data Points")
hist(norm_3000, xlab = "Random Data", ylab = "Frequency", main = "3000 Data Points")

dev.off()

# Question 7-8
image_file = "norm_1.png"
png(here("images", image_file))

x = seq(0, 20, length.out = 1000)
y = dnorm(x, mean = 10.4, sd = 2.4, log = FALSE)

plot(x, y, main = "Normal PDF: mean = 10.4, sd = 2.4", type = "l", xlim = c(0, 20))
abline(h = 0)

dev.off()


# Picking my parameters
pnorm(20, mean = 10.4, sd = 2.4)
min(norm_3000)
max(norm_3000)

# Questions 9-10
image_file = "random_data_sets.png"
png(
  here("images", image_file),
  width = 1500, height = 1600,
  res = 180)

par(mfrow = c(2, 2))

set.seed(97)
n_pts = 25
x_min = 1
x_max = 200
x = runif(n = n_pts, min = x_min, max = x_max)
dat = data.frame(x = x, y_observed = runif(n_pts))
plot(dat$x, dat$y_observed, main = "Scatterplot: 25 Points (1-200)", xlab = "x-value", ylab = "Observed y-value", pch = 16)

set.seed(97)
n_pts = 100
x_min = 1
x_max = 80
x = runif(n = n_pts, min = x_min, max = x_max)
dat = data.frame(x = x, y_observed = runif(n_pts))
plot(dat$x, dat$y_observed, main = "Scatterplot: 100 Points (1-80)", xlab = "x-value", ylab = "Observed y-value", pch = 16, col="darkgreen", cex=0.7)

set.seed(97)
n_pts = 100
x_min = 1
x_max = 30
x = runif(n = n_pts, min = x_min, max = x_max)
dat = data.frame(x = x, y_observed = runif(n_pts))
boxplot(dat$y_observed, main = "Boxplot: 100 Points (1-30)", ylab = "Observed y-value", col = "darkorchid4")

set.seed(97)
n_pts = 1000
x_min = 1
x_max = 50
x = runif(n = n_pts, min = x_min, max = x_max)
dat = data.frame(x = x, y_observed = runif(n_pts))
hist(dat$y_observed, main = "Histogram: 1000 Points (1-50)", xlab = "Observed y-value", ylab = "Frequency", col = "pink")

dev.off()


# Questions 11-12
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
require(here)
image_file = "measuring_error_linear_plot.png"
png(
  here("images", image_file),
  width = 1500, height = 1500,
  res = 180)
set.seed(97)
n_pts = 25
x_min = 1
x_max = 200
x = runif(n = n_pts, min = x_min, max = x_max)
dat = data.frame(x = x, y_observed = runif(n_pts))

guess_x = 50
guess_y = 0.3
guess_slope = .003

plot(y_observed ~ x, data = dat, pch = 8, main = "Scatterplot: Measuring Error", xlab = "x-value", ylab = "Observed y-value")
curve(line_point_slope(x, guess_x, guess_y, guess_slope), add = T)



# Questions 13-14
line_point_slope(dat$x, guess_x, guess_y, guess_slope)
dat = data.frame(x = x, y_observed = runif(n_pts), y_predicted = line_point_slope(dat$x, guess_x, guess_y, guess_slope))
dat = data.frame(x = x, y_observed = rnorm(n_pts), 
                 y_predicted = line_point_slope(dat$x, guess_x, guess_y, guess_slope), 
                 resids = dat$y_observed-dat$y_predicted)

sum(dat$resids)
sum(abs(dat$resids))

image_file = "residual_plots.png"
png(
  here("images", image_file),
  width = 1500, height = 1500,
  res = 180)
par(mfrow = c(2, 2))
plot(x = dat$y_predicted, y = dat$resids,
     xlab = "Predicted Values", ylab = "Model Residuals", main = "Residual Scatterplot: Linear Model", col = "blue", cex = 1.5, pch = 16)
hist(dat$resids,
     xlab = "Residual Values", ylab = "Frequency", main = "Histogram of Model Residuals: Linear Model", col = "red")

dev.off()
