require(here)
dat = read.csv(here("data", "dispersal.csv"))

# relationship between juvenile dispersal (disp.rate.ftb) and distance (dist.class
plot(dat$dist.class, dat$disp.rate.ftb)

# Linear
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
guess_x = 700
guess_y = .2
guess_slope = -.0002

plot(dat$dist.class, dat$disp.rate.ftb, main = "Linear Model", ylab = "Juvenile Dispersal", xlab = "Distance")
curve(line_point_slope(x, guess_x, guess_y, guess_slope), add = T)

# Exponential 
exp_fun = function(x, a, b)
{
  return(a * exp(-b*x))
}
plot(dat$dist.class, dat$disp.rate.ftb, main = "Exponential Model", ylab = "Juvenile Dispersal", xlab = "Distance")
curve(exp_fun(x, 1, .002), add = TRUE, from = 0, to = 1500,
  ann = FALSE, ylab = "f(x)"); box()

# Ricker
ricker_fun = function(x, a, b) 
{
  return(a * x * exp(-b * x))
}
plot(dat$dist.class, dat$disp.rate.ftb, main = "Ricker Model", ylab = "Juvenile Dispersal", xlab = "Distance")
curve(
  ricker_fun(x, .0055, (1/350)), from = 0, to = 1500, add = TRUE)

# Question 2
curve(exp_fun(x, 1.9, 0.1), from = 0, to = 40,  col = "black", lty = "solid", main = "Question 2", ylab = "")
curve(exp_fun(x, 1.9, 0.3), from = 0, to = 40, add = TRUE, col = "black", lty = "dotted")
curve(exp_fun(x, 1.2, 0.2), from = 0, to = 40, add = TRUE, col = "red", lty = "solid")
curve(exp_fun(x, 1.2, 0.4), from = 0, to = 40, add = TRUE, col = "red", lty = "dotted")

# Question 5
curve(ricker_fun(x, 25, 0.1), from = 0, to = 30, add = FALSE, col = "black", lty = "solid", main = "Question 5", ylab = "")
curve(ricker_fun(x, 20, 0.2), from = 0, to = 30, add = TRUE, col = "black", lty = "dotted")
curve(ricker_fun(x, 10, 0.2), from = 0, to = 30, add = TRUE, col = "black", lty = "dotted")
curve(ricker_fun(x, 75, 0.3), from = 0, to = 30,add = TRUE, col = "red", lty = "solid")
curve(ricker_fun(x, 50, 0.3), from = 0, to = 30, add = TRUE, col = "red", lty = "dotted")
curve(ricker_fun(x, 40, 0.3), from = 0, to = 30, add = TRUE, col = "red", lty = "dotted")

# Question 6
curve(ricker_fun(x, 25, 0.2), from = 0, to = 25, add = FALSE, col = "black", lty = "solid", main = "Change in a", ylab = "")
curve(ricker_fun(x, 10, 0.2), from = 0, to = 25, add = TRUE, col = "red", lty = "solid")

# Question 7
curve(ricker_fun(x, 25, 0.15), from = 0, to = 25, add = FALSE, col = "black", lty = "solid", main = "Change in b", ylab = "")
curve(ricker_fun(x, 25, 0.3), from = 0, to = 25, add = TRUE, col = "red", lty = "solid")

# Question 14
# subtract predicted - observed

pred_linear = line_point_slope(dat$dist.class, 700, .2, -0.0002)
resids_linear = (pred_linear - dat$disp.rate.ftb)

pred_exp = exp_fun(dat$dist.class, 1, .002)
resids_exp = (pred_exp - dat$disp.rate.ftb)

pred_ricker = ricker_fun(dat$dist.class, .0055, (1/350))
resids_ricker = (pred_ricker - dat$disp.rate.ftb)

dat$resids_linear <- resids_linear
dat$resids_exp <- resids_exp
dat$resids_ricker <- resids_ricker

# Question 15
image_file = "residual_models.png"
png(
  here("images", image_file), width = 1000, height = 1600, res = 300)

par(mfrow = c(3,1))
hist(dat$resids_linear, main = "Linear Model Residuals", xlab = "Residuals")
hist(dat$resids_exp, main = "Exponential Model Residuals", xlab = "Residuals")
hist(dat$resids_ricker, main = "Ricker Model Residuals", xlab = "Residuals") 

dev.off()

