require(palmerpenguins)
penguin_dat = droplevels(subset(penguins, species != "Gentoo"))
adelie_dat = droplevels(subset(penguin_dat, species == "Adelie"))
chinstrap_dat = droplevels(subset(penguin_dat, species == "Chinstrap"))

t.test(flipper_length_mm ~ species, data = penguin_dat, alternative = "less")

install.packages("simpleboot")
require(simpleboot)
??two.boot()
# two.boot(sample1, sample2, FUN, R, student = FALSE, M, weights = NULL, ...)
two.boot(adelie_dat$flipper_length_mm, chinstrap_dat$flipper_length_mm, mean, 1000)
two.boot(adelie_dat$flipper_length_mm, chinstrap_dat$flipper_length_mm, mean, 10000)
hist(two.boot(adelie_dat$flipper_length_mm, chinstrap_dat$flipper_length_mm, mean, 10000))

require(here)
veg = read.csv(here("data", "vegdata.csv"))
boxplot(pine ~ treatment, dat = veg)
dat_tree = droplevels(subset(veg, treatment %in% c("control", "clipped")))
boxplot(pine ~ treatment, dat = dat_tree)

table(dat_tree$treatment)

# Wilcox.test
aggregate(pine ~ treatment, data = dat_tree, FUN = mean, na.rm = TRUE)
wilcox.test(17.875, 1.875, alternative = "two.sided")

# Bootstrap
require(boot)
tree_boot = 
  two.boot(
    subset(dat_tree, treatment == "clipped")$pine,
    subset(dat_tree, treatment == "control")$pine,
    FUN = mean,
    R = 10000,
    na.rm = TRUE
  )

# sum(tree_boot$t >= 0)
# sum(tree_boot$t < 0)

boot.ci(tree_boot)
hist(tree_boot$t, main = "Bootstrap sampling distribution")

quantile(tree_boot$t, 0.025)

dat_bird = read.csv(here("data", "bird.sub.csv"))
dat_habitat = read.csv(here("data", "hab.sub.csv"))

dat_all = merge(
  dat_bird, 
  dat_habitat,
  by = c("basin", "sub"))

head(dat_all[, c("b.sidi", "s.sidi")])

# Calculate the sample mean and sd:
b_sidi_mean = mean(dat_all$b.sidi, na.rm = TRUE)
b_sidi_sd   = sd(dat_all$b.sidi, na.rm = TRUE)

# Use the subset-by-name symbol ($) to create a 
# new column of z-standardized values.

dat_all$b.sidi.standardized = (dat_all$b.sidi - b_sidi_mean)/b_sidi_sd

mean(dat_all$b.sidi.standardized)
sd(dat_all$b.sidi.standardized)

plot(
  b.sidi ~ s.sidi, data = dat_all,
  main = "Simpson's diversity indices",
  xlab = "Vegetation cover diversity",
  ylab = "Bird diversity")

fit_1 = lm(b.sidi ~ s.sidi, data = dat_all)
coef(fit_1)
slope_observed = coef(fit_1)[2]

plot(
  b.sidi ~ s.sidi, data = dat_all,
  main = "Simpson's diversity indices",
  xlab = "Vegetation cover diversity",
  ylab = "Bird diversity")
abline(fit_1)

# The Slope Coefficient
dat_1 = 
  subset(
    dat_all,
    select = c(b.sidi, s.sidi))

# Resampling The Data
index_1 = sample(nrow(dat_1), replace = TRUE)
index_2 = sample(nrow(dat_1), replace = TRUE)

dat_resampled_i = 
  data.frame(
    b.sidi = dat_1$b.sidi[index_1],
    s.sidi = dat_1$s.sidi[index_2]
  )

fit_resampled_i = lm(b.sidi ~ s.sidi, data = dat_resampled_i)
slope_resampled_i = coef(fit_resampled_i)[2]

print(slope_resampled_i)

plot(
  b.sidi ~ s.sidi, data = dat_resampled_i,
  main = "Simpson's diversity indices",
  xlab = "Vegetation cover diversity",
  ylab = "Bird diversity")
abline(fit_resampled_i)

# Randomization Loop
m = 10000 
result = numeric(m) 

for(i in 1:m)
{
  index_1 = sample(nrow(dat_1), replace = TRUE)
  index_2 = sample(nrow(dat_1), replace = TRUE)
  
  dat_resampled_i = 
    data.frame(
      b.sidi = dat_1$b.sidi[index_1],
      s.sidi = dat_1$s.sidi[index_2]
    )
  
  fit_resampled_i = lm(b.sidi ~ s.sidi, data = dat_resampled_i)
  
  result[i] = coef(fit_resampled_i)[2]
} 

hist(result, main = "Null Distribution of Regression Slope", xlab = "Slope Parameter")
abline(v = slope_observed, lty = "solid", col = "blue", lwd = 2)
critical_value = quantile(result, c(.05))
abline(v = critical_value, lty = 2, col = "red", lwd = 2)

critical_value = quantile(result, c(.05))

# Questions 1-4
pen_boot = two.boot(adelie_dat$flipper_length_mm, chinstrap_dat$flipper_length_mm, mean, 10000, na.rm = TRUE)
hist(pen_boot$t, main = "Mean Flipper Length of Chinstrap and Adelie Penguins.", xlab = "Bootstrapped Differences")
quantile(pen_boot$t, c(.025, 0.975))

sd(pen_boot$t)

mean(pen_boot$t)
median(pen_boot$t)

# Question 5-7
pen_ecdf = ecdf(pen_boot$t)
1 - pen_ecdf(-4.5)
pen_ecdf(-8)

# Question 9
wilcox.test(pine ~ treatment, data = dat_tree, alternative = "two.sided", exact = FALSE)

# Questions 10 - 11
dat_clipped = droplevels(subset(dat_tree, treatment == "clipped"))
dat_control = droplevels(subset(dat_tree, treatment == "control"))
tree_boot = two.boot(dat_clipped$pine, dat_control$pine, FUN = mean, R = 10000, na.rm = TRUE)
quantile(tree_boot$t, c(0.025, 0.975))
mean(tree_boot$t)
     
# Questions 12-17
b_sidi_mean = mean(dat_all$b.sidi, na.rm = TRUE)
b_sidi_sd   = sd(dat_all$b.sidi, na.rm = TRUE)
dat_all$b.sidi.standardized = (dat_all$b.sidi - b_sidi_mean)/b_sidi_sd

s_sidi_mean = mean(dat_all$s.sidi, na.rm = TRUE)
s_sidi_sd   = sd(dat_all$s.sidi, na.rm = TRUE)
dat_all$s.sidi.standardized = (dat_all$s.sidi - s_sidi_mean)/s_sidi_sd

mean(dat_all$b.sidi.standardized)
sd(dat_all$b.sidi.standardized)

mean(dat_all$s.sidi.standardized)
sd(dat_all$s.sidi.standardized)

rm(list = ls())

dat_bird = read.csv(here("data", "bird.sub.csv"))
dat_habitat = read.csv(here("data", "hab.sub.csv"))

dat_all = merge(
  dat_bird, 
  dat_habitat,
  by = c("basin", "sub"))
dat_1 = 
  subset(
    dat_all,
    select = c(b.sidi, s.sidi))

fit_1 = lm(b.sidi ~ s.sidi, data = dat_all)
coef(fit_1)
slope_observed = coef(fit_1)[2]

# Start of my loop
rm(list = ls())

dat_bird = read.csv(here("data", "bird.sub.csv"))
dat_habitat = read.csv(here("data", "hab.sub.csv"))
dat_all = merge(dat_bird, dat_habitat,by = c("basin", "sub"))
dat_1 = subset(dat_all,select = c(b.sidi, s.sidi))

m = 10000 
result = numeric(m) 

for(i in 1:m)
{
  index_1 = sample(nrow(dat_1), replace = TRUE)
  index_2 = sample(nrow(dat_1), replace = TRUE)
  

  fit_1 = lm(b.sidi ~ s.sidi, data = dat_all)
  coef(fit_1)
  slope_observed = coef(fit_1)[2]
  
  dat_resampled_i = 
    data.frame(
      b.sidi = dat_1$b.sidi[index_1],
      s.sidi = dat_1$s.sidi[index_2]
    )
  
  fit_resampled_i = lm(b.sidi ~ s.sidi, data = dat_resampled_i)
  
  result[i] = coef(fit_resampled_i)[2]
} 

quantile(result, c(.05))
print(slope_observed)

hist(result, main = "Null Distribution of Regression Slope", xlab = "Slope Parameter")
abline(v = slope_observed, lty = "solid", col = "blue", lwd = 2)
abline(v = critical_value, lty = 2, col = "red", lwd = 2)

critical_value = quantile(result, c(.05))
print(critical_value)

print(slope_observed)
