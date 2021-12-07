x_observed = c(2, 6)
print(x_observed)

dpois(x = 2, lambda = 4.5)
dpois(x = 6, lambda = 4.5)

dpois(x = 2, lambda = 4.5) * dpois(x = 6, lambda = 4.5)

wiwa_counts = c(2, 6)
dpois(x = wiwa_counts, lambda = 4.5)

# Product of Likelihoods
prod(dpois(x = wiwa_counts, lambda = 4.5))

# Sum of Log-Likelihoods
sum(log(dpois(x = wiwa_counts, lambda = 4.5)))

# Likelihood of Many Observations
dat_bird = read.csv(here::here("data", "bird.sta.csv"))
dat_habitat = read.csv(here::here("data", "hab.sta.csv"))
dat_all = merge(dat_bird, dat_habitat)

# Numerical Data Exploration
summary(dat_all$WIWA)

# Graphical Exploration: Histograms

# Default histogram
hist(dat_all$WIWA)

# Histogram with custom breaks
hist(dat_all$WIWA, breaks = 7)

hist(dat_all$WIWA, breaks = 0:7)


0:7 - 0.5
hist(dat_all$WIWA, breaks = 0:7 - .5)

# Histograms with (discrete) count data
par(mfrow = c(1, 2))
dat = dat_all$WIWA
hist(dat, breaks = 0:(max(dat) + 1) - 0.5, main = "Histogram of\nWilson's Warbler counts")

dat = dat_all$GRJA
hist(dat, breaks = 0:(max(dat) + 1) - 0.5, main = "Histogram of\nGray Jay counts")

# Wilson’s Warbler Sum of Log-Likelihoods
sum(log(dpois(x = dat_all$WIWA, lambda = 1.0)))

# Question 1
wiwa_counts = c(2, 6)
dpois(x = wiwa_counts, lambda = 4.5)
sum(log(dpois(x = wiwa_counts, lambda = 4.5)))
sum(log(dpois(x = wiwa_counts, lambda = 4)))
sum(log(dpois(x = wiwa_counts, lambda = 5)))
sum(log(dpois(x = wiwa_counts, lambda = 1)))
sum(log(dpois(x = wiwa_counts, lambda = 3)))
sum(log(dpois(x = wiwa_counts, lambda = 3.9)))
sum(log(dpois(x = wiwa_counts, lambda = 4.1)))

# Question 2
# Winter Wren = WIWR
sum(log(dpois(x = dat_all$WIWR, lambda = 1.0)))
sum(log(dpois(x = dat_all$WIWR, lambda = 2.0)))
sum(log(dpois(x = dat_all$WIWR, lambda = 0.5)))
sum(log(dpois(x = dat_all$WIWR, lambda = 3.0)))
sum(log(dpois(x = dat_all$WIWR, lambda = 2.5)))
sum(log(dpois(x = dat_all$WIWR, lambda = 2.1)))
sum(log(dpois(x = dat_all$WIWR, lambda = 1.5)))
sum(log(dpois(x = dat_all$WIWR, lambda = 1.4)))
sum(log(dpois(x = dat_all$WIWR, lambda = 1.6)))

# Question 3
sum(log(dbinom(dat_all$WIWR, 6, .24)))

# In Class Discussion
n = 20
prob_guess = 1.45/n
prob_guess

sum(log(dbinom(x=dat_all$WIWR, size=n, prob = prob_guess + 0.0001)))
     
