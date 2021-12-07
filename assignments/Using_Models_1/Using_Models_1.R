require(here)
catrate = data.frame(read.csv(here("data", "catrate.csv")))
head(catrate)
summary(catrate) 
hist(catrate$cat.rate)
shapiro.test(catrate$cat.rate)

t.test(catrate$cat.rate, mu = 0.28)

# One sided 
t.test(catrate$cat.rate, mu = 0.28, alternative = "greater")
t.test(catrate$cat.rate, mu = 0.28, alternative = "less")

wilcox.test(catrate$cat.rate, mu = 2 / 7, exact = FALSE)
wilcox.test(catrate$cat.rate, mu = 2 / 7, exact = FALSE, alternative = "less")
wilcox.test(catrate$cat.rate, mu = 2 / 7, exact = FALSE, alternative = "greater")

require(palmerpenguins)
penguin_dat = droplevels(subset(penguins, species != "Gentoo"))

summary(penguin_dat)

boxplot(flipper_length_mm ~ species, data = penguin_dat,
  ylab = "Flipper Length (mm)")

dat_adelie = subset(penguin_dat, species == "Adelie")
dat_chinstrap = subset(penguin_dat, species == "Chinstrap")
shapiro.test(dat_adelie$flipper_length_mm)
shapiro.test(dat_chinstrap$flipper_length_mm)

t.test(flipper_length_mm ~ species, data = penguin_dat)
wilcox.test(flipper_length_mm ~ species, data = penguin_dat, exact = FALSE)

levels(penguin_dat$species)

# Questions 1
hist(catrate$cat.rate, main = "Salamander Reproduction Catastrophic Rates", xlab = "Rate", col = "darkblue")

# Question 2
shapiro.test(catrate$cat.rate)

# Question 5
t.test(catrate$cat.rate, mu = 0.28)
mean(catrate$cat.rate)

# Question 8
t.test(catrate$cat.rate, mu = 0.28, alternative = "greater")

# Question 9

# Question 11
wilcox.test(catrate$cat.rate, mu = 2 / 7, exact = FALSE, alternative = "greater")

# Question 16
dat_adelie = subset(penguin_dat, species == "Adelie")
dat_chinstrap = subset(penguin_dat, species == "Chinstrap")
shapiro.test(dat_adelie$flipper_length_mm)
shapiro.test(dat_chinstrap$flipper_length_mm)

hist(dat_adelie$flipper_length_mm)
hist(dat_chinstrap$flipper_length_mm)

# Question 18
png(
  here("images", "Models_1_hist.png"), 
  width = 1800, height = 1000, 
  res=180)

par(mfrow = c(1, 2))
hist(dat_adelie$flipper_length_mm, main = "Adelie Flipper Length", xlab = "Flipper Length (mm)", col = "darkred")
hist(dat_chinstrap$flipper_length_mm, main = "Chinstrap Flipper Length", xlab = "Flipper Length (mm)", col = "darkred")

dev.off()
# Question 19
t.test(flipper_length_mm ~ species, data = penguin_dat)

