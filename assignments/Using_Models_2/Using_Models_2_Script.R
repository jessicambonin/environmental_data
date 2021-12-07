require(palmerpenguins)
penguins -> palmerpenguins

t.test(subset(penguins, species == "Gentoo")$flipper_length_mm)

t.test(
  x = subset(penguins, species == "Gentoo")$flipper_length_mm,
  mu = 218)

t.test(
  x = subset(penguins, species == "Gentoo")$flipper_length_mm,
  mu = 218, alternative = "less")

t.test(flipper_length_mm ~ species, data = subset(penguins, species != "Chinstrap"))

par(mfrow = c(1, 2))
hist(penguins$body_mass_g, breaks = 80, main = "histogram of body mass", xlab = "body mass (g)")
plot(density(penguins$body_mass_g, na.rm = TRUE), main = "density plot of body mass")

require(palmerpenguins)
boxplot(body_mass_g ~ species, data = penguins)

dat_chinstrap = subset(penguins, species == "Chinstrap")
mean(dat_chinstrap$body_mass_g, na.rm = TRUE)

shapiro.test(dat_chinstrap$body_mass_g)

aggregate(body_mass_g ~ species, data = penguins, FUN = mean)
aggregate(body_mass_g ~ species,data = penguins,
  FUN = function(x) shapiro.test(x)$p.value)

fit_species = lm(body_mass_g ~ species, data = penguins)
summary(fit_species)

anova(fit_species)

fit_species = lm(body_mass_g ~ species, data = penguins)
summary(fit_species)

anova(fit_species)
boxplot(body_mass_g ~ species, data = penguins)

fit_additive = lm(body_mass_g ~ sex + species, data = penguins)

fit_interactive = lm(body_mass_g ~ sex .....)

require(palmerpenguins)

#Question 1
boxplot(formula = body_mass_g ~ sex*species, data = penguins, 
        main = "Body Mass by Both \n Species and Sex", xlab = "Species by Sex", 
        ylab = "Body Mass (g)", 
        names = c("Adelie \n (F)", "Adelie \n (M)", "Chinstrap \n (F)", "Chinstrap \n (M)", "Gentoo \n (F)", "Getoo \n (M)"), 
        col = c("cyan3", "cyan3", "coral3", "coral3", "darkseagreen3", "darkseagreen3"))

# Question 4
fit_both=lm(formula=body_mass_g~species*sex,data=penguins)
summary(fit_both)

# Question 6
summary(fit_both)

# Question 7
3368.84 +  158.37 

# Question 8 
dat_female_chinstrap = subset(dat_chinstrap, sex == "female")
mean(dat_female_chinstrap$body_mass_g)
