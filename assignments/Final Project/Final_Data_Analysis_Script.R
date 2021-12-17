require(here)
delomys <- read.csv(here("data", "delomys.csv"))

summary(delomys$body_length, delomys$body_mass)
shapiro.test(delomys$body_length)
shapiro.test(delomys$body_mass)

require(palmerpenguins)
plot(bill_length_mm ~ body_mass_g, data = penguins)

# Questions 1-4
plot(body_length ~ body_mass, data = delomys)
hist(delomys$body_mass)
hist(delomys$body_length, breaks = 30)
boxplot(body_mass ~ binomial, data = delomys)
boxplot(body_mass ~ sex, data = delomys)
boxplot(body_mass ~ sex*binomial, data = delomys)

# Questions 5-6
fit1 <- lm(body_length ~ body_mass, data = delomys)
anova(fit1)
fit2 <- lm(body_mass ~ sex, data = delomys)
anova(fit2)
fit3 <- lm(body_mass ~ binomial, data = delomys)
anova(fit3)
fit4 <- lm(body_mass ~ sex + binomial, data = delomys)
anova(fit4)
fit5 <- lm(body_mass ~ sex * binomial, data = delomys)
anova(fit5)

hist(residuals(fit1))
hist(residuals(fit2))
hist(residuals(fit3))
hist(residuals(fit4))
hist(residuals(fit5))

shapiro.test(residuals(fit1))
shapiro.test(residuals(fit2))
shapiro.test(residuals(fit3))
shapiro.test(residuals(fit4))
shapiro.test(residuals(fit5))

# Question 7-9
coef(summary(fit1))
coef(summary(fit2))
coef(summary(fit3))
coef(summary(fit4))
coef(summary(fit5))

plot(delomys$body_mass,delomys$body_length)
abline(lm(body_length ~ body_mass, data = delomys), col = "red")

76.1246565 +  0.8754988 
76.1246565 + (0.8754988*100)

# Question 17-18
AIC(fit1)
AIC(fit2)
AIC(fit3)
AIC(fit4)
AIC(fit5)
