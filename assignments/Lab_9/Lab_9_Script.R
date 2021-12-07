require(here)
catrate = read.csv(here("data", "catrate.csv"))
head(catrate)

n_success = sum(catrate$success)
n_years = sum(catrate$years)
binom.test(n_success, n_years)

late_fill_rate = 2/7
normal_fill_rate = 1 - late_fill_rate

binom.test(n_success, n_years, p = normal_fill_rate)

binom.test(n_success, n_years, p = normal_fill_rate, alternative ='less')

# F-distribution Example: Vegetation Data
veg = read.csv(here("data", "vegdata.csv"))
head(veg)

boxplot(pine ~ treatment, data = veg)

# Variance test
var.test(pine ~ treatment, data = veg, subset = treatment %in% c('control','clipped'))

# F-tests Assumes Normality
shapiro.test(veg$pine[veg$treatment=="control"])

shapiro.test(veg$pine[veg$treatment=="clipped"])

# Non-parametric Variance Test
fligner.test(pine ~ treatment, data = veg,
  subset = treatment %in% c('control','clipped'))

bartlett.test(pine ~ treatment, data=veg)

fligner.test(pine ~ treatment, data = veg)

# Comparing two sample means
t.test(pine ~ treatment, data = veg, subset = treatment %in% c('control','clipped'))

wilcox.test(pine ~ treatment, data = veg, subset = treatment %in% c('control','clipped'))

# Tests for paired samples
control = veg$pine[veg$treatment=='control']
clipped = veg$pine[veg$treatment=='clipped']

t.test(control, clipped, paired=TRUE)

wilcox.test(control, clipped, paired=TRUE)

# Correlation
disp = read.csv(here("data", "dispersal.csv"))
disp
plot(disp$disp.rate.ftb, disp$disp.rate.eb)
cor.test(disp$disp.rate.ftb, disp$disp.rate.eb, use='complete.obs')

cor.test(disp$disp.rate.ftb, disp$disp.rate.eb, use='complete.obs',
  method='spearman')

# Comparing two distributions
plot(ecdf(disp$disp.rate.ftb), verticals=TRUE)

plot(
  ecdf(disp$disp.rate.ftb),
  verticals=TRUE)
plot(
  ecdf(disp$disp.rate.eb),
  verticals=TRUE,
  lty=3,
  add=TRUE)

ks.test(disp$disp.rate.ftb,disp$disp.rate.eb)

# Comparing two or more proportions
prop.test(c(4,16),c(40,250))

# Dependence of variables in a contingency table
# Contingency: Chi-square test
owls = matrix(c(16, 9, 4, 11), nrow=2)
rownames(owls) = c("present", "absent")
colnames(owls) = c("old", "young")
chisq.test(owls)

fisher.test(owls)

birds   = read.csv(here("data", "bird.sta.csv"))
hab     = read.csv(here("data", "hab.sta.csv"))
birdhab = merge(birds, hab, by=c("basin", "sub", "sta"))

# Create a contingency table for edge/interior and brown creeper presence/absence
table(birdhab$s.edge, birdhab$BRCR > 0)

# set the presence to be in the first column
br_creeper_table = table(birdhab$s.edge, birdhab$BRCR > 0)[, 2:1]

# Question 1 - 2
chisq.test(br_creeper_table)

# Question 3
require(palmerpenguins)
fit_species = lm(formula = body_mass_g ~ species, data = penguins)

# Question 4
fit_sex = lm(formula = body_mass_g ~ sex, data = penguins)

# Question 5
fit_both=lm(formula=body_mass_g~species*sex,data=penguins)

# Question 6
boxplot(formula = body_mass_g ~ species, data = penguins, 
        main = "Body Mass by Species", xlab = "Species", 
        ylab = "Body Mass (g)", col = c("cyan3", "coral3", "darkseagreen3"))

# Question 7
boxplot(formula = body_mass_g ~ sex, data = penguins, 
        main = "Body Mass by Sex", xlab = "Sex", 
        ylab = "Body Mass (g)", names = c("Female", "Male"), 
        col = c("pink", "cadetblue2"))

# Question 8
boxplot(formula = body_mass_g ~ sex*species, data = penguins, 
        main = "Body Mass by Both \n Species and Sex", xlab = "Species by Sex", 
        ylab = "Body Mass (g)", 
        names = c("Adelie (F)", "Adelie (M)", "Chinstrap (F)", "Chinstrap (M)", "Gentoo (F)", "Getoo (M)"), 
        col = c("cyan3", "cyan3", "coral3", "coral3", "darkseagreen3", "darkseagreen3"))

# Question 10
bartlett.test(body_mass_g ~ species, data = penguins)

# Question 11
bartlett.test(body_mass_g ~ sex, data = penguins)

# Question 12
dat_groups = aggregate(body_mass_g ~ sex*species,data = penguins, FUN = c)
str(dat_groups)

bartlett.test(dat_groups$body_mass_g)
