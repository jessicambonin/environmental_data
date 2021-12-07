install.packages("palmerpenguins")
require(palmerpenguins)
require(here)

penguins = data.frame(penguins)
mean(penguins$body_mass_g)
head(penguins)

?mean
# na.rm	- a logical value indicating whether NA values should be stripped before the computation proceeds.
# na.rm = false is the default

penguins = data.frame(penguins)
mean(penguins$body_mass_g, na.rm = TRUE)
summary(penguins)

# Pair Plot
plot(penguins)

# Histogram
hist(penguins$body_mass_g, xlab = "Body Mass (g)", main = "Penguin Body Mass")

# Scatterplot
plot(x = penguins$body_mass_g, y = penguins$flipper_length_mm, xlab = "Body Mass (g)", ylab = "Flipper Length (mm)", main = "Penguin Body Mass by Flipper Length")

# Boxplots
boxplot(penguins$bill_depth_mm)
boxplot(bill_depth_mm ~ sex, data = penguins)

par(mfrow = c(1, 2))
boxplot(penguins$bill_depth_mm)
boxplot(bill_depth_mm ~ sex, data = penguins)

# Coplot
coplot(body_mass_g ~ bill_depth_mm | sex, data = penguins)

# Coplot changing conditioning varibable
coplot(body_mass_g ~ bill_depth_mm | species, data = penguins)

# Saving Plots to File
require(here)
png(filename = here("coplot_bonin.png"), width = 800, height = 600)
coplot(body_mass_g ~ bill_depth_mm | species, data = penguins)
dev.off()

