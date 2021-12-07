install.packages("here")
library(here)
file.exists(here("documents", "environmental_data", "data", "hab.sta.csv"))
dat_habitat <- data.frame(read.csv(here("documents", "environmental_data", "data", "hab.sta.csv")))

par(mfrow = c(2, 3))

# Histogram - Elevation
hist(dat_habitat$elev,xlab = "Elevation (m)", main = "Histogram of Elevation")
# Histogram - Aspect
hist(dat_habitat$aspect,xlab = "Aspect (degrees)", breaks = c(0, 90, 180, 270, 360), main = "Histogram of Aspect")
# Histogram - Slope
hist(dat_habitat$slope,xlab = "Slope (%)", main = "Histogram of Slope")

# Scatterplot - Elevation - Linear Regression
plot(dat_habitat$elev, dat_habitat$ba.tot, xlab = "Elevation (m)", ylab = "Total Basal Area", main = "Basal Area and Eleveation", cex = .5, col = "blue")
abline(lm(dat_habitat$ba.tot ~ dat_habitat$elev), col = "red")

# Scatterplot - Aspect - Linear Regression
plot(dat_habitat$aspect, dat_habitat$ba.tot, xlab = "Aspect (degrees)", ylab = "Total Basal Area", main = "Basal Area and Aspect", cex = .5)
abline(lm(dat_habitat$ba.tot ~ dat_habitat$aspect), col = "red")

# Scatterplot - Slope - Linear Regression
plot(dat_habitat$slope, dat_habitat$ba.tot, xlab = "Slope (%)", ylab = "Total Basal Area", main = "Basal Area and Slope", cex = .5, col = "pink")
abline(lm(dat_habitat$ba.tot ~ dat_habitat$slope), col = "black")



# Scatterplot - Elevation
data_center_x = mean(dat_habitat$elev)
data_center_y = mean(dat_habitat$ba.tot)
c(data_center_x, data_center_y)
plot(dat_habitat$elev, dat_habitat$ba.tot, xlab = "Elevation (m)", ylab = "Total Basal Area", main = "Basal Area and Eleveation", cex = .5)
points(x = data_center_x, y = data_center_y, col = "red")
curve(
  line_point_slope(
    x, 
    data_center_x, 
    data_center_y,
    .2), 
  add = TRUE)

# Scatterplot - Aspect
data_center_x = mean(dat_habitat$aspect)
data_center_y = mean(dat_habitat$ba.tot)
c(data_center_x, data_center_y)
plot(dat_habitat$aspect, dat_habitat$ba.tot, xlab = "Aspect (degrees)", ylab = "Total Basal Area", main = "Basal Area and Aspect", cex = .5)
points(x = data_center_x, y = data_center_y, col = "red")
curve(
  line_point_slope(
    x, 
    data_center_x, 
    data_center_y,
    0), 
  add = TRUE)

# Scatterplot - Slope
data_center_x = mean(dat_habitat$slope)
data_center_y = mean(dat_habitat$ba.tot)
c(data_center_x, data_center_y)
plot(dat_habitat$slope, dat_habitat$ba.tot, xlab = "Slope (%)", ylab = "Total Basal Area", main = "Basal Area and Slope", cex = .5)
points(x = data_center_x, y = data_center_y, col = "red")
curve(
  line_point_slope(
    x, 
    data_center_x, 
    data_center_y,
    .2), 
  add = TRUE)