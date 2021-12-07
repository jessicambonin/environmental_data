# Nicer Pairplots
# Dont re instal packages every time
install.packages("psych")
require(psych)
pairs.panels(iris)

# Data
require(here)
dat_bird = read.csv(here("data", "bird.sta.csv"))
head(dat_bird)

dat_habitat = read.csv(here("data", "hab.sta.csv"))
head(dat_habitat)

# Merging Data
dat_all = merge(dat_bird, dat_habitat)
plot(ba.tot ~ elev, data = dat_all)

# Convert to presence/absence
sample(dat_all$CEWA, 100)

# Challenge: How could I use R to calculate the total number of waxwings and the number of sites in which they were present?
sum(dat_all$CEWA)

# Presence/Absence 
my_vec = rep(1:3, 5)
my_vec == 3
my_vec > 1

# Data Type Coercion
as.numeric(my_vec > 1)

cewa_present_absent = as.numeric(dat_all$CEWA > 0)
cewa_present_absent 
plot(dat_all$elev, cewa_present_absent)

# Function to calculate the logistic parameter a given the slope and midpoint
get_logistic_param_a = function(slope, midpoint)
{
  b = slope / 4
  return (-midpoint * (slope / 4))
}

# Function to calculate the logistic parameter b given the slope
get_logistic_param_b = function(slope)
{
  return (slope / 4)
}


# Calculate the value of the logistic function at x, given the parameters a and b.
logistic = function(x, a, b)
{
  val = exp(a + b * x)
  return(val / (1 + val))
}

# Calculate the value of the logistic function at x, given a slope and midpoint.
logistic_midpoint_slope = function(x, midpoint, slope)
{
  b = get_logistic_param_b(slope)
  a = get_logistic_param_a(slope, midpoint)
  return(logistic(x, a, b))
}

# Logistic Fit 1
plot(x = dat_all$elev, y = cewa_present_absent)
curve(logistic_midpoint_slope(x, midpoint = 400, slope = 0.1), add = TRUE)

# Logistic Fit 2
plot(x = dat_all$elev, y = cewa_present_absent)
curve(logistic_midpoint_slope(x, midpoint = 400, slope = -0.1), add = TRUE)

# Logistic Fit 3
plot(x = dat_all$elev, y = cewa_present_absent)
curve(logistic_midpoint_slope(x, midpoint = 400, slope = -0.05), add = TRUE)

# Does elevation look like a good predictor of cedar waxwing presence/absence? - NO

# Questions
pairs.panels(dat_habitat[c("elev", "aspect", "slope", "ba.tot")])

# Black-headed Grosbeak=BHGR, Chestnut-bk Chickadee=CBCH Plots
bhgr_present_absent = as.numeric(dat_all$BHGR > 0)
bhgr_present_absent 
plot(dat_all$ba.tot, bhgr_present_absent, xlim = c(0, 150), xlab = "Basal Area (m2 per ha)", ylab = "Presence/Absence", main = "Black-headed Grosbeak - Presence/Absence", cex = 2, col = adjustcolor("aquamarine4", 0.1))
curve(logistic_midpoint_slope(x, midpoint = 15, slope = -1), add = TRUE)

cbch_present_absent = as.numeric(dat_all$CBCH > 0)
cbch_present_absent 
plot(dat_all$ba.tot, cbch_present_absent, xlab = "Basal Area (m2 per ha)", ylab = "Presence/Absence", main = "Chestnut-bk Chickade - Presence/Absence", cex = 2, col = adjustcolor("darkorchid4", 0.2))
curve(logistic_midpoint_slope(x, midpoint = 25, slope = 0.5), add = TRUE)

# How to Save as PNG
png(filename= here("assignments", "Lab_3", "Black_headed_Grosbeak_Presence_Absence.png"), width = 1600, height = 1400)
plot(dat_all$ba.tot, bhgr_present_absent, xlab = "Basal Area (m2 per ha)", ylab = "Presence/Absence", main = "Black-headed Grosbeak - Presence/Absence", cex = 2, col = adjustcolor("aquamarine4", 0.2))
curve(logistic_midpoint_slope(x, midpoint = 15, slope = -1), add = TRUE)
dev.off()

# Question 7 and 8 - Gray Jay=GRJA
sum(dat_all$GRJA)

# Question 9 and 10
grja_present_absent = as.numeric(dat_all$GRJA > 0)
grja_present_absent 
sum(grja_present_absent)
