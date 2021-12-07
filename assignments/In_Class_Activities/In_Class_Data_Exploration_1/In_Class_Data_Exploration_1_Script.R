pairs(iris)
pairs(iris[, c("Petal.Width", "Sepal.Width", "Sepal.Length")])

require(here)
dat_bird = read.csv(here("data", "bird.sta.csv"))
dat_habitat = read.csv(here("data", "hab.sta.csv"))

hist(dat_bird$CBCH, xlab = "Number of Birds Counted", main = "Histogram of Chestnut Beaked Chickadee Abundance", breaks = 0:7 - 0.5)

require(psych)
pairs.panels(iris)
pairs.panels(dat_habitat[c("elev", "slope", "ba.con")])
