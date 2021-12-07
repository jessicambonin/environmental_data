require(here)
here("data", "catrate.csv")
dat_catrate <- read.csv("/Users/jessicabonin/Documents/environmental_data/data/catrate.csv")

here("data", "delomys.csv")
dat_delomys <- read.csv("/Users/jessicabonin/Documents/environmental_data/data/delomys.csv")

here("data", "rope.csv")
dat_rope <- read.csv("/Users/jessicabonin/Documents/environmental_data/data/rope.csv")

# For easy sharing of code
dat_rope<-read.csv(here("data", "rope.csv")) 
dat_catrate<-read.csv(here("data", "catrate.csv"))              
dat_delomys<-read.csv(here("data"  , "delomys.csv")) 

head(dat_catrate)
head(dat_delomys)
head(dat_rope)

plot(dat_catrate$pond, dat_catrate$success, xlab = "Pond", ylab ="Success", main = "Jessica Bonin")
plot(dat_delomys$body_length, dat_delomys$body_mass, xlab = "Body Length", ylab = "Body Mass", main = "Jessica Bonin")
hist(dat_rope$p.strength, xlab = "Strength", ylab = "Frequency", main = "Jessica Bonin")
