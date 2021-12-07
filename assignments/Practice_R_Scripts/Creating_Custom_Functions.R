require(palmerpenguins)
require(here)

# Saving PNG
image_file = "ugly_histogram.png"
png(
  here("assignments", "Practice_R_Scripts", "Images", image_file),
  width = 1200, height = 1000)

hist(penguins$flipper_length_mm)
dev.off()

# Saving PNG using function
save_png_1 = function(image_file)
{
  require(here)
  png(
    here("assignments", "Practice_R_Scripts", "Images", image_file),
    width = 1200, height = 1000)
}

save_png_1("ugly_histogram_2.png")
hist(penguins$flipper_length_mm)
dev.off()

# Creating Histogram
dat_vec = penguins$body_mass_g
my_title = "Jessica's Histogram"
x_label = "Jessica's Data"
hist(
  dat_vec, 
  col = "steelblue", 
  main = my_title, 
  xlab = x_label)

# Creating histogram using function
steelblue_hist_fun = function(dat_vec, my_title, x_label)
{
  hist(
    dat_vec, 
    col = "steelblue", 
    main = my_title, 
    xlab = x_label)
}

steelblue_hist_fun(
  dat_vec = sample(x = 1:100, size = 1000, replace = TRUE),
  my_title = "Jessica's Random Numbers",
  x_label = "x-values"
)


