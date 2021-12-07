# Question 1
c(1, 2, 3)
"c(1, 2, 3)"

# Question 2 - 4
c_1 = c(1, 2, 3)
c_2 = "c(1, 2, 3)"

# Question 5 - 6
my_vec <- c(1, 2, 3)
mat_1 = matrix(my_vec)
mat_1
mat_1[3,1]

# Question 7 - 11
mat_2 = matrix(my_vec, nrow = 2, ncol = 3, byrow = TRUE)
mat_3 = matrix(my_vec, nrow = 3, ncol = 2, byrow - TRUE)
mat_4 = matrix(my_vec, nrow = 5, ncol = 2, byrow = TRUE)

# Question 12 - 14
my_list_1 <- list(5.2, "five point two", c(1, 2, 3, 4, 5))
names(my_list_1) <- c("two", "one", "three")
# Line 1
my_list_1[[1]]
# Line 2
my_list_1[[as.numeric("1")]]
# Line 3
my_list_1[["1"]]
# Line 4
my_list_1[["one"]]
# Line 5
my_list_1$one
# Line 6
my_list_1$"one"
# Line 7
my_list_1$1
# Line 8
my_list_1$"1"

# Question 12 - 14: Changing for Answers
my_list_1 <- list(5.2, "five point two", c(1, 2, 3, 4, 5))
names(my_list_1) <- c("two", "one", "three")
# Line 1 - Position : Double brackets picks the numbered position
my_list_1[[1]]
my_list_1[[2]]
# Line 2 - Position
my_list_1[[as.numeric("1")]]
my_list_1[[as.numeric("2")]]
# Line 3 - Name
my_list_1[["1"]]
my_list_1[["one"]]
# Line 4 - Name
my_list_1[["one"]]
my_list_1[["two"]]
# Line 5 - Name
my_list_1$one
my_list_1$two
# Line 6 - Name
my_list_1$"one"
my_list_1$"two"
# Line 8 - Name
my_list_1$"1"
my_list_1$"one"
