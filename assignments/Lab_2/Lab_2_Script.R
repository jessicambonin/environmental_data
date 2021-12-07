# 1 equals TRUE
# 0 equals FALSE

# Questions 1 -2
n = 12345
vec_1 = sample(12, n, replace = TRUE)
head(vec_1)

vec_2 <- vec_1 == 3
vec_1[vec_2]

#Questions 3 - 5
n = 12345
vec_1 = sample(12, n, replace = TRUE)
head(vec_1)
length(vec_1)
sum(vec_1 == 3)

n = 10
vec_1 = sample(12, n, replace = TRUE)
paste0("Sum of elements with value 3: ", sum(vec_1 == 3))

# Question 6
for (i in 1:10)
{
  print((paste0("This is loop iteration: ", i)))
}

# Question 7 
for (i in 1:n)
{
  print(i)
}

# Question 8
n= 17
vec_1 = sample (1:10, n, replace = TRUE)
{print((paste0("The element of vec_1 at index ", (1:n), " is " , vec_1)))}

# Question 9 
create_and_print_vec = function(n, min = 1, max = 10)
{
  vec_3 <- sample(min:max, n, replace = TRUE) 
  for (i in 1:n)
  print(paste0("The element of at index ", i, " is " , vec_3[i]))
}
create_and_print_vec(100, min = 1, max = 10)
