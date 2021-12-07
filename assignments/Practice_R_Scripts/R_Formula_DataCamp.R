# Create variables
a <- c(1,2,3,4,5,6,7,8,9)

b <- list(x = LifeCycleSavings[,1], y = LifeCycleSavings[,2])

# Retrieve the types of `a` and `b`
typeof(a)

typeof(b)

# Retrieve the classes of `a` and `b`
class(a)

class(b)

# Retrieve the object type
typeof(quote(x * 10))

# Retrieve the class
class(quote(x * 10))

# A formula
c <- y ~ x
d <- y ~ x + b

# Double check the class of `c`
class(c)

# Return the type of `d`
typeof(d)

# Retrieve the attributes of `d`
attributes(d)

e <- ~ x + y + z
f <- y ~ x + b 

# Return the length of `g`
length(e)
length(f)

# Retrieve the elements at index 1 and 2
e[[1]]
e[[2]]
f[[3]]

# How to create a formula
y ~ x
~ x + y + z
g <- y ~ x + b

"y ~ x1 + x2"

h <- as.formula("y ~ x1 + x2")

h <- formula("y ~ x1 + x2")

# How To Concatenate Formulae
# Create variables
i <- y ~ x
j <- y ~ x + x1
k <- y ~ x + x1 + x2

# Concatentate
formulae <- list(as.formula(i),as.formula(j),as.formula(k))

# Double check the class of the list elements
class(formulae[[1]])

# Join all with `c()`
l <- c(i, j, k)

# Apply `as.formula` to all elements of `f`
lapply(l, as.formula)


# Formula Operators
# Use multiple independent variables
y ~ x1 + x2

# Ignore objects in an analysis
y ~ x1 - x2

# Same Regression
y ~ x1 * x2

y ~ x1 + x2 + x1:x2

# Set seed
set.seed(123)

# Data
x = rnorm(5)
x2 = rnorm(5)
y = rnorm(5)

# Model frame
model.frame(y ~ x * x2, data = data.frame(x = x, y = y, x2=x2))
model.frame(y ~ x + x2 + x:x2, data = data.frame(x = x, y = y, x2))

# Nesting
y ~ a + b %in% a
model.frame( y ~ x + x^2, data = data.frame(x = rnorm(5), y = rnorm(5)))
y ~ x + x^2

model.frame( y ~ x + I(x^2), data = data.frame(x = rnorm(5), y = rnorm(5)))
y ~ x + I(x^2)

y ~ I(2 * x)
# Polynomial Regression
y ~ x + I(x^2) + I(x^3)

# Factorial ANOVA
y ~ (a*b*c)^2
y ~ .
