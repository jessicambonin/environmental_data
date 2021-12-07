# R’s probability functions

dpois(x = 7, lambda = 10.4)
dpois(x = 8, lambda = 10.4)

# Binomial Probabilities
# n: the number of trials. R calls this size.
# p: the probability of success on each trial. R calls this prob.

# Question 2: Binomial probability 1
??dbinom
# dbinom(x, size, prob, log = FALSE)

dbinom(4, 6, 2/3)

# Question 3: Binomial probability 2
dbinom(0, 6, 2/3)

# Cumulative probability: The p-functions
# probability that I observe a count of 7 or fewer if I have a poisson-distributed population with lambda = 10.4:
ppois(q = 7, lambda = 10.4)

# probability of observing a count greater than 7 using the law of total probability
1 - ppois(q = 7, lambda = 10.4)

# probability of observing seven or greater
1 - ppois(q = 6, lambda = 10.4)

# Question 4: Binomial Probabilities 3
??pbinom
# pbinom(q, size, prob, lower.tail = TRUE, log.p = FALSE)
pbinom(4, 6, 2/3)

# Question 5: Binomial Probabilities 4
# Probabiility that im NOT getting 0, 1, 2, or 3
1 - pbinom(3, 6, 2/3)

