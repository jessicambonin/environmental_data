qnorm(c(0.025, 0.975))

# Question 1
qnorm(c(0.05, 0.95))

# Question 2
# qt(p, df, ncp, lower.tail = TRUE, log.p = FALSE)
qt(0.975, 10, lower.tail = FALSE)
qt(0.975, 10, lower.tail = TRUE)

# Question 3
qnorm(.025)
qt(0.025, 100)

# Question 4
qnorm(.025)
qt(0.025, 475)

# Question 5
3.14 / sqrt(50)

# Question 6
10-(0.4440631*qt(.025, 49))
10+(0.4440631*qt(.025, 49))
