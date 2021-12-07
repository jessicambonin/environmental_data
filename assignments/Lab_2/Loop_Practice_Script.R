for(i in 1:2)
{
  print(i)
}  

# Only create "i" variables in loops

for(i in c(3, 6, 77))
{
  print(i)
}  

# After loop, "i" gets value of last run

# Rep repeats
rep(1:3, 2)

for(i in rep(1:3, 2))
{
  print(i)
}   