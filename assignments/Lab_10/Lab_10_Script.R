rm(list = ls())
require(here)
read.csv(here("data", "rope.csv"))

rope = read.csv(here("data", "rope.csv"))
rope$rope.type = factor(x = rope$rope.type)
levels(rope$rope.type)
                        
n_obs = length(rope$rope.type)
n_groups = length(levels(rope$rope.type))

mean_cut= mean(rope$p.cut)                       
resids_cut = rope$p.cut - mean_cut
ss_tot = sum(resids_cut^2)
df_tot = n_obs - 1

agg_resids = aggregate(
  x = rope$p.cut,
  by = list(rope$rope.type),
  FUN = function(x){x-mean(x)})                        
str(agg_resids)

agg_sq_resids =  aggregate(
  x = rope$p.cut,
  by = list(rope$rope.type),
  FUN = function(x){sum((x-mean(x))^2)}) 
str(agg_sq_resids)

ss_within = sum(agg_sq_resids$x)
df_within = 115
                        
ss_among = ss_tot - ss_within
df_among = 5
                        
ms_within = ss_within / (n_obs - n_groups)
ms_among  = ss_among / (n_groups - 1)
                        
f_ratio = ms_among / ms_within
f_pval = pf(f_ratio, df_among, df_within, lower.tail = FALSE)

fit_1 = lm(p.cut ~ rope.type, data=rope)
anova(fit_1)
anova_fit_1 = anova(fit_1)
str(anova_fit_1)

anova_fit_1$Sum Sq
anova_fit_1$"Sum Sq"

# Self Check
# number comparison tolerance
digits_check = 5

# Build the reference model using R functions
fit_1 = lm(p.cut ~ rope.type, data=rope)
anova(fit_1)
anova_fit_1 = anova(fit_1)

# Check degrees of freedom
anova_fit_1$Df == c(df_among, df_within)

# Check sums of squares
round(anova_fit_1$`Sum Sq`, digits = digits_check) == round(c(ss_among, ss_within), digits = digits_check)

# Check mean squares
round(anova_fit_1$`Mean Sq`, digits = digits_check) == round(c(ms_among, ms_within), digits = digits_check)

# Check the F-ratio
round(anova_fit_1$`F value`[1], digits = digits_check) == round(f_ratio, digits = digits_check)

# Check the F test statistic p-value
round(anova_fit_1$`Pr(>F)`[1], digits = digits_check) == round(f_pval, digits = digits_check)

# Question 3
bartlett.test(p.cut ~ rope.type, data = rope)

# Question 5
fit_rope_1 = lm(p.cut ~ rope.type, data = rope)
summary(fit_rope_1)

# Question 7
0.36714 + (1 * -0.10164)
