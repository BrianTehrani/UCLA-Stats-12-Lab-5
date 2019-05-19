#### Exercise 1 ####

flint <- read.csv(file.choose("flint_2015.csv"))

## a)
# H_o: p_o = .10, H_a: p_o > .10
# This is a one-sided test becasue we are checking for a proportion that is greater.

n_1 = length(flint$Pb)
p_o <- 0.10

## b) 
# proportion of dangerous lead levels in flint
sample_p_hat_flint <- mean(flint$Pb >= 15)
sample_p_hat_flint

# sd of flint lead levels
sd_p_hat_flint <- sd(flint$Pb >= 15)
sd_p_hat_flint

## c)
# Standard Error SE = sqrt(p_o(1-p_o)/n)
SE <- sqrt((p_o*(1-p_o))/n_1)
SE
#z-value for this test
z_score <- (sample_p_hat_flint - p_o) / SE
z_score 

## d) 
p_value <- 1 - pnorm(z_score)
p_value

## e)
# Since the p-value is less than the significance level (0.03225 < 0.05), we have enough
# to reject the null hypothesis. 

## f)
# Based on these results, I would say that remediation action should be taken. 

# g) 
library(mosaic)
prop.test(x = sum(flint$Pb >= 15), n = n_1, p = p_o, alt = "greater" )
# No the p-value increases a little but is relatively close to part d).

# h) for a confidence level of 99%
prop.test(x = sum(flint$Pb >= 15), n = 541, p = 0.1, alt = "greater",
          conf.level = 0.99)


#### Exercise 2 ####
## Optional ##
##a
# H_o: p_hat_1 = p_hat_2, H_a: p_hat_1 != p_hat_2
# This is a two-sided test.

# b) 
Conc_Pb_North <- flint$Pb[flint$Region == "North"]
n1 <- length(flint$Pb[flint$Region == "North"])
n1


temp <- Conc_Pb_North >= 15
success_p_hat_1 = 0
for(i in 1:length(temp)){
  if (temp[i] == TRUE){
    success_p_hat_1 = success_p_hat_1 + 1
  }
}
#Conc_Pb_North 
p_hat_1 <- success_p_hat_1 / n1
p_hat_1 

temp2 <- Conc_Pb_South >= 15
success_p_hat_2 = 0
for(i in 1:length(temp2)){
  if (temp2[i] == TRUE){
    success_p_hat_2 = success_p_hat_2 + 1
  }
}

Conc_Pb_South <- flint$Pb[flint$Region == "South"]
n2 <- length(flint$Pb[flint$Region == "South"])
n2

#Conc_Pb_South
p_hat_2 <- success_p_hat_2 / n2
p_hat_2

p_hat <- (success_p_hat_1 + success_p_hat_2)/(n1+n2)
p_hat

SE_two_prop <- sqrt(p_hat*(1-p_hat)*((1/n1)+(1/n2)))
SE_two_prop

z_score_2_prop <- (p_hat_1 - p_hat_2)/SE_two_prop
z_score_2_prop

# c) P-value
p_value_2_prop <- 2*(1-pnorm(z_score_2_prop))
p_value_2_prop 

# d) 
# Since the p-value is less than the test statistic, we have enough evidence
# to reject the null hypothesis.

# e)
prop.test(x = c(success_p_hat_1, success_p_hat_2), n = c(n1,n2), alt = "two.sided")

#### Exercise 3 ####
# a) H_o: mu = 40; H_a: mu != 40
# This is a two-sided test becuse we are finding a difference in Cu levels.

n_2 <- length(flint$Cu)

# b) sample mean and sample sd of Cu levels
samp_mean_Cu <- mean(flint$Cu)
samp_mean_Cu
samp_sd_Cu <- sd(flint$Cu)
samp_sd_Cu

# c) SE for sample mean for Cu
SE_Cu <- samp_sd_Cu/sqrt(n_2)
SE_Cu

# d) T-statistic and P-value
test_stat <- (samp_mean_Cu - 40)/ SE_Cu
test_stat
p_val <- 2*(1-pt(test_stat, df = n_2-1))
p_val

# e)
# Since the p-value is greater that the significance level (0.1123 > 0.01), we fail to reject the 
# null hypothesis and believe the average copper levels in Michigan is 40 ppm.

# f)
t.test(flint$Cu, mu = 40, alt = "two.sided")
# The results do not change if the significance level is 0.01, however
# if the significance level is 0.05 we have enough evidence to reject 
# the null hypothesis (0.01123 < 0.05).
