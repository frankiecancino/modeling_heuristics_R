D_count = (.497 * 335877 * .47) + (.503 * 335877 * .59)
p_D = D_count / 335877
p_D

p_theta_D_male = (.47 * p_D) / (.497)
p_theta_D_male

p_theta_D_female = (.59 * p_D) / .503
p_theta_D_female

total_male = .497 * 335877
total_male

total_female = .503 * 335877
total_female

male_vote_D = total_male * p_theta_D_male
male_vote_D

female_vote_D = total_female * p_theta_D_female
female_vote_D

total_vote_D = male_vote_D + female_vote_D
as.integer(total_vote_D)

proportion = as.integer(total_vote_D) / 335877
proportion

proportion_other = 1 - proportion
proportion_other

votes_D = proportion * 335877
votes_D

votes_other = proportion_other * 335877
votes_other

diff = votes_D - votes_other
diff


## MORLEYS CODE
library(bayesAB)

A_s <- rbinom(100, 1, 169074/335877)
B_s <- rbinom(100, 1, 166526/335877)
AB1 <- bayesTest(A_s, B_s, priors = c('alpha' = 100, 'beta' = 117), distribution = 'bernoulli')
summary(AB1)
