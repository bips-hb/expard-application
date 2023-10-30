# determine the AIC

library(expard)

x <- readr::read_rds("results/fit_diabetics_bleeding.rds")
colnames(x)

x <- x %>% mutate(AIC = -2*n_param + 2*loglikelihood) %>% 
  group_by(model) %>% 
  mutate(bestAIC = min(AIC)) %>% 
  ungroup() 

y <- x %>% filter(model != 'past-use')

plot(y$AIC,y$BIC)
