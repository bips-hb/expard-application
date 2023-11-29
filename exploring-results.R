library(expard)
library(readr)
library(ggplot2)

get_fit <- function(i) {
  readr::read_rds(fit_filenames[i])
}

plot_fit_local <- function(i, exclude_null = FALSE) {
  fit <- get_fit(i)
  
  if(exclude_null) {
    fit <- fit %>% filter(model != 'no-association')
  }
  
  expard::plot_fit(fit)
} 

plot_fit_past <- function(i, add_current_use = FALSE, past_range = c(1,55)) {
  fit <- get_fit(i)
  
  if (add_current_use) {
    fit <- fit %>% filter(model == 'past-use' | model == 'current-use') %>%
      arrange(model)
    fit[1, 'past'] <- 0 
    
    fit <- fit %>% 
      filter((past >= past_range[1] & past <= past_range[2]) | past == 0)
    
  } else {
    fit <- fit %>% filter(model == 'past-use') %>% 
      filter(past >= past_range[1] & past <= past_range[2]) 
  }
  
  ggplot(fit, mapping = aes(x = past, y = BIC, color = model)) + 
    geom_point()
}


fit_filenames <-
  c(
    "results/fit_penicillin_shock.rds", 
    "results/fit_doacs_bleeding.rds", 
    "results/fit_nsaids_hf.rds", 
    "results/fit_oc_shock.rds", 
    "results/fit_antibiotics_bleeding.rds",  
    "results/fit_atd_bleeding.rds",
    "results/fit_diabetics_bleeding.rds",
    "results/fit_psychotics_type2diabetes.rds",
    "results/fit_psychotics_clo_type2diabetes.rds",
    "results/fit_psychotics_ola_type2diabetes.rds",
    "results/fit_psychotics_que_type2diabetes.rds"
  )

i <- 3

plot_fit_local(i, exclude_null = FALSE)
plot_fit_local(i, exclude_null = TRUE)
plot_fit_past(i, add_current_use = T) #, past_range = c(1,10))



# expard::plot_risk(drug_history = c(rep(0,4), rep(1,5), rep(0, 47)), 
#                  risk_model = expard::risk_model_past(30))


fit_filename = fit_filenames[4]
