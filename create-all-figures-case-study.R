library(expard)
library(readr)
library(ggplot2)

get_fit <- function(i) {
  readr::read_rds(fit_filenames[i])
}


plot_fit_past <- function(i, add_current_use = TRUE,
                          x_label = "past parameter (in quarter years)", 
                          y_label = "BIC",
                          title = "",
                          y_range = NULL, 
                          past_range = c(1,55)) {
  
  fit <- get_fit(i)
  
  fit <- fit %>% filter(model == 'past-use' | model == 'current-use') %>% arrange(model)
  fit$model = c("current use", rep('past use', 55))
  
  if (add_current_use) {
    fit <- fit %>% filter(model == 'past use' | model == 'current use') %>%
      arrange(model)
    fit[1, 'past'] <- 0 
    
    fit <- fit %>% 
      filter((past >= past_range[1] & past <= past_range[2]) | past == 0)
    
  } else {
    fit <- fit %>% filter(model == 'past use') %>% 
      filter(past >= past_range[1] & past <= past_range[2]) 
  }
  
  ggplot(fit, mapping = aes(x = past, y = BIC, color = model)) + 
    geom_point() + 
    scale_color_manual(values = c("current use" = "#fa8537", "past use" = "#1763aa")) + 
    coord_cartesian(ylim=y_range) + 
    ggtitle(title) + 
    theme_bw() +
    xlab(x_label) +
    ylab(y_label) + 
    scale_x_continuous(breaks = seq(0, 56, by = 20),
                       minor_breaks = seq(0, 56, by = 4))
}

fit_filenames <-
  c(
    "results/fit_penicillin_shock.rds", #
    "results/fit_doacs_bleeding.rds",   # 
    "results/fit_nsaids_hf.rds",        # 
    "results/fit_oc_shock.rds",         
    "results/fit_antibiotics_bleeding.rds",  # negative control
    "results/fit_atd_bleeding.rds",
    "results/fit_diabetics_bleeding.rds",    # negative control
    "results/fit_psychotics_type2diabetes.rds", #
    "results/fit_psychotics_clo_type2diabetes.rds",
    "results/fit_psychotics_ola_type2diabetes.rds",
    "results/fit_psychotics_que_type2diabetes.rds"
  )

# "results/fit_penicillin_shock.rds"
i = 1

fit <- get_fit(i) %>% filter(model == 'no-association')
fit$BIC
p <- expard::plot_fit(fit = get_fit(i), 
                      title = "penicillin and anaphylaxis", 
                      y_range = c(5150, 5230))
ggsave("results/figure_penicillin_shock.pdf", p, width = 5, height = 5)

# "results/fit_doacs_bleeding.rds"
i = 2

fit <- get_fit(i) %>% filter(model == 'no-association')
fit$BIC

p <- expard::plot_fit(fit = get_fit(i), 
                      title = "DOACs and GI bleeding", 
                      y_range = c(224500, 226000))
ggsave("results/figure_doacs_bleeding.pdf", p, width = 5, height = 5)

p <- plot_fit_past(i, add_current_use = T, title = "DOACs and GI bleeding")
ggsave("results/past_figure_doacs_bleeding.pdf", p, width = 4.5, height = 4)

# "results/fit_antibiotics_bleeding.rds"
i = 5

p <- expard::plot_fit(fit = get_fit(i), 
                      title = "antibiotics and GI bleeding")
ggsave("results/figure_antibiotics_bleeding.pdf", p, width = 5, height = 5)

p <- plot_fit_past(i, add_current_use = T, title = "antibiotics and GI bleeding")
ggsave("results/past_figure_antibiotics_bleeding.pdf", p, width = 4.5, height = 4)

# "results/fit_psychotics_type2diabetes.rds"
i = 8

fit <- get_fit(i) %>% filter(model == 'no-association')
fit$BIC

p <- expard::plot_fit(get_fit(i), y_range = c(214000, 216300),
                      title = "antipsychotics and type 2 diabetes")
ggsave("results/figure_psychotics_type2diabetes.pdf", p, width = 5, height = 5)

p <- plot_fit_past(i, add_current_use = T, title = "antipsychotics and type 2 diabetes")
ggsave("results/past_figure_psychotics_type2diabetes.pdf", p, width = 4.5, height = 4)
