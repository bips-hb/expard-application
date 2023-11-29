library(expard)
library(readr)
library(ggplot2)
library(ggh4x)
library(ggbreak)

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


# a simple function to help make the segments
add_separators <- function(x, y = 0, angle = 45, length = .1){
  add_y <-  length * sin(angle * pi/180)
  add_x <- length * cos(angle * pi/180)
  ## making the list for your segments
  myseg <- list(x = x - add_x, xend = x + add_x, 
                y = rep(y - add_y, length(x)), yend = rep(y + add_y, length(x)))
  ## this function returns an annotate layer with your segment coordinates
  annotate("segment", 
           x = myseg$x, xend = myseg$xend,
           y = myseg$y, yend = myseg$yend) 
}


# you will need to set limits for correct positioning of your separators
# I chose 0.05 because this is the expand factor by default 
x_sep <- .5

plot_fit_with_breakline <- function(i, x_label = "model", 
                                    title = "", 
                                    y_range = NULL, 
                                    past_values = NULL) {
  
  # see https://stackoverflow.com/questions/69534248/how-can-i-make-a-discontinuous-axis-in-r-with-ggplot2
  # for how to create breaklines
  
  fit <- get_fit(i)
  
  # get the best BIC fit for each model
  best_fit <- fit %>% group_by(model) %>% 
    filter(BIC == min(BIC))  %>% 
    arrange(past) %>% 
    filter(row_number() == 1) %>% 
    arrange(BIC)
  
  # get the overall minimum and maximum BIC value
  min_BIC <- min(best_fit$BIC)
  max_BIC <- max(best_fit$BIC)
  
  if (is.null(y_range)) {
    y_range <- c(min_BIC,max_BIC)
  }
  
  y1end <- 5500
  y2start <- 6500
  
  # plot just the best fit
  p <- ggplot(best_fit) +
    geom_bar(aes(x = reorder(model, BIC), y = BIC), stat="identity") +
    coord_cartesian(ylim=y_range) + 
    ggtitle(title) + 
    #scale_y_continuous(expand = expansion(mult = c(0.1, .1))) + #expand = c(0, 100)) + 
    theme_bw() +
    theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) +
    xlab(x_label) 
  
  p + scale_y_cut(breaks=c(6500)) + theme_bw() + 
    theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) +
    xlab(x_label) + 
    coord_cartesian(ylim=y_range) #, which=c(1,2,3), scales=c(1,.2,0)) #+ 
    #coord_cartesian(ylim=y_range)
  
  
  p +
    guides(y = guide_axis_truncated(
      trunc_lower = c(-Inf, y2start),
      trunc_upper = c(y1end, Inf)
    )) +
    add_separators(y = c(y1end, y2start), x = x_sep, angle = 30)# +
    # you need to set expand to 0
    #scale_y_continuous(expand = c(0,0)) +
    ## to make the angle look like specified, you would need to use coord_equal()
    #coord_cartesian(clip = "off", ylim = c(y_sep, NA)) 
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

i <- 8

plot_fit_local(i, exclude_null = FALSE)
plot_fit_local(i, exclude_null = TRUE)
plot_fit_past(i, add_current_use = T, past_range = c(0,20)) #, past_range = c(1,10))


# we use: "results/fit_penicillin_shock.rds"
i = 1
plot_fit_local(i, exclude_null = FALSE)
plot_fit_local(i, exclude_null = TRUE)
# plot_fit_past(i, add_current_use = T)

fit <- get_fit(i)
r = fit %>% filter(model == 'no-association')
r$BIC
p <- expard::plot_fit(fit = get_fit(i), 
                 title = "penicillin and anaphylaxis", 
                 y_range = c(5150, 5230)
)

ggsave("results/figure_penicillin_shock.pdf", p, width = 5, height = 5)

# "results/fit_doacs_bleeding.rds"
i = 2
plot_fit_local(i, exclude_null = FALSE)
plot_fit_local(i, exclude_null = TRUE)
plot_fit_past(i, add_current_use = T, title = "DOACs and bleeding")


fit <- get_fit(i)
min(fit$BIC)
r = fit %>% filter(model == 'no-association')
r$BIC
p <- expard::plot_fit(fit = get_fit(i), 
                      title = "DOACs and bleeding", 
                      y_range = c(224500, 226000)
)

ggsave("results/figure_doacs_bleeding.pdf", p, width = 5, height = 5)

p <- plot_fit_past(i, add_current_use = T, title = "DOACs and bleeding")

ggsave("results/past_figure_doacs_bleeding.pdf", p, width = 4.5, height = 4)


# "results/fit_antibiotics_bleeding.rds"
i = 5
plot_fit_local(i, exclude_null = FALSE) # note that no association is more likely
# plot_fit_local(i, exclude_null = TRUE)
plot_fit_past(i, add_current_use = T) # long term effect? just time

p <- expard::plot_fit(fit = get_fit(i), 
                      title = "antibiotics and bleeding"
)

ggsave("results/figure_antibiotics_bleeding.pdf", p, width = 5, height = 5)

p <- plot_fit_past(i, add_current_use = T, title = "antibiotics and bleeding")

ggsave("results/past_figure_antibiotics_bleeding.pdf", p, width = 4.5, height = 4)


# "results/fit_psychotics_type2diabetes.rds"
i = 8
plot_fit_local(i, exclude_null = FALSE) # note that no association is more likely
plot_fit_local(i, exclude_null = TRUE)


fit <- get_fit(i)
r = fit %>% filter(model == 'no-association')
r$BIC

p <- expard::plot_fit(get_fit(i), y_range = c(214000, 216300),
                      title = "antipsychotics and type 2 diabetes")
ggsave("results/figure_psychotics_type2diabetes.pdf", p, width = 5, height = 5)

p <- plot_fit_past(i, add_current_use = T, title = "antipsychotics and type 2 diabetes")
ggsave("results/past_figure_psychotics_type2diabetes.pdf", p, width = 4.5, height = 4)

###############################################

# "results/fit_nsaids_hf.rds"
i = 3
plot_fit_local(i, exclude_null = FALSE) # note that no association is more likely
#plot_fit_local(i, exclude_null = TRUE)
plot_fit_past(i, add_current_use = T) # long term effect? just time


# "results/fit_diabetics_bleeding.rds" 
i = 7
plot_fit_local(i, exclude_null = FALSE) # note that no association is more likely
plot_fit_local(i, exclude_null = TRUE)
plot_fit_past(i, add_current_use = T) # long term effect? just time



expard::plot_risk(drug_history = c(rep(0,4), rep(1,20), rep(0, 27)), 
                  risk_model = expard::risk_model_past(4))


fit_filename = fit_filenames[4]
