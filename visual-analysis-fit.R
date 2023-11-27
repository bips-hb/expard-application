library(expard)
library(readr)
library(stringr)

fit_filenames <-
  c(
    "results/fit_penicillin_shock.rds",
    "results/fit_doacs_bleeding.rds",
    "results/fit_nsaids_hf.rds",
    "results/fit_oc_shock.rds",
    "results/fit_antibiotics_bleeding.rds",
    "results/fit_atd_bleeding.rds"
  )

figure_filenames <- str_replace(fit_filenames, "fit_", "figure_")
figure_filenames <- str_replace(figure_filenames, ".rds", ".pdf")

titles <- c(
  "penicillin and anaphylaxis",
  "DOACs and bleeding",
  "NSAIDs and heart failure",
  "oral contraceptives and anaphylaxis",
  "antibiotics and bleeding",
  "ATDs and bleeding"
)

n_files <- length(fit_filenames)

for (i in 1:n_files) {
  input <- readr::read_rds(fit_filenames[i])
  output <- figure_filenames[i]
  title <- titles[i]
  
  p <- expard::plot_fit(fit = input, title = title)
  
  ggsave(output, p, width = 5, height = 5)
}

#data <- readr::read_rds(filename)

#expard::plot_fit(fit = data, title = "Penicillin and Anaphylaxis")