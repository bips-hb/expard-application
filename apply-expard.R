library(readr)
library(expard)
library(stringr)

source("init.R")

# all the result files
data_filenames <-
  c(
    "results/data_penicillin_shock.rds",
    "results/data_doacs_bleeding.rds",
    "results/data_diabetics_bleeding.rds",
    "results/data_psychotics_type2diabetes.rds",
    "results/data_psychotics_clo_type2diabetes.rds",
    "results/data_psychotics_ola_type2diabetes.rds",
    "results/data_psychotics_que_type2diabetes.rds"
  )

# output_filenames <- str_replace(data_filenames, "data_", "fit_")

# go over all files 
res <- lapply(data_filenames, function(filename) {
  cat(sprintf("Processing file %s\n", filename))
  output_filename <- str_replace(filename, "data_", "fit_")
  if (file.exists(output_filename)) { 
    warning(sprintf("%s already exists\n", output_filename))
    return(readr::read_rds(output_filename))
  } 
  
  # apply expard 
  cat(sprintf("Applying expard to %s...\n", filename))
  
  # read in data processed in 'process-data'
  pair <- readr::read_rds(filename)
  
  models <- c(
    'no-association',
    'current-use',
    'past-use',
    'withdrawal',
    'delayed',
    'decaying',
    'delayed+decaying',
    'long-term'
  )
  
  res <- expard::fit_all_models(pair = pair,
                                models = models, 
                                maxiter = 10000, 
                                mc.cores = MC.CORES)
  
  readr::write_rds(res, output_filename)
})
