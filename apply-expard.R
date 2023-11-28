library(readr)
library(expard)
library(stringr)

source("init.R")

# all the result files
data_filenames <-
  c("results/data_penicillin_shock.rds", 
    "results/data_doacs_bleeding.rds", 
    "results/data_nsaids_hf.rds", 
    "results/data_oc_shock.rds", 
    "results/data_antibiotics_bleeding.rds",  
    "results/data_atd_bleeding.rds",
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
  data <- readr::read_rds(filename)
  
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
  
  res <- expard::fit_all_models(pair = list(drug_history = data$drug_history,
                                            adr_history = data$adr_history),
                                models = models, 
                                zero_patients = data$zero_patients,
                                zero_timepoints = data$zero_timepoints,
                                maxiter = 10000, 
                                mc.cores = MC.CORES)
  
  readr::write_rds(res, output_filename)
})
