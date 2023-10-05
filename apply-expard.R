library(readr)
library(expard)


# read in data processed in 'process-data'
pair <- readr::read_rds("results/processed_data_pen.rds")

# ------------------------------------------------------------------------------
# Apply expard
# ------------------------------------------------------------------------------

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
                              maxiter = 10000)

readr::write_rds(res, "results/fit_penicillin.rds")