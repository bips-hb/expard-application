library(readr)
library(dplyr)
library(lubridate)
library(Matrix)
#library(ggvenn)
library(expard)
library(stringr)

# returns a time point, where Q1 2004 is time point 1 and 
# Q4 in 2017 is time point 56
return_time_point <- function(year, quarter) { 
  # earliest year is 2004 
  year <- year - 2004 
  
  return(4*year + quarter)
}

process_data <- function(filename_diag, filename_pres, filename_out) {
  
  # read in penicillin data
  diag <- readr::read_rds(filename_diag)
  pres <- readr::read_rds(filename_pres)
  
  # filter diagnosis type that end with an h (from Oliver)
  diag <- diag %>% filter(str_sub(diag_type, -1) == "H")
  
  # ------------------------------------------------------------------------------
  # Determine quarter and year
  # ------------------------------------------------------------------------------
  
  # penicillin + shock
  diag$quarter <- lubridate::quarter(diag$hosp_date)
  diag$year    <- lubridate::year(diag$hosp_date)
  
  pres$quarter <- lubridate::quarter(pres$del_dat)
  pres$year    <- lubridate::year(pres$del_dat)
  
  #'filter out year before 2003 (sometimes happens with prescription. 
  #'For penicillin, this just happens once)
  pres <- pres %>% filter(year >= 2004) 
  diag <- diag %>% filter(year >= 2004)
  
  # add the time points to the data frames
  diag <- diag %>% mutate(time = return_time_point(year, quarter))
  pres <- pres %>% mutate(time = return_time_point(year, quarter))
  
  # ------------------------------------------------------------------------------
  # Create raw data frames
  # ------------------------------------------------------------------------------
  
  # determine unique number of patients 
  patient_ids_diag <- diag$idnum %>% unique()
  patient_ids_pres <- pres$idnum %>% unique()
  
  # create a Venn Diagram 
  # ggvenn(
  #   list(diag = patient_ids_diag, pres = patient_ids_pres), 
  #   fill_color = c("#0073C2FF", "#EFC000FF"),
  #   stroke_size = 0.5, set_name_size = 4
  # )
  
  # get the union and determine the total number of unique patients
  all_patients <- sort(union(patient_ids_diag, patient_ids_pres))
  n_patients <- length(all_patients)
  
  # total number of time points available
  n_timepoints <- (2017 - 2004 + 1) * 4
  
  # initialize the matrices
  drug_exposures <- matrix(0, nrow = n_patients, ncol = n_timepoints)
  adr_history    <- matrix(0, nrow = n_patients, ncol = n_timepoints)
  
  # associate each patient with an index -----------------------------------------
  
  # returns the unique index associated with a specific idnum
  diag$patient_index <- match(diag$idnum, all_patients) 
  pres$patient_index <- match(pres$idnum, all_patients)
  
  # fill in the drug exposure matrix
  indices <- cbind(pres$patient_index, pres$time)
  drug_exposures[indices] <- 1
  
  # fill in the ADR history matrix
  indices <- cbind(diag$patient_index, diag$time)
  adr_history[indices] <- 1
  
  pair <- list(drug_history = drug_exposures, adr_history = adr_history)
  
  readr::write_rds(x = pair, filename_out)
  
  return(pair)
}

################################################
# Going over all drug-ADR pairs we consider
################################################

# 1. penicillin + anaph. shock 
process_data(
  filename_diag = "data/diag_pen_all.rds",
  filename_pres = "data/pres_pen.rds",
  filename_out = "results/data_penicillin_shock.rds"
)

# 2. DOACs + Bleeding
process_data(
  filename_diag = "data/diag_bleeding.rds",
  filename_pres = "data/pres_doac.rds",
  filename_out = "results/data_doacs_bleeding.rds"
)

# 3. Antidiabetics + Bleeding
process_data(
  filename_diag = "data/diag_bleeding.rds",
  filename_pres = "data/pres_diab.rds",
  filename_out = "results/data_diabetics_bleeding.rds"
)

# 4. All Psychotics + Type 2 Diabetes
process_data(
  filename_diag = "data/diag_t2d_hosp.rds",
  filename_pres = "data/pres_psych.rds",
  filename_out = "results/data_psychotics_type2diabetes.rds"
)

# 5. Psychotics CLO + Type 2 Diabetes
process_data(
  filename_diag = "data/diag_t2d_hosp.rds",
  filename_pres = "data/pres_psych_clo.rds",
  filename_out = "results/data_psychotics_clo_type2diabetes.rds"
)

# 6. Psychotics OLA + Type 2 Diabetes
process_data(
  filename_diag = "data/diag_t2d_hosp.rds",
  filename_pres = "data/pres_psych_ola.rds",
  filename_out = "results/data_psychotics_ola_type2diabetes.rds"
)

# 7. Psychotics QUE + Type 2 Diabetes
process_data(
  filename_diag = "data/diag_t2d_hosp.rds",
  filename_pres = "data/pres_psych_que.rds",
  filename_out = "results/data_psychotics_que_type2diabetes.rds"
)

# Resulting filenames: 
# "results/data_penicillin_shock.rds"
# "results/data_doacs_bleeding.rds"
# "results/data_diabetics_bleeding.rds"
# "results/data_psychotics_type2diabetes.rds"
# "results/data_psychotics_clo_type2diabetes.rds"
# "results/data_psychotics_ola_type2diabetes.rds"
# "results/data_psychotics_que_type2diabetes.rds"
