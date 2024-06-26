# Process all the data. Results are stored in the results/ folder

library(readr)
library(dplyr)
library(lubridate)
library(Matrix)
#library(ggvenn)
library(expard)
library(stringr)

source("init.R")

# read in the data for the insurants. See script 'process-insurant-data.R'
insurants <- readr::read_rds("data/processed-insurant-data.rds")

# returns a time point, where Q1 2004 is time point 1 and 
# Q4 in 2017 is time point 56
return_time_point <- function(year, quarter) { 
  # earliest year is 2004 
  year <- year - 2004 
  
  return(4*year + quarter)
}

process_data <- function(filename_diag, 
                         filename_pres, 
                         filename_out, 
                         insurants, 
                         krankenkasse = KRANKENKASSEN) {
  
  if (file.exists(filename_out)) { 
    warning("Output file already exists. Data is not processed again")
    return(NULL)
  }
  
  # read in penicillin data
  diag <- readr::read_rds(filename_diag)
  pres <- readr::read_rds(filename_pres)
  
  # filter diagnosis type that end with an h (from Oliver)
  diag <- diag %>% filter(str_sub(diag_type, -1) == "H")
  
  # filter out insurants that are not a member of the Krankenkassen given above
  diag <- diag %>% filter(kk %in% krankenkasse)
  pres <- pres %>% filter(kk %in% krankenkasse)
  
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
  insurants$patient_index <- match(insurants$idnum, all_patients)
  insurants$duration <- insurants$time_end - insurants$time_begin + 1
  
  # which patients were never exposed and never suffered the ADR: 
  ind_never_drug_and_ADR <- which(is.na(insurants$patient_index))
  zero_patients <- nrow(insurants) - n_patients
  zero_timepoints <- sum(insurants$duration[ind_never_drug_and_ADR])
  
  # fill in the drug exposure matrix
  indices <- cbind(pres$patient_index, pres$time)
  drug_exposures[indices] <- 1
  
  # fill in the ADR history matrix
  indices <- cbind(diag$patient_index, diag$time)
  adr_history[indices] <- 1
  
  # fill in the NAs for time points for which the patients were not insured -----
  
  # filter out any patient whose patient id does not occur in the other dataset
  insurants <- insurants %>% filter(!is.na(patient_index))
  
  # filter out all patients that were insured the whole time
  insurants <- insurants %>% filter(time_begin > 1 | time_end < n_timepoints)

  # go over all patients and create a vector that keeps track of where there 
  # should be an NA and where not
  for (i in 1:length(insurants)) { 
    
    # initialize vector 
    NAs <- rep(FALSE, n_timepoints)   
    
    # obtain the index of the patient 
    k <- as.integer(insurants[i, 'patient_index'])
    
    # get the beginning and end point of the insurance period of this patient
    time_begin <- as.integer(insurants[i, 'time_begin'])
    time_end <- as.integer(insurants[i, 'time_end'])
    
    # add the NAs in the beginning if needed
    if (time_begin > 1) { 
      NAs[1:(time_begin-1)] <- TRUE
    }
    
    # add the NAs in the end if needed
    if (time_end < n_timepoints) { 
      NAs[(time_end+1):n_timepoints] <- TRUE  
    }
    
    # update the drug and ADR history of the patient accordingly
    drug_exposures[k, NAs] <- NA
    adr_history[k, NAs] <- NA
  }
  
  pair <- list(drug_history = drug_exposures, adr_history = adr_history,
               zero_patients = zero_patients,
               zero_timepoints = zero_timepoints)
  
  readr::write_rds(x = pair, filename_out)
  
  return(pair)
}

################################################
# Going over all drug-ADR pairs we consider
################################################


# 1. penicillin + anaph. shock ---------------------------
cat("Processing 'penicillin + anaph. shock' (1 / 11)...\n")
process_data(
  filename_diag = "data/diag_anaphylaxis_hosp.rds",
  filename_pres = "data/pres_pen.rds",
  filename_out = "results/data_penicillin_shock.rds", 
  insurants = insurants
)
cat("DONE Processing 'penicillin + anaph. shock' (1 / 11)\n")

# 2. DOACs + Bleeding ------------------------------------
cat("Processing 'DOACs + bleeding' (2 / 11)...\n")
process_data(
  filename_diag = "data/diag_bleeding.rds",
  filename_pres = "data/pres_doac.rds",
  filename_out = "results/data_doacs_bleeding.rds", 
  insurants = insurants
)
cat("DONE Processing 'DOACs + bleeding' (2 / 11)\n")

# 3. NSAIDS and heart failure --------------------------
cat("Processing 'NSAIDs + Heart Failure ' (3 / 11)...\n")
process_data(
  filename_diag = "data/diag_hf.rds",
  filename_pres = "data/pres_nsaid.rds",
  filename_out = "results/data_nsaids_hf.rds", 
  insurants = insurants
)
cat("DONE Processing 'NSAIDs + Heart Failure' (3 / 11)\n")

# 4. oral contraceptives + anaphylactic shock (negative control) --------
cat("Processing 'oral contraceptives + anaphylactic shock' (4 / 11)...\n")
process_data(
  filename_diag = "data/diag_anaphylaxis_hosp.rds",
  filename_pres = "data/pres_oc.rds",
  filename_out = "results/data_oc_shock.rds", 
  insurants = insurants
)
cat("DONE Processing 'oral contraceptives + anaphylactic shock' (4 / 11)\n")

# 5. Antibiotics + bleeding (negative control) ------------------------
cat("Processing 'Antibiotics + bleeding' (5 / 11)...\n")
process_data(
  filename_diag = "data/diag_bleeding.rds",
  filename_pres = "data/pres_antibiotics.rds",
  filename_out = "results/data_antibiotics_bleeding.rds", 
  insurants = insurants
)
cat("DONE Processing 'Antibiotics + bleeding' (5 / 11)\n")

# 6. Antithyroid drug (ATC) + bleeding (negative control) --------------
cat("Processing 'Antithyroid drug (ATC) + bleeding' (6 / 11)...\n")
process_data(
  filename_diag = "data/diag_bleeding.rds",
  filename_pres = "data/pres_atd.rds",
  filename_out = "results/data_atd_bleeding.rds", 
  insurants = insurants
)
cat("DONE Processing 'Antithyroid drug (ATC) + bleeding' (6 / 11)\n")

# 3. Antidiabetics + Bleeding ------------------------------
cat("Processing 'Antidiabetics + bleeding' (7 / 11)...\n")
process_data(
  filename_diag = "data/diag_bleeding.rds",
  filename_pres = "data/pres_diab.rds",
  filename_out = "results/data_diabetics_bleeding.rds",
  insurants = insurants
)
cat("DONE Processing 'Antidiabetics + bleeding' (7 / 11)\n")


# 4. All Psychotics + Type 2 Diabetes
cat("Processing 'All antipsychotics + Type II diabetes' (8 / 11)...\n") # ---
process_data(
  filename_diag = "data/diag_t2d_hosp.rds",
  filename_pres = "data/pres_psych.rds",
  filename_out = "results/data_psychotics_type2diabetes.rds",
  insurants = insurants
)
cat("DONE Processing 'All antipsychotics + Type II diabetes' (8 / 11)\n")

# 5. Psychotics CLO + Type 2 Diabetes
cat("Processing 'Antipsychotic CLO + Type II diabetes' (9 / 11)...\n")
process_data(
  filename_diag = "data/diag_t2d_hosp.rds",
  filename_pres = "data/pres_psych_clo.rds",
  filename_out = "results/data_psychotics_clo_type2diabetes.rds",
  insurants = insurants
)
cat("DONE Processing 'Antipsychotic CLO + Type II diabetes' (9 / 11)\n")

# 6. Psychotics OLA + Type 2 Diabetes
cat("Processing 'Antipsychotic OLA + Type II diabetes' (10 / 11)...\n")
process_data(
  filename_diag = "data/diag_t2d_hosp.rds",
  filename_pres = "data/pres_psych_ola.rds",
  filename_out = "results/data_psychotics_ola_type2diabetes.rds",
  insurants = insurants
)
cat("DONE Processing 'Antipsychotic OLA + Type II diabetes' (10 / 11)\n")

# 7. Psychotics QUE + Type 2 Diabetes
cat("Processing 'Antipsychotic QUE + Type II diabetes' (1 / 11)...\n")
process_data(
  filename_diag = "data/diag_t2d_hosp.rds",
  filename_pres = "data/pres_psych_que.rds",
  filename_out = "results/data_psychotics_que_type2diabetes.rds",
  insurants = insurants
)

cat("DONE PROCESSING DATA\n")

# Resulting filenames: 
# "results/data_penicillin_shock.rds"
# "results/data_doacs_bleeding.rds"
# "results/data_diabetics_bleeding.rds"
# "results/data_psychotics_type2diabetes.rds"
# "results/data_psychotics_clo_type2diabetes.rds"
# "results/data_psychotics_ola_type2diabetes.rds"
# "results/data_psychotics_que_type2diabetes.rds"
