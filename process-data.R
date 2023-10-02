library(readr)
library(dplyr)
library(lubridate)
library(Matrix)
library(ggvenn)
library(expard)
library(stringr)

# read in penicillin data
diag_pen <- readr::read_rds("penicillin/diag_pen.rds")
pres_pen <- readr::read_rds("penicillin/pres_pen.rds")

# filter diagnosis type that end with an h (from Oliver)
diag_pen <- diag_pen %>% filter(str_sub(diag_type, -1) == "H")

# ------------------------------------------------------------------------------
# Determine quarter and year
# ------------------------------------------------------------------------------

# penicillin + shock
diag_pen$quarter <- lubridate::quarter(diag_pen$hosp_date)
diag_pen$year    <- lubridate::year(diag_pen$hosp_date)

#'filter out year before 2003 (sometimes happens with prescription. 
#'For penicillin, this just happens once)

pres_pen$quarter <- lubridate::quarter(pres_pen$del_dat)
pres_pen$year    <- lubridate::year(pres_pen$del_dat)

pres_pen <- pres_pen %>% filter(year >= 2004) 

# returns a time point, where Q1 2004 is time point 1 and 
# Q4 in 2017 is time point 56
return_time_point <- function(year, quarter) { 
  # earliest year is 2004 
  year <- year - 2004 

  return(4*year + quarter)
}

# add the time points to the data frames
diag_pen <- diag_pen %>% mutate(time = return_time_point(year, quarter))
pres_pen <- pres_pen %>% mutate(time = return_time_point(year, quarter))

# ------------------------------------------------------------------------------
# Create raw data frames
# ------------------------------------------------------------------------------

# determine unique number of patients 
patient_ids_diag <- diag_pen$idnum %>% unique()
patient_ids_pres <- pres_pen$idnum %>% unique()

# create a Venn Diagram 
ggvenn(
  list(diag = patient_ids_diag, pres = patient_ids_pres), 
  fill_color = c("#0073C2FF", "#EFC000FF"),
  stroke_size = 0.5, set_name_size = 4
)

# get the union and determine the total number of unique patients
all_patients <- sort(union(patient_ids_diag, patient_ids_pres))
n_patients <- length(all_patients)

# total number of time points available
n_timepoints <- (2017 - 2004 + 1) * 4

# initialize the matrices
drug_exposures <- matrix(0, nrow = n_patients, ncol = n_timepoints)
adr_history    <- matrix(0, nrow = n_patients, ncol = n_timepoints)

# associate each patient with an index 
which(all_patients == 100004363)

# returns the unique index associated with a specific idnum
return_patient_index <- function(idnums, all_patients) {
  sapply(idnums, function(idnum) {
  which(all_patients == idnum)
})}

# add the indices of the patients
diag_pen$patient_index <- return_patient_index(diag_pen$idnum, all_patients) #%>% mutate(patient_index = which(all_patients == idnum))
pres_pen$patient_index <- return_patient_index(pres_pen$idnum, all_patients) 

# fill in the drug exposure matrix
indices <- cbind(pres_pen$patient_index, pres_pen$time)
drug_exposures[indices] <- 1

# fill in the ADR history matrix
indices <- cbind(diag_pen$patient_index, diag_pen$time)
adr_history[indices] <- 1

pair <- list(drug_history = drug_exposures, adr_history = adr_history)

#readr::write_rds(x = pair, 
#                 "processed-data-penicillin.rds")

t2x2 <- expard::create2x2table(pair)

library(pvm)

pvm::ROR(t2x2$a, t2x2$b, t2x2$c, t2x2$d)
pvm::fisherExactTest(t2x2$a, t2x2$b, t2x2$c, t2x2$d)

# ------------------------------------------------------------------------------
# Apply expard
# ------------------------------------------------------------------------------
#' res <- expard::fit_all_models(pair = list(drug_history = drug_exposures, adr_history = adr_history), 
#'                               models = c(
#'                                 'no-association',
#'                                 'current-use', 
#'                                 'past-use'#, 
#'                                 #'decaying'
#'                               ), 
#'                               maxiter = 20)
#' 
#' 
#' models = c(
#'   'no-association',
#'   'current-use',
#'   'past-use',
#'   'withdrawal',
#'   'delayed',
#'   'decaying',
#'   'delayed+decaying',
#'   'long-term'
#' )
#' 
#' 
#' #drug_exposures[pres_pen$patient_index, pres_pen$time] <- 1
#' #drug_exposures[c(1,2),c(1,2)] <- 1
#' 
#' #return_time_point(2005, 4)
#' 
#' #temp <- data.frame(expand.grid(
#' #  quarter = 1:4,
#' #  year = 2004:2017 
#' #))
#' 
#' #temp <- temp %>% mutate(time_point = return_time_point(year, quarter))
