# Generate tables for the multiple imputation analysis %%%%%%%%%%%%%%%%%%%%%%%%
# The purpose of this script is to take output from the MI analysis in SAS
# and create tables for the manuscript. 

# This script was specifically designed for the mixed model results and the
# ttest results without any covariates
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# Load libraries ---------------------------------------------------------------
pacman::p_load(REDCapR,
               here,
               Hmisc,
               tidyverse,
               gtsummary,
               bstfun,
               furrr,
               flextable,
               magrittr,
               nlme)

# Mixed Model Multiple imputation ----------------------------------------------
# make a function of a directory
dir_in <- here("data", "multiple_imputation")

# List all files in the directory
file_paths <- list.files(path = dir_in, pattern = ".sas7bdat", full.names = TRUE, all.files = TRUE )

# Make empty data frame
tab <- NULL

# Loop through files
for(i in file_paths){
  # Get the DV 
  # 2 will be the starting point for extracting the DV name
  start = 2
  
  # 12 characters from end to remove the file extenstion for the mupltiple 
  # imputation
  end = nchar(str_remove(i, dir_in)) - 12
  
  dv = substr(str_remove(i, dir_in), start, end)

  # Import data
  assign("mi_results", haven::read_sas(i))

  # Extract the interaction term
  sub_tab <- 
  mi_results %>%
    filter(Parm ==	"randomization*redcap_event_name",
           randomization == "Intervention",
           redcap_event_name == "Post") %>%
    select(Estimate, StdErr, Probt) %>%
    mutate(DV = dv) %>%
    select(DV, everything())
  
  # Append sub_tab to tab
  tab <- bind_rows(tab, sub_tab)
}


# Format tab
tab %<>%
  mutate(Estimate = round(Estimate, 2),
         StdErr = round(StdErr, 2),
         `Difference in change, Intervention vs. Control (SE)` = str_c(Estimate, " (", StdErr, ")"),
         `P-value` = round(Probt, 2)) %>%
  select(-Estimate, -StdErr, -Probt)




# Save tab to .csv
write_csv(tab, na = "", file = here("tables", "multiple_imputation", "mixed_mod_results_table.csv"))


# T-Test Multiple imputation ---------------------------------------------------
# make a function of a directory
dir_in <- here("data", "multiple_imputation_ttest")

# List all files in the directory
file_paths <- list.files(path = dir_in, pattern = ".sas7bdat", full.names = TRUE, all.files = TRUE )

# Make empty data frame
tab <- NULL

# Loop through files
for(i in file_paths){
  # Get the DV 
  # 2 will be the starting point for extracting the DV name
  start = 2
  
  # 12 characters from end to remove the file extenstion for the mupltiple 
  # imputation
  end = nchar(str_remove(i, dir_in)) - 18
  
  dv = substr(str_remove(i, dir_in), start, end)

  # Import data
  assign("mi_results", haven::read_sas(i))

  # Extract the interaction term
  sub_tab <- 
  mi_results %>%
    filter(Parm ==	"randomizationIntervention") %>%
    select(Estimate, StdErr, Probt) %>%
    mutate(DV = dv) %>%
    select(DV, everything())
  
  # Append sub_tab to tab
  tab <- bind_rows(tab, sub_tab)
}


# Format tab
tab %<>%
  mutate(Estimate = round(Estimate, 2),
         StdErr = round(StdErr, 2),
         `Difference in change, Intervention vs. Control (SE)` = str_c(Estimate, " (", StdErr, ")"),
         `P-value` = round(Probt, 2)) %>%
  select(-Estimate, -StdErr, -Probt)




# Save tab to .csv
write_csv(tab, na = "", file = here("tables", "multiple_imputation_ttest", "ttest_results_table.csv"))
