# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Carlos Rodriguez, PhD. Dept. of Family Medicine, CU Anschutz Medical Campus
# February 06, 2023

# Updated by Kaitlyn Bertin, ACCORDS
# April 2023

# Better Together

# Purpose: This script is intended to download Redcap data for the better 
# together project, prepare and process the data, and finally save it to an R
# data file that can be loaded for generating reports and conducting analyses.

# Dependencies:
# get_redcap_data() function and credentials files. See documentation for 
# get_redcap_data().

# apply_redcap_processing.R script to apply labels, create factored columns,
# and set levels.
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# Load libraries ---------------------------------------------------------------
pacman::p_load(REDCapR,
               here,
               Hmisc,
               tidyverse,
               magrittr)


# Load project data ------------------------------------------------------------
# Load the get_redcap_data() function
source(here("scripts", "functions", "get_redcap_data.R")) 

# Set the project ID to the better together project
project_id <- 26979

# Download/refresh data
data <- get_redcap_data(project_id)

# Process the randomization rows  ----------------------------------------------
# Each participant is associated with up to 3 distinct events in the redcap data
# 1) randomization, 2) pre-, and 3) post-intervention. Randomization events will
# not contain any data for the pre or post outcomes and need to be removed. The
# randomization event contains the values that indicate which treatment the 
# participant was randomized to.

# Filter the unique randomization event variables to a separate data frame
randomization_arm_1 <- 
  data %>%
  filter(redcap_event_name == "randomization_arm_1") %>%
  select(identifier, randomization, randomization_complete)

# Remove randomization rows and columns from the main redcap data
data %<>%
  filter(redcap_event_name != "randomization_arm_1") %>%
  select(-randomization, -randomization_complete)

# Join in randomization (treatment) and randomization complete values so that
# missing data from the instruments in the randomization event is cleared out
data %<>%
  left_join(., 
            select(randomization_arm_1, identifier, randomization, randomization_complete), 
            by = "identifier")

# Calculate instrument scores --------------------------------------------------
# Note: option "na.rm=TRUE" allows missing items but excludes them from the mean. 
# If we don't want to allow missing items (set subscale score to NA if any 
# item is NA), then use na.rm=FALSE

## Maslach Burnout Inventory (MBI) ---------------------------------------------
# Emotional Exhaustion (EE) subscale: items 1, 2, 3, 6, 8, 13, 14, 16, 20
mbi_ee <- c(1:3, 6, 8, 13, 14, 16, 20)

# Depersonalization (DP) subscale: items 5, 10, 11, 15, 22
mbi_dp <- c(5, 10, 11, 15, 22)

# Personal Accomplishment (PA) subscale: items 4, 7, 9, 12, 17, 18, 19, 21
mbi_pa <- c(4, 7, 9, 12, 17:19, 21)

# If an item is missing, a total score will not be calculated
# Calculate the number of missing items in each subscale and create a binary
# variable to indicate if any single item was missing for filtering data 
data %<>%
  mutate(
    mbi_ee_sum = rowSums(across(num_range("mbi", mbi_ee)), na.rm = F), # na.rm=T so sum score not calculated if any item is missing
    mbi_ee_n_na_items = rowSums(is.na(across(num_range("mbi", mbi_ee)))), # count how many items are missing (out of 9) for EE
    mbi_ee_bin_na_items = ifelse(mbi_ee_n_na_items,1,0),
    
    mbi_dp_sum = rowSums(across(num_range("mbi", mbi_dp)), na.rm = F),
    mbi_dp_n_na_items = rowSums(is.na(across(num_range("mbi", mbi_dp)))), # count how many items are missing (out of 5) for DP
    mbi_dp_bin_na_items = ifelse(mbi_dp_n_na_items,1,0),

    mbi_pa_sum = rowSums(across(num_range("mbi", mbi_pa)), na.rm = F),
    mbi_pa_n_na_items = rowSums(is.na(across(num_range("mbi", mbi_pa)))), # count how many items are missing (out of 8) for PA
    mbi_pa_bin_na_items = ifelse(mbi_pa_n_na_items,1,0)) 

# Create a binary variable for burnout mbi_burned_out
# Check that the created variable will have the same number NAs as the original
# sum variable
data %>%
  mutate(mbi_bin = ifelse(mbi_ee_sum >= 27, 1, 0)) %>%
  filter(is.na(mbi_bin)) %>%
  nrow()

data %>%
  filter(is.na(mbi_ee_sum)) %>%
  nrow()

data %<>%
  mutate(mbi_bin = ifelse(mbi_ee_sum >= 27, 1, 0))


# Rearrange Columns so that all MBI related variables are together
# Removes the redcap calculated scores that were based on means instead of sums
data %<>%
  select(identifier:mbi22,
         mbi_ee_sum:mbi_pa_bin_na_items,
         everything())

# Removes the redcap calculated mbi_means, since they are scored as means 
# instead sum and prior work was done with sum scores
data %<>%
  select(-(mbieemean:mbipamean))

## Self-Compassion Scale - Short form (SCS-SF) ---------------------------------
# https://self-compassion.org/wp-content/uploads/2021/03/SCS-SF-information.pdf
# Create new item variables with reverse scoring for items 1, 4, 8, 9, 11, 12
# Reverse score the scs items
data %<>%
  mutate(
    scs1 = 6 - scs1,
    scs4 = 6 - scs4,
    scs8 = 6 - scs8,
    scs9 = 6 - scs9,
    scs11 = 6 - scs11,
    scs12 = 6 - scs12)

# Score the scssf_tot as sums to match Fainstad, 2022 and pilot data
data %<>%
  mutate(scssf_sum_tot = rowSums(across(scs1:scs12), na.rm = F),
         scssf_n_na_items = rowSums(is.na(across(num_range("scs", c(1:12))))),
         scssf_bin_na_items = ifelse(scssf_n_na_items > 0, 1, 0))

max(data$scssf_sum_tot, na.rm = T)

# Rearrange the columns
data %<>%
  select(identifier:scs12,
         scssf_sum_tot:scssf_bin_na_items,
         everything())

# Remove the redcap calculated scores for the scsf
data %<>% 
  select(-(scsoveridentificationmean:scstotalmean))

# # Check participant 110
# data %>% filter(identifier == 110) %>% select(starts_with("scssf"))


## Moral Injury Symptom Scale – Healthcare Providers (MISS-HP) -----------------
# Create new item variables with reverse scoring for items 5, 6, 7, 10
data %<>%
  mutate(
    miss5 = 11 - miss5,
    miss6 = 11 - miss6,
    miss7 = 11 - miss7,
    miss10 = 11 - miss10)

# Score the misshp_tot as the sum of items
data %<>%
  mutate(
    misshp_tot = rowSums(across(num_range("miss", c(1:10))), na.rm = F), 
    misshp_n_na_items = rowSums(is.na(across(num_range("miss", c(1:10))))), 
    misshp_bin_na_items = ifelse(misshp_n_na_items > 0, 1, 0))

# Rearrange columns and remove redcap calculated variable
data %<>%
  select(identifier:miss10,
         misshp_tot:misshp_bin_na_items,
         everything(),
         -misssum)  

## Young Impostor Scale (YIS) --------------------------------------------------
# Responding “Yes” to 5 or more of these questions considered a positive for
# imposter syndrome
data %<>%
  mutate(
    yis_sum = rowSums(across(yis1:yis8), na.rm=F),
    yis_bin = if_else(yis_sum >= 5, 1, 0),
    yis_n_na_items = rowSums(is.na(across(yis1:yis8))),
    yis_bin_na_items = ifelse(yis_n_na_items > 0, 1, 0),
    
    #countNA_yis8 = rowSums(is.na(across(num_range("yis", c(1:8))))), # count how many items are missing (out of 8) - to be used in count of "yes" responses below
    # yis_count = rowSums(across(num_range("yis", c(1:8))), na.rm=TRUE), # count of "yes" responses - allows missing items
    #yis_count = if_else(countNA_yis8 == 8, NA_real_, rowSums(across(num_range("yis", c(1:8))), na.rm=F)), # count of "yes" responses - allows missing items, unless all items missing
    #yis_ispos = if_else(yis_count >= 5, 1, 0, NA_real_)
  )
  

# Rearrange columns
data %<>%
  select(identifier:yis8,
         yis_sum:yis_bin_na_items,
         everything(),
         -yissum)

    
## Three-item Loneliness Scale (LS) --------------------------------------------
# dichotomize scores (>=6: lonely, 3-5: not lonely)
data %<>%
  mutate(
    ls_sum = rowSums(across(starts_with("ls_")), na.rm=F),
    ls_bin_lonely = if_else(ls_sum > 5, 1, 0), 
    ls_n_na_items = rowSums(is.na(across(ls_lack_companionship:ls_isolated))))

# Rearrange columns
data %<>%
  select(identifier:ls_isolated,
         ls_sum:ls_n_na_items,
         everything(),
         -lssum)


## Flourish Index (FI) and Secure Flourish Index (SFI) -------------------------
data %<>%
  mutate(
    ## Domain-specific indices 
    # D1. Happiness and life satisfaction
    sfi_d1 = rowMeans(across(num_range("sfi", c(1, 2))), na.rm = F),
    
    # D2. Mental and physical health
    sfi_d2 = rowMeans(across(num_range("sfi", c(3, 4))), na.rm = F),
    
    # D3. Meaning and purpose
    sfi_d3 = rowMeans(across(num_range("sfi", c(5, 6))), na.rm = F),
    
    # D4. Character and virtue
    sfi_d4 = rowMeans(across(num_range("sfi", c(7, 8))), na.rm = F),
    
    # D5. Close social relationships
    sfi_d5 = rowMeans(across(num_range("sfi", c(9, 10))), na.rm = F),
    
    # D6. Financial and material stability
    sfi_d6 = rowMeans(across(num_range("sfi", c(11, 12))), na.rm = F),
    
    # FI total score (mean of first 5 domain indices)
    fi_tot = rowMeans(across(num_range("sfi_d", c(1:5))), na.rm = F),
    
    # SFI total score (mean of all 6 domain indices)
    sfi_tot = rowMeans(across(num_range("sfi_d", c(1:6))), na.rm = F), 
    
    sfi_n_na_items = rowSums(is.na(across(num_range("sfi", c(1:12))))), 
    fi_n_na_items = rowSums(is.na(across(num_range("sfi", c(1:10))))),
    sfi_bin_na_items = ifelse(sfi_n_na_items > 0, 1, 0),
    fi_bin_na_items = ifelse(fi_n_na_items > 0, 1, 0))

# Rearrange columns
data %<>%
  select(identifier:sfi12,
         fi_tot:fi_bin_na_items,
         everything(),
         -sfimean)


# Sexual Orientation -----------------------------------------------------------
# Responses to sexual orientation question are contained in several columns.
# The purpose of this code chunk is to create a single column with all of the 
# sexual orientation responses for table generating purposes.

# Create a vector of the column names for sexual orientation responses
orient_vars <- 
  names(data %>% select(sexual_orientation___1:sexual_orientation___8))

# How many participants endorse more than 1?
data %>%
  select(all_of(orient_vars)) %>%
  mutate(total = rowSums(across(everything()))) %>%
  filter(total > 1) %>% 
  nrow()

# Since no participant endorsed more than one value, values across columns can
# be collapsed into one column, sexual_orientation
data %<>%
  mutate(sexual_orientation = ifelse(sexual_orientation___1 == 1, "Asexual", NA),
         sexual_orientation = ifelse(sexual_orientation___2 == 1, "Bisexual", sexual_orientation),
         sexual_orientation = ifelse(sexual_orientation___3 == 1, "Gay or Lesbian", sexual_orientation),
         sexual_orientation = ifelse(sexual_orientation___4 == 1, "Heterosexual", sexual_orientation),
         sexual_orientation = ifelse(sexual_orientation___5 == 1, "Pansexual", sexual_orientation),
         sexual_orientation = ifelse(sexual_orientation___6 == 1, "Queer", sexual_orientation),
         sexual_orientation = ifelse(sexual_orientation___7 == 1, "Prefer to Self-describe", sexual_orientation),
         sexual_orientation = ifelse(sexual_orientation___8 == 1, "Prefer not to answer", sexual_orientation)
         ) %>%
  mutate(sexual_orientation = as_factor(sexual_orientation))

# Check that all remaining values are in fact missing for self describe and 0
# for orient_vars
data %>%
  filter(redcap_event_name == "pre_arm_1", 
         is.na(sexual_orientation)) %>%
  select(all_of(orient_vars), sex_orien_self_describe)

# Remove sexual_orientation___* columns
# data %<>% select(-all_of(orient_vars))

# Race and Ethnicity -----------------------------------------------------------
# Responses to race and ethnicity question are contained in several columns.
# The purpose of this code chunk is to create a single column with all of the 
# race and ethnicity responses for table generating purposes.
# For those with 2 or more race ethnicity responses, create a separate temporary
# variable (gt1_race_ethn) to indicate which participants need to be set to
# that value. Then group AA, Black, 2+, Mid East, Prefer* into one category.

# Create a vector of the column names for race & ethnicity responses
race_vars <- names(data %>% select(race_ethnicity___1:race_ethnicity___9))

# How many participants endorse more than 1?
data %>%
  filter(redcap_event_name == "pre_arm_1") %>%
  select(all_of(race_vars)) %>%
  mutate(total = rowSums(across(everything()))) %>%
  filter(total > 1) %>% 
  nrow()

data %<>%
  mutate(gt1_race_ethn = rowSums(across(race_ethnicity___1:race_ethnicity___9)),
         race_ethnicity = ifelse(race_ethnicity___1 == 1, "American Indian, Alaska Native or First Nations", NA),
         race_ethnicity = ifelse(race_ethnicity___2 == 1, "Asian", race_ethnicity),
         race_ethnicity = ifelse(race_ethnicity___3 == 1, "Black or African American", race_ethnicity),
         race_ethnicity = ifelse(race_ethnicity___4 == 1, "Hispanic or Latinx", race_ethnicity),
         race_ethnicity = ifelse(race_ethnicity___5 == 1, "Native Hawaiian and Pacific Islander", race_ethnicity),
         race_ethnicity = ifelse(race_ethnicity___6 == 1, "Middle Eastern or North African", race_ethnicity),
         race_ethnicity = ifelse(race_ethnicity___7 == 1, "White", race_ethnicity),
         race_ethnicity = ifelse(race_ethnicity___8 == 1, "Prefer to self-describe", race_ethnicity),
         race_ethnicity = ifelse(race_ethnicity___9 == 1, "Prefer not to answer", race_ethnicity),
         race_ethnicity = ifelse(gt1_race_ethn > 1, "2 or more", race_ethnicity),
         race_ethnicity = as_factor(race_ethnicity), 
         race_ethnicity = fct_collapse(race_ethnicity,
                                       Other = c("American Indian, Alaska Native or First Nations",
                                                 "Black or African American",
                                                 "2 or more",
                                                 "Middle Eastern or North African",
                                                 "Prefer to self-describe",
                                                 "Prefer not to answer"))) %>%
  select(-gt1_race_ethn)
    
# Check that all remaining values are in fact missing
data %>%
  filter(redcap_event_name == "pre_arm_1", 
         is.na(race_ethnicity)) %>%
  select(all_of(race_vars), race_ethn_self_describe)


# Add rows for those who did not submit a post intervention survey -------------
# Create a dataset where the folks with only one entry at the post time point 
# will have an added row. Label these rows as "Post" then fill in values of 
# their DVs with NAs

# Check that all who only have 1 time point have the pre event
data %>% 
  filter(identifier %in% (data %>%
                            group_by(identifier) %>%
                            count() %>%
                            filter(n == 1) %>%
                            pull(identifier))) %>%
  select(redcap_event_name)


# Create the post event rows to append to the data set
empty_post_rows <- 
data %>%
  group_by(identifier) %>%
  count() %>%
  filter(n == 1) %>%
  select(identifier) %>%
  mutate(redcap_event_name = "post_arm_1",
         identifier = as.numeric(identifier))

#create a copy of the data up until now to test the data
backup_data <- data

# zaps all labels, even those needed for downstream tables
#data <- bind_rows(haven::zap_label(data), empty_post_rows)
data <- bind_rows(data, empty_post_rows)

# Apply Redcap processing ------------------------------------------------------
# Applies the downloaded R commands to apply labels, create factored columns,
# and set levels. Saved in a separate script to improve readability.
source(here("scripts", "apply_redcap_processing.R")) 

# Set the levels of redcap_event_name.factor -----------------------------------
# Removes the levels of the factor that contain randomization for event
data %<>%
  mutate(redcap_event_name.factor = factor(redcap_event_name.factor))

# Remove identifying information -----------------------------------------------
data %<>% 
  select(-first_name, -last_name, -work_email, -firstname, -enrollment_signature)


# Gender identity --------------------------------------------------------------
# Performed after Redcap processing to use fct_collapse after the column 
# gender_identity.factor is created.
data %<>%
  mutate(gender_identity.factor = fct_collapse(gender_identity.factor,
                                               Other = c("Non-binary/third gender",
                                                         "Trans female",
                                                         "Trans male",
                                                         "Prefer to self-describe",
                                                         "Prefer not to answer")))


# Specialty --------------------------------------------------------------------
# Some participants selected Other and provided a free text response for 
# specialty, Tyra and Carlee reviewed the free text responses and categorized
# them as shown in the .csv file. 
# Internal medicine (IM) will be a category which includes GIM, pall care, 
# geriatrics, heme/onc or “medical oncology”, ID, endo, rheum.

# Load the .csv file,
fr_text_specialties <- read.csv(here("data", "free_text_specialties_10.30.23.csv"))

data <- bind_rows(
  # Filter out data with "Other" and where specialty .factor is missing to get 
  # the post timepoint data too.
  (data %>% 
    filter(specialty.factor != "Other" | is.na(specialty.factor)) %>%
     mutate(identifier = as.numeric(identifier))),
  
  # Merge data for the participants that selected other
  (data %>% 
    filter(specialty.factor == "Other") %>%
    mutate(identifier = as.numeric(identifier)) %>%
    left_join(., fr_text_specialties, by = c("identifier" = "Identifier")) %>%
    mutate(specialty.factor = ifelse(!is.na(specialty.factor), Specialty, specialty.factor)) %>%
    select(-Specialty))
)

# Recode the new values, needs verification
# IM <- General Internal Medicine
# BH <- Behavioral Health
# FM <- Family Medicine
data %<>%
  mutate(specialty.factor = recode(specialty.factor,
                                   "BH" = "Behavioral Health",
                                   "FM" = "Family Medicine",
                                   "IM" = "Internal Medicine",
                                   "Palliative Care" = "Internal Medicine",
                                   "Geriatrics" = "Internal Medicine",
                                   "General Internal Medicine" = "Internal Medicine"))

data %>%
  select(specialty.factor) %>%
  table()
                                     

# Set aside labels for renaming columns
## These demographic variables  will need modification to replace the 
## name with the label
out_demo <- data %>% 
  select(living_situation___0:living_situation___9,
         caregiving___0:caregiving___4)


# Save Data --------------------------------------------------------------------
# ## Save the prepped Rdsat to .csv ----
# data %>%
#   write.csv(., 
#           file = here("data/better_together_prepped_data.csv"),
#           row.names = FALSE)

## Save to .csv for SAS ----


### Convert all NAs to a dot (".") for SAS ----
out <- data %>%
  select(identifier, 
         randomization.factor, 
         redcap_event_name.factor,
         age,
         gender_identity.factor,
         race_ethnicity,
         sexual_orientation,
         department_randomization.factor,
         specialty.factor,
         bhc.factor,
         degree.factor,
         years_since_training,
         years_at_clinic,
         
         mbi_ee_sum,
         mbi_ee_bin_na_items,
         mbi_dp_sum,
         mbi_dp_bin_na_items,
         mbi_pa_sum,
         mbi_pa_bin_na_items,
         mbi_bin,
         
         misshp_tot,
         misshp_bin_na_items,
         yis_sum,
         yis_bin,
         yis_bin_na_items,
         ls_sum,
         ls_bin_lonely,
         
         scssf_sum_tot,
         scssf_bin_na_items,
         
         fi_tot,
         sfi_tot) %>%
  arrange(identifier, redcap_event_name.factor) %>%
  mutate(specialty.factor = factor(specialty.factor)) %>%
  mutate_if(is.numeric, ~ifelse(is.na(.), ".", .))%>%
  mutate_if(is.factor, ~fct_explicit_na(., na_level = "."))

### Remove the ".factor" suffix from column names ----
# Pull variable names that need the ".factor" removed since it causes trouble 
# when referencing columns in SAS
out_names <- names(out)

# Create a new vector with modified variable names to remove the ".factor" suffix
out_names <- sub(".factor", "", out_names)

# Replace current column names with those that have the ".factor" removed
colnames(out) <- out_names

### Fill in missing values ----
# Since some demographic information is only captured at the first "pre" time
# point, the same information needs to be filled in to the "post" time point
# row to avoid SAS analysis errors related to missing values.
source(here("scripts", "functions", "fill_values.R")) 

# Set the columns that have values to fill in
cols_to_fill <- c("age",
                  "gender_identity",
                  "race_ethnicity",
                  "sexual_orientation",
                  "department_randomization",
                  "specialty",
                  "bhc",
                  "degree",
                  "years_since_training",
                  "years_at_clinic",
                  "randomization"
                  )

# Modify the columns in a for-loop
for (i in 1:length(cols_to_fill)){
  out <<- fill_values(out, cols_to_fill[i])
}

### Arrange by identifier ----
# out %>%
#   arrange(identifier, redcap_event_name)

### Create a binary variable to indicate whether or not they completed the 
# follow up
out %<>%
  mutate(pre_post_complete = ifelse(identifier %in% empty_post_rows$identifier, 0, 1))

### Write out data set----
out %>%
  write.csv(., 
          file = here("data/better_together_sas_data.csv"),
          row.names = FALSE)



# ## Save to Rdata ----
data <- 
  left_join(data, (out %>% select(identifier, redcap_event_name, pre_post_complete)), by = c("identifier", "redcap_event_name.factor" = "redcap_event_name"))

data %<>%
  arrange(identifier, redcap_event_name.factor)
  
  
# Set the columns that have values to fill in
cols_to_fill <- c("age" ,
                  "gender_identity.factor",
                  "race_ethnicity",
                  "sexual_orientation",
                  "department_randomization.factor",
                  "specialty.factor",
                  "bhc.factor",
                  "degree.factor",
                  "years_since_training",
                  "years_at_clinic",
                  "randomization.factor"
                  )

# Modify the columns in a for-loop
for (i in 1:length(cols_to_fill)){
  data <- fill_values(data, cols_to_fill[i])
}


# # Set the outliers to keep track of sample sizes for analyses
# Values came from values in SAS which was used to determine outliers
data %<>%
  mutate(mbi_dp_sum_out = ifelse(mbi_dp_sum >= 23 & redcap_event_name.factor == "Post" & randomization.factor == "Intervention", 1, 0))

data %<>%
  mutate(scssf_sum_tot_out = ifelse(scssf_sum_tot >= 54 & redcap_event_name.factor == "Pre" & randomization.factor == "Waitlist", 1, 0)) %>%
  mutate(scssf_sum_tot_out = ifelse(scssf_sum_tot >= 57 & redcap_event_name.factor == "Pre" & randomization.factor == "Intervention", 1, scssf_sum_tot_out)) #%>%

data %<>%
  mutate(fi_tot_out = ifelse(fi_tot <= 2.5 & redcap_event_name.factor == "Pre" & randomization.factor == "Waitlist", 1, 0)) #%>%
  #mutate(fi_tot_out = ifelse(fi_tot >= 9 & redcap_event_name.factor == "Pre" & randomization.factor == "Intervention", 1, fi_tot_out))

data %<>%
  mutate(sfi_tot_out = ifelse(sfi_tot <= 3.083 & redcap_event_name.factor == "Pre" & randomization.factor == "Waitlist", 1, 0)) #%>%
  # mutate(sfi_tot_out = ifelse(sfi_tot >= 9 & redcap_event_name.factor == "Pre" & randomization.factor == "Intervention", 1, sfi_tot_out))

save(data, file = here("data/prepped_data.Rdata"))
