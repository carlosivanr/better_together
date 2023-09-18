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
# Each participant can be associated with 3 distinct events, randomization, pre,
# and post intervention. Randomization events will not contain any data for the
# pre/post outcomes and need to be removed.

# Filter the unique randomization event variables to a separate data frame
randomization_arm_1 <- 
  data %>%
  filter(redcap_event_name == "randomization_arm_1") %>%
  select(identifier, randomization, randomization_complete)

# Remove randomization rows and columns
data %<>%
  filter(redcap_event_name != "randomization_arm_1") %>%
  select(-randomization, -randomization_complete)

# Join in randomization (treatment) and randomization complete values
data %<>%
  left_join(., 
            select(randomization_arm_1, identifier, randomization, randomization_complete), 
            by = "identifier")

# KB: Calculate instrument scores ----------------------------------------------

# Note: option "na.rm=TRUE" allows missing items but excludes them from the mean. 
# If we don't want to allow missing items (set subscale score to NA if any 
# item is NA), then use na.rm=FALSE

data2 <- data %>%
  mutate(
    
    ## Maslach Burnout Inventory (MBI)
    
    # Emotional Exhaustion (EE) subscale: items 1, 2, 3, 6, 8, 13, 14, 16, 20
    # Depersonalization (DP) subscale: items 5, 10, 11, 15, 22
    # Personal Accomplishment (PA) subscale: items 4, 7, 9, 12, 17, 18, 19, 21
    
    # Scores as item means/averages
    mbi_ee_avg = rowMeans(across(num_range("mbi", c(1:3, 6, 8, 13, 14, 16, 20))), na.rm=TRUE), # currently this allows missing items
    mbi_dp_avg = rowMeans(across(num_range("mbi", c(5, 10, 11, 15, 22))), na.rm=TRUE),
    mbi_pa_avg = rowMeans(across(num_range("mbi", c(4, 7, 9, 12, 17:19, 21))), na.rm=TRUE),
    
    # same as above, just different variable names (moved "avg" from suffix to prefix for consistency with new SCS variables)
    mbi_avg_ee = rowMeans(across(num_range("mbi", c(1:3, 6, 8, 13, 14, 16, 20))), na.rm=TRUE), # currently this allows missing items
    mbi_avg_dp = rowMeans(across(num_range("mbi", c(5, 10, 11, 15, 22))), na.rm=TRUE),
    mbi_avg_pa = rowMeans(across(num_range("mbi", c(4, 7, 9, 12, 17:19, 21))), na.rm=TRUE),
    
    # Scores as item sums
    mbi_ee_sum = rowSums(across(num_range("mbi", c(1:3, 6, 8, 13, 14, 16, 20))), na.rm=FALSE), # na.rm=FALSE so sum score not calculated if any item is missing
    mbi_dp_sum = rowSums(across(num_range("mbi", c(5, 10, 11, 15, 22))), na.rm=FALSE),
    mbi_pa_sum = rowSums(across(num_range("mbi", c(4, 7, 9, 12, 17:19, 21))), na.rm=FALSE),
    
    # same as above, just different variable names (moved "sum" from suffix to prefix for consistency with new SCS variables)
    mbi_sum_ee = rowSums(across(num_range("mbi", c(1:3, 6, 8, 13, 14, 16, 20))), na.rm=FALSE), # na.rm=FALSE so sum score not calculated if any item is missing
    mbi_sum_dp = rowSums(across(num_range("mbi", c(5, 10, 11, 15, 22))), na.rm=FALSE), 
    mbi_sum_pa = rowSums(across(num_range("mbi", c(4, 7, 9, 12, 17:19, 21))), na.rm=FALSE), 
    
    # count of items missing for each subscale
    countNA_mbi_ee9 = rowSums(is.na(across(num_range("mbi", c(1:3, 6, 8, 13, 14, 16, 20))))), # count how many items are missing (out of 9) for EE
    countNA_mbi_dp5 = rowSums(is.na(across(num_range("mbi", c(5, 10, 11, 15, 22))))), # count how many items are missing (out of 5) for DP
    countNA_mbi_pa8 = rowSums(is.na(across(num_range("mbi", c(4, 7, 9, 12, 17:19, 21))))), # count how many items are missing (out of 8) for PA
    
    
    ## Self-Compassion Scale - Short form (SCS-SF)
    
    # https://self-compassion.org/wp-content/uploads/2021/03/SCS-SF-information.pdf
    # Create new item variables with reverse scoring for items 1, 4, 8, 9, 11, 12
    scssfnew1 = 6 - scs1,
    scssfnew2 = scs2,
    scssfnew3 = scs3,
    scssfnew4 = 6 - scs4,
    scssfnew5 = scs5,
    scssfnew6 = scs6,
    scssfnew7 = scs7,
    scssfnew8 = 6 - scs8,
    scssfnew9 = 6 - scs9,
    scssfnew10 = scs10,
    scssfnew11 = 6 - scs11,
    scssfnew12 = 6 - scs12,
    
      # Note: at least one participant (110) only answered one item (scs1) so if we allow any number of missing items, 
      # that person's over-identification and total scores are both based on that one item 
    
      # TO DO: we should decide our own rules for missing items because the scoring instructions don't say 
      # (and each subscale is only 2 items so if either is missing then the subscale is one item)
    
    # Scores as item means
    # scssf_kind = rowMeans(across(num_range("scssfnew", c(2, 6))), na.rm=FALSE), # For now set to FALSE so subscale score not calculated if any item is missing
    # scssf_judg = rowMeans(across(num_range("scssfnew", c(11, 12))), na.rm=FALSE),
    # scssf_human = rowMeans(across(num_range("scssfnew", c(5, 10))), na.rm=FALSE),
    # scssf_isol = rowMeans(across(num_range("scssfnew", c(4, 8))), na.rm=FALSE),
    # scssf_mind = rowMeans(across(num_range("scssfnew", c(3, 7))), na.rm=FALSE),
    # scssf_ident = rowMeans(across(num_range("scssfnew", c(1, 9))), na.rm=FALSE),
    # scssf_tot = rowMeans(across(starts_with("scssf_")), na.rm=FALSE), # For now I set this to FALSE so we don't allow a missing subscale score for the total
    scssf_avg_kind = rowMeans(across(num_range("scssfnew", c(2, 6))), na.rm=FALSE), # For now set to FALSE so subscale score not calculated if any item is missing
    scssf_avg_judg = rowMeans(across(num_range("scssfnew", c(11, 12))), na.rm=FALSE),
    scssf_avg_human = rowMeans(across(num_range("scssfnew", c(5, 10))), na.rm=FALSE),
    scssf_avg_isol = rowMeans(across(num_range("scssfnew", c(4, 8))), na.rm=FALSE),
    scssf_avg_mind = rowMeans(across(num_range("scssfnew", c(3, 7))), na.rm=FALSE),
    scssf_avg_ident = rowMeans(across(num_range("scssfnew", c(1, 9))), na.rm=FALSE),
    scssf_avg_tot = rowMeans(across(starts_with("scssf_avg_")), na.rm=FALSE), # For now I set this to FALSE so we don't allow a missing subscale score for the total
    scssf_tot = scssf_avg_tot,
    
    # Scores as item sums
    scssf_sum_kind = rowSums(across(num_range("scssfnew", c(2, 6))), na.rm=FALSE), # Set to FALSE so score not calculated if any item is missing
    scssf_sum_judg = rowSums(across(num_range("scssfnew", c(11, 12))), na.rm=FALSE),
    scssf_sum_human = rowSums(across(num_range("scssfnew", c(5, 10))), na.rm=FALSE),
    scssf_sum_isol = rowSums(across(num_range("scssfnew", c(4, 8))), na.rm=FALSE),
    scssf_sum_mind = rowSums(across(num_range("scssfnew", c(3, 7))), na.rm=FALSE),
    scssf_sum_ident = rowSums(across(num_range("scssfnew", c(1, 9))), na.rm=FALSE),
    scssf_sum_tot = rowSums(across(starts_with("scssf_sum_")), na.rm=FALSE), 
    
    countNA_scssf12 = rowSums(is.na(across(num_range("scs", c(1:12))))), # count how many items are missing (out of 12) for SCS-SF
    
    ## Moral Injury Symptom Scale – Healthcare Providers (MISS-HP)
    # Create new item variables with reverse scoring for items 5, 6, 7, 10
    misshpnew1 = miss1,
    misshpnew2 = miss2,
    misshpnew3 = miss3, 
    misshpnew4 = miss4, 
    misshpnew5 = 11 - miss5,
    misshpnew6 = 11 - miss6,
    misshpnew7 = 11 - miss7,
    misshpnew8 = miss8,
    misshpnew9 = miss9,
    misshpnew10 = 11 - miss10,
    # Calculate total score as sum of items
    misshp_tot = rowSums(across(num_range("misshpnew", c(1:10))), na.rm=FALSE), # For now set to FALSE so sum score not calculated if any item is missing
    
    countNA_misshp10 = rowSums(is.na(across(num_range("miss", c(1:10))))), # count how many items are missing (out of 10) for MISS-HP
    
    ## Young Impostor Scale (YIS) - Responding “Yes” to 5 or more of these questions considered a positive finding of IS
    countNA_yis8 = rowSums(is.na(across(num_range("yis", c(1:8))))), # count how many items are missing (out of 8) - to be used in count of "yes" responses below
    # yis_count = rowSums(across(num_range("yis", c(1:8))), na.rm=TRUE), # count of "yes" responses - allows missing items
    yis_count = if_else(countNA_yis8 == 8, NA_real_, rowSums(across(num_range("yis", c(1:8))), na.rm=TRUE)), # count of "yes" responses - allows missing items, unless all items missing
    yis_ispos = if_else(yis_count >= 5, 1, 0, NA_real_),
    
    # Three-item Loneliness Scale (LS)
    ls3_tot = rowSums(across(starts_with("ls_")), na.rm=FALSE), # don't calculate sum score if any items missing
    ls3_lonely = if_else(ls3_tot >= 6, 1, 0, NA_real_), # dichotomize scores (>=6: lonely, 3-5: not lonely)
    countNA_ls3 = rowSums(is.na(across(starts_with("ls_")))), # count how many items are missing (out of 3) for LS
    
    ## Flourish Index (FI) and Secure Flourish Index (SFI)
    ## Domain-specific indices 
    # D1. Happiness and life satisfaction
    sfi_d1 = rowMeans(across(num_range("sfi", c(1, 2))), na.rm=FALSE),  # For now set to FALSE so domain score not calculated if an item is missing
    # D2. Mental and physical health
    sfi_d2 = rowMeans(across(num_range("sfi", c(3, 4))), na.rm=FALSE),
    # D3. Meaning and purpose
    sfi_d3 = rowMeans(across(num_range("sfi", c(5, 6))), na.rm=FALSE),
    # D4. Character and virtue
    sfi_d4 = rowMeans(across(num_range("sfi", c(7, 8))), na.rm=FALSE),
    # D5. Close social relationships
    sfi_d5 = rowMeans(across(num_range("sfi", c(9, 10))), na.rm=FALSE),
    # D6. Financial and material stability
    sfi_d6 = rowMeans(across(num_range("sfi", c(11, 12))), na.rm=FALSE),
    # FI total score (mean of first 5 domain indices)
    fi_tot = rowMeans(across(num_range("sfi_d", c(1:5))), na.rm=FALSE), # For now set to FALSE so total score not calculated if any domain score is missing
    # SFI total score (mean of all 6 domain indices)
    sfi_tot = rowMeans(across(num_range("sfi_d", c(1:6))), na.rm=FALSE), 
    
    countNA_sfi12 = rowSums(is.na(across(num_range("sfi", c(1:12))))), # count how many items are missing (out of 12) for SFI
    countNA_fi10 = rowSums(is.na(across(num_range("sfi", c(1:10))))), # count how many items are missing (out of 10) for FI
  )

# Check data for only the survey instruments -----------------------------------
data_check <- data2 %>%
  select(identifier, redcap_event_name, starts_with("mbi"), starts_with("scs"), starts_with("miss"), 
         starts_with("yis"), starts_with("ls"), starts_with("sfi"), starts_with("fi_"), starts_with("countNA_"))

# data_check <- data2 %>%
#   filter(identifier %in% c(26, 41, 100, 122, 146, 150, 172)) %>%
#   select(identifier, redcap_event_name, starts_with("mbi"))

# Checking counts of missing items for MBI scales - 5 participants are missing all items but will be excluded later anyway because randomization not = complete
# with(data2, table(countNA_mbi_ee9))
# with(data2, table(countNA_mbi_dp5))
# with(data2, table(countNA_mbi_pa8))

# Check participants missing all items for any scale
data2 %>%
  select(identifier, starts_with("countNA_")) %>%
  filter(countNA_mbi_ee9 == 9 | countNA_mbi_dp5 == 5 | countNA_mbi_pa8 == 8 | countNA_scssf12 == 12 | countNA_misshp10 == 10 | countNA_yis8 == 8 |
         countNA_ls3 == 3 | countNA_sfi12 == 12 | countNA_fi10 == 10)

# Check participants missing all items on all scales
data2 %>%
  select(identifier, starts_with("countNA_")) %>%
  filter(countNA_mbi_ee9 == 9 & countNA_mbi_dp5 == 5 & countNA_mbi_pa8 == 8 & countNA_scssf12 == 12 & countNA_misshp10 == 10 & countNA_yis8 == 8 &
           countNA_ls3 == 3 & countNA_sfi12 == 12 & countNA_fi10 == 10)

# Check any other participants missing at least one item on any scale (excluding those missing all items on all scales)
data2 %>%
  select(identifier, starts_with("countNA_")) %>%
  filter(countNA_mbi_ee9 >= 1 | countNA_mbi_dp5 >= 1 | countNA_mbi_pa8 >= 1 | countNA_scssf12 >= 1 | countNA_misshp10 >= 1 | countNA_yis8 >= 1 |
           countNA_ls3 >= 1 | countNA_sfi12 >= 1 | countNA_fi10 >= 1) %>%
  filter(!(countNA_mbi_ee9 == 9 & countNA_mbi_dp5 == 5 & countNA_mbi_pa8 == 8 & countNA_scssf12 == 12 & countNA_misshp10 == 10 & countNA_yis8 == 8 &
             countNA_ls3 == 3 & countNA_sfi12 == 12 & countNA_fi10 == 10)) %>%
  nrow()

# Check participants missing SFI score but not FI score (because only domain 6 was missing)
data2 %>%
  select(identifier, starts_with("sfi_d"), sfi_tot, fi_tot) %>%
  filter(is.na(sfi_tot)) 

# Check participants missing at least 1 but not all YIS items
data2 %>%
  select(identifier, countNA_yis8, num_range("yis", c(1:8)), yis_count, yis_ispos) %>%
  filter(countNA_yis8 >= 1 & countNA_yis8 < 8) 

# Check participant 110
data2 %>%
  select(identifier, starts_with("scssf_"), starts_with("misshp_"), 
         starts_with("yis_"), starts_with("ls3_"), starts_with("sfi_"), starts_with("fi_")) %>%
  filter(identifier == 110)




data <- data2



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

# Check that all remaining values are in fact missing
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





# Apply Redcap processing ------------------------------------------------------
# Applies the downloaded R commands to apply labels, create factored columns,
# and set levels. Saved in a separate script to improve readability.
source(here("scripts", "apply_redcap_processing.R")) 

# Set the levels of redcap_event_name.factor to available values only ----------
# Perform after redcap processing
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


# Save Data --------------------------------------------------------------------

## Save to Rdata ----
save(data, file = here("data/prepped_data.Rdata"))

## Save the prepped Rdsat to .csv ----
data %>%
  write.csv(., 
          file = here("data/better_together_prepped_data.csv"),
          row.names = FALSE)

## Save to .csv for SAS ----
## These demographic variables  will need modification to replace the 
## name with the label
out_demo <- data %>% 
  select(living_situation___0:living_situation___9,
         caregiving___0:caregiving___4)

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
         mbi_dp_sum,
         mbi_pa_sum,
         
         mbi_sum_ee, # KB: these 3 are same as above, just different variable naming convention for consistency with new SCS-SF variables
         mbi_sum_dp,
         mbi_sum_pa,
         
         misshp_tot,
         yissum,
         yis_ispos,
         ls3_tot,
         
         scssf_tot,
         scssf_avg_tot,   # KB added these 2: new SCS-SF variables (mean vs sum)
         scssf_sum_tot,
         
         fi_tot,
         sfi_tot) %>%
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
                  "years_at_clinic")

# Modify the columns in a for-loop
for (i in 1:length(cols_to_fill)){
  out <<- fill_values(out, cols_to_fill[i])
}

### Write out data set----
out %>%
  write.csv(., 
          file = here("data/better_together_sas_data.csv"),
          row.names = FALSE)
