# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Carlos Rodriguez, PhD. Dept. of Family Medicine, CU Anschutz Medical Campus
# December 16, 2023
# Better Together
# Developed to trouble shoot the multiple imputation procedures in R to 
# understand what may be happening in SAS
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# Trying to get multiple imputation to work
dvs <- c("mbi_ee_sum",
         "mbi_dp_sum",
         "mbi_pa_sum",
         "misshp_tot",
         "yis_sum",
         "ls_sum",
         "scssf_sum_tot",
         "sfi_tot")
         
         
         
dvs <- "mbi_ee_sum"
         
for (i in dvs){
  dv <- i
  print(dv)

  # Create an imputation data set for one variable
  data_to_imp <- data %>%
    select(identifier, redcap_event_name.factor, rlang::sym(dv)) %>%
    group_by(identifier) %>%
    pivot_wider(., names_from = redcap_event_name.factor, values_from = rlang::sym(dv)) %>%
    left_join(., 
              (data %>%
                 filter(redcap_event_name.factor =="Pre") %>%
                 select(identifier, years_since_training, specialty.factor, degree.factor, gender_identity.factor, race_ethnicity, sexual_orientation, randomization.factor) %>%
                 mutate(across(specialty.factor:sexual_orientation, ~if_else(is.na(.x), "Unknown", .x)))),
              by = "identifier") %>%
    
    # These variables cause mice() to log events due to constant
    select(-specialty.factor, -degree.factor, -gender_identity.factor, -race_ethnicity, -sexual_orientation)
  

  invisible(
  imp_data <- mice(data_to_imp, m=10, maxit = 50, method = 'pmm', seed = 1101, printFlag = F)
  )
  
  
  test <- with(data=imp_data, exp = lm(I(Post-Pre)~randomization.factor))
  # test <- with(data = imp_data, exp = gls(I(Post-Pre) ~ randomization.factor))
  combine <- pool(test)
  print(summary(combine))
}
