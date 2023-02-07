# Score Practice Culture Scale %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Carlos Rodriguez, PhD. CU Anschutz Dept. of Family Medicine
#
# Description:
# This function calculates subscores for the practice culture scale in the PATHWEIGH
# practice member survey data. The practice culture scale is a 22 item scale 
# designed to measure:
# 1) Improvement and Change Culture
#     - 1, 6:9, 13:14, 16:17, 20
#     - Scoring: 25*((q1 + q6 + q7 + q8 + q9 + q13 + q14 +q16 + q17 + q20) -10)/10

# 2) Work Relationships
#     - 2:3, 5, 11:12, 15, 21:22
#     - Scoring: 25*((q2 + q3 + q5 + q11 + q12 + q15 + q21 + q22) – 8) /8

# 3) Chaos
#     - 4, 10, 18:19
#     - Scoring:  25*((q4 + q10 + (6-q18) + q19) - 4)/4
#     - N.b.: Item 18 (stability) is reverse scored

# Inputs: pre-processed redcap data download
# Output: returns a data frame
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

score_pc_scale <- function(temp){
  
  pc_items <- c("pc_discussion", "pc_opinion", "pc_jobs_fit",  "pc_chaos", "pc_rely",
                "pc_effort", "pc_input", "pc_improve", "pc_time", "pc_organized", 
                "pc_tension" , "pc_thoughtful", "pc_data", "pc_share", "pc_affects",
                "pc_available", "pc_efforts", "pc_stable", "pc_change", "pc_leadership",
                "pc_enjoy", "pc_environment")
  
  # Test to ensure that all items are in the data set
  if(sum(names(temp) %in% pc_items) == length(pc_items)){
  
    ## Set NA function to check for differential missingness across questions
    # for each subscore
    na_fun = function(vec){length(which(is.na(vec)))}
    
    # Set the names of the practice culture items
    pc_items <- names(temp %>% select(starts_with("pc_") & !ends_with(".factor")))
    
    ## 1. Improvement and change culture
    pc_imp_items <- pc_items[c(1, 6:9, 13:14, 16:17, 20)]
                                              
    ## 2. Work relationships
    pc_rel_items <- pc_items[c(2:3, 5, 11:12, 15, 21:22)]
  
    ## 3. Chaos
    pc_chs_items <- pc_items[c(4, 10, 18:19)]
  
    # na.rm FALSE will set NA for participants that did not score all items
    # Reverse score item #18 stability before scoring.
    temp %<>%
      mutate(
        pc_stable = 6-temp$pc_stable,
        pc_imp_subscore = 25 * (rowSums(select(., all_of(pc_imp_items)), na.rm = FALSE) - 10)/10,
        pc_rel_subscore = 25 * (rowSums(select(., all_of(pc_rel_items)), na.rm = FALSE) - 8)/8,
        pc_chs_subscore = 25 * (rowSums(select(., all_of(pc_chs_items)), na.rm = FALSE) - 4)/4,
        pc_scale_mean = (pc_imp_subscore + pc_rel_subscore + pc_chs_subscore) / 3)
    
    # Return the data frame as output
    return(temp)
    } else {
      stop("One or more of the practice culture scale items were not found in the input data frame. Consider reviewing data processing pipeline.")
    }
}
# culture Average - looks like it is calculated from the columns that start with pc_
# Use the columns that start with pc_ but that do not end with .factor
