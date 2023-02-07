# Score Implementation Climate Scale %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Carlos Rodriguez, PhD. CU Anschutz Dept. of Family Medicine
#
# Description:
# This function calculates the average implementation climate scale score for 
# each participant in the PATHWEIGH practice member survey data. Additionally,
# this function will rename the implementation climate scale columns by adding
# a "ic_" prefix to facilitate select(starts_with())
#
# All 6 implementation climate scale questions are scored from 1:5, where 1 ==
# not at all and 5 == to a great extent.
# 
# Inputs: pre-processed redcap data download
# Output: returns a data frame with the average placed immediately after the set
# of 6 implementation climate scale items.
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

score_ic_scale <- function(temp){
  
  # Set the implementation climate items as named in the redcap download
  ic_scale_items <- c("provide_wls",     
                      "help_wls",        
                      "support_to_id",   
                      "support_to_asst", 
                      "recognition",     
                      "appreciation")
  
  # Test to ensure that all items are in the data set
  if(sum(names(temp) %in% ic_scale_items) == length(ic_scale_items)){
    
    # Calculate the average of the implementation climate scale items
    temp %<>%
      mutate(ic_scale_mean = rowMeans(select(., all_of(ic_scale_items))))
    
    # Reorder the columns, to place the ic_scale_mean after the ic scale items
    # index all positions from 1 through position of ic item 1 - 1, 
    # then select all ic items, the avg ic scale, 
    # finally place the remaining columns
    temp %<>%
      select(1:(grep(ic_scale_items[1], names(temp))[1] - 1),
             all_of(ic_scale_items), 
             ic_scale_mean, 
             everything())
    
    # Add a prefix to the first six implementation climate scale items.
    # First identify the numeric indexes of the columns that match the 
    # ic scale items by name, then use the first 6 indexes to prevent renaming
    # the columns that end in the .factor suffix.
    names(temp)[grep(
      str_c(ic_scale_items, collapse = "|"), colnames(temp)
      )[1:6]] <- str_c("ic_", names(temp[ic_scale_items]))
    
    # Return the data frame as output
    return(temp)
    
    } else {
      stop("The implementation climate scale items, were not found in the input data frame. Consider reviewing data processing pipeline.")
    }
}