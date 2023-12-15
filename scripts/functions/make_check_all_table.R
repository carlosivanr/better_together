################################################################################
# Carlos Rodriguez, PhD. Dept. of Family Medicine, CU Anschutz Medical Campus
# February 06, 2023

# Updated by Kaitlyn Bertin (KB), ACCORDS - April 6, 2023

# Make check all table:
# This wrapper was created to generate frequency tables for RedCap data that are
# check all that apply. For example, in questions of race and ethnicity, 
# participants can select several options because they're not mutually 
# exclusive. In order to display these type of data, each option must be a
# binary indicator (0 or 1) in its own column (wide format). 

# Inputs:
# data, the data set containing the check all that apply variables. All variables
#   should be prepared as binary indicators prior to using as input
# vars, a character vector of the check all that apply variable names
# by_var, an optional variable to display the data broken apart by an additional
#   variable, such as sex, intervention, etc.
# grouping_label, a character string to label the input variables

# Requires: gtsummary, tidyverse, rlang, tibble packages
################################################################################


# KB: in first line below, I added new argument "pct_digits" to define number of decimal places consistently. Set to 1 as default but that can be changed when running the function
make_check_all_tbl <- function(data, vars, by_var = NULL, grouping_label, pct_digits = 1){
  # Create an index where the column sums are used to sort the variables -------
  index <- data %>% 
    select(all_of(vars)) %>%
    colSums() %>%
    order(decreasing = TRUE)
  
  # Create a new vector of column names ordered by overall frequency -----------
  ordered_vars <- vars[index]
  
  # Create a basic gtsummary table ---------------------------------------------
  tbl <- data %>%
    select(all_of(c(ordered_vars, by_var))) %>%
    tbl_summary(by = all_of(by_var),
                digits = list(all_categorical() ~ c(0, pct_digits)) # KB added this line to define number of decimal places consistently for percentages (using new pct_digits argument above)
                )
  
  # Create a named set of arguments --------------------------------------------
  # unpacked add_variable_grouping() function from bstfun bc couldn't get the
  # name of the vector to be named dynamically
  dots <- rlang::dots_list("grouping_label" = ordered_vars)
  #rlang::is_named(dots)
  
  # Convert the named set of arguments -----------------------------------------
  df_insert <- 
    dots %>%
    imap(~which(tbl$table_body$variable %in% .x[1])[1]) %>%
    unlist() %>%
    tibble::enframe("include_group", "insert_above_row_number")
  
  # Insert the grouping variable dynamically -----------------------------------
  df_insert[1] <- grouping_label
  
  for (i in rev(seq_len(nrow(df_insert)))) {
    tbl <-
      tbl %>%
      gtsummary::modify_table_body(
        ~ tibble::add_row(.x,
                          variable = df_insert$include_group[i],
                          label = df_insert$include_group[i],
                          row_type = "label",
                          .before = df_insert$insert_above_row_number[i])
      )
  }
  
  # update the indentation for the grouped variables ---------------------------
  tbl <-
    tbl %>%
    gtsummary::modify_table_styling(
      columns = "label",
      rows = .data$variable %in% unname(unlist(dots)) & .data$row_type %in% "label",
      text_format = "indent"
    ) %>%
    gtsummary::modify_table_styling(
      columns = "label",
      rows = .data$variable %in% unname(unlist(dots)) & !.data$row_type %in% "label",
      undo_text_format =  TRUE,
      text_format = "indent"
    ) %>%
    gtsummary::modify_table_styling(
      columns = "label",
      rows = .data$variable %in% unname(unlist(dots)) & !.data$row_type %in% "label",
      text_format = "indent2"
    )

  # return final tbl -----------------------------------------------------------
  return(tbl)
}
