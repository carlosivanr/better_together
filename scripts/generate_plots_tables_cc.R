# Generate plots and tables for the Intention to treat analysis

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

# Source Functions -------------------------------------------------------------
source(here("scripts", "functions", "disp_stats_plots.R"))

# Load Data --------------------------------------------------------------------
load(here("data", "prepped_data.Rdata"))


# MBI EE Sum -------------------------------------------------------------------
# Run the function
mbi_ee_list <- disp_stats_plot("mbi_ee_sum", "complete_case")

# Display plot
mbi_ee_list[[4]]

# Save plot
ggsave(here("figures", "complete_case", "mbi_ee_sum.png"), 
       height = 4, 
       width = 5.5, 
       device='png', 
       dpi=600)


# MBI DP Sum -------------------------------------------------------------------
# Run the function
mbi_dp_list <- disp_stats_plot("mbi_dp_sum", "complete_case")

# Display plot
mbi_dp_list[[4]]

# Save plot
ggsave(here("figures", "complete_case", "mbi_dp_sum.png"), 
       height = 4, 
       width = 5.5, 
       device='png', 
       dpi=600)

# MBI PA Sum -------------------------------------------------------------------
# Run the function
mbi_pa_list <- disp_stats_plot("mbi_pa_sum", "complete_case")

# Display plot
mbi_pa_list[[4]]

# Save plot
ggsave(here("figures", "complete_case", "mbi_pa_sum.png"), 
       height = 4, 
       width = 5.5, 
       device='png', 
       dpi=600)


# Moral Injury Syndrome Scale --------------------------------------------------
# Run the function
misshp_list <- disp_stats_plot("misshp_tot", "complete_case")

# Display plot
misshp_list[[4]]

# Save plot
ggsave(here("figures", "complete_case", "misshp.png"), 
       height = 4, 
       width = 5.5, 
       device='png', 
       dpi=600)

# Young's Imposter Syndrome Scale ----------------------------------------------
yissum_list <- disp_stats_plot("yissum", "complete_case")

# Display plot
yissum_list[[4]]

# Save plot
ggsave(here("figures", "complete_case", "yissum.png"), 
       height = 4, 
       width = 5.5, 
       device='png', 
       dpi=600)


# UCLA Loneliness Scale --------------------------------------------------------
ls3_list <- disp_stats_plot("ls3_tot", "complete_case")

# Display plot
ls3_list[[4]]

# Save plot
ggsave(here("figures", "complete_case", "ls3.png"), 
       height = 4, 
       width = 5.5, 
       device='png', 
       dpi=600)


# Neff's Self-Compassion Scale Short Form --------------------------------------
scssf_list <- disp_stats_plot("scssf_sum_tot", "complete_case")

# Display plot
scssf_list[[4]]

# Save plot
ggsave(here("figures", "complete_case", "scssf.png"), 
       height = 4, 
       width = 5.5, 
       device='png', 
       dpi=600)


# Flourishing Index ------------------------------------------------------------
fi_list <- disp_stats_plot("fi_tot", "complete_case")

# Display plot
fi_list[[4]]

# Save plot
ggsave(here("figures", "complete_case", "fi.png"), 
       height = 4, 
       width = 5.5, 
       device='png', 
       dpi=600)


# Secure Flourishing Index -----------------------------------------------------
sfi_list <- disp_stats_plot("sfi_tot", "complete_case")

# Display plot
sfi_list[[4]]

# Save plot
ggsave(here("figures", "complete_case", "sfi.png"), 
       height = 4, 
       width = 5.5, 
       device='png', 
       dpi=600)


# Put together Tables ----------------------------------------------------------

## Table of values ----
long_table <- bind_rows(
  mbi_ee_list[[5]],
  mbi_dp_list[[5]],
  mbi_pa_list[[5]],
  misshp_list[[5]],
  yissum_list[[5]],
  ls3_list[[5]],
  scssf_list[[5]],
  fi_list[[5]],
  sfi_list[[5]]) %>%
  mutate(Outcome = case_when(Outcome == "mbi_ee_sum" ~ "MBI EE",
                             Outcome == "mbi_dp_sum" ~ "MBI DP",
                             Outcome == "mbi_pa_sum" ~ "MBI PA",
                             Outcome == "misshp_tot" ~ "MISS HP",
                             Outcome == "yissum" ~ "YISS",
                             Outcome == "ls3_tot" ~ "ULS",
                             Outcome == "scssf_sum_tot" ~ "SCSSF",
                             Outcome == "fi_tot" ~ "FI",
                             Outcome == "sfi_tot" ~ "SFI")) %>%
  mutate_if(is.numeric, ~round(., 2)) %>%
  mutate("Estimate (95% CI)" = str_c(Estimate, " ", "(", Lower, ", ", Upper, ")")) %>%
  select(Outcome, Comparison, "Estimate (95% CI)", Probt) %>%
  rename(p = Probt) %>%
  mutate(Comparison = ifelse(Comparison == "DID", "Difference in change", Comparison)) %>%
  gt::gt()

gt::gtsave(long_table, "estimate_ci_p.rtf", path = here("tables", "complete_case"))


## Final table of mixed effect models ----



dependent_vars <- c(
                    "mbi_ee_sum",
                    "mbi_dp_sum",
                    "mbi_pa_sum",
                    "misshp_tot",
                    "yis_sum",
                    "ls_sum",
                    "scssf_sum_tot",
                    "fi_tot",
                    "sfi_tot")

# creating a matrix with 0 rows  
# and columns  
mat = matrix(ncol = 0, nrow = 0) 
  
# converting the matrix to data  
# frame 
final_table = data.frame(mat)  

for (i in dependent_vars){
  #print(i)
  # The number of unique patients in Post and Pre
  # Drops anyone who did not have a score, did not have degree, did not have randomization, or redcap_event name
  # 
  # Get the identifiers for those that have complete cases, must have both
  # pre and post scores for a given dependent variable
  cc_ids <- 
    data %>% 
    select(identifier, !!rlang::sym(i), redcap_event_name.factor) %>% 
    filter(!is.na(!!rlang::sym(i))) %>%
    group_by(identifier) %>% 
    count() %>% 
    filter(n == 2) %>% 
    pull(identifier)
  
  participant_n <-
  data %>% 
  filter(identifier %in% cc_ids,
         !is.na(!!rlang::sym(i)),
         !is.na(degree.factor),
         !is.na(randomization.factor),
         !is.na(redcap_event_name.factor)) %>%
  group_by(redcap_event_name.factor, randomization.factor) %>%
  summarise(n_distinct_subjs = n_distinct(identifier), .groups = "drop") %>%
  #arrange(desc(redcap_event_name.factor)) %>%
  pivot_wider(names_from = redcap_event_name.factor, values_from = n_distinct_subjs)
  
  # add an empty row to df for merging
  participant_n[3,] <- NA
  
  if (i == "mbi_ee_sum"){
    df <- get("mbi_ee_list")[[5]]
    
    } else if (i == "mbi_dp_sum"){
    df <- get("mbi_dp_list")[[5]]  
    
    } else if (i == "mbi_pa_sum"){
    df <- get("mbi_pa_list")[[5]]
    
    } else if (i == "misshp_tot"){
    df <- get("misshp_list")[[5]]
    
    } else if (i == "yis_sum"){
    df <- get("yissum_list")[[5]]
    
    } else if (i == "ls_sum"){
    df <- get("ls3_list")[[5]]
    
    } else if (i == "scssf_sum_tot"){
    df <- get("scssf_list")[[5]]
    
    } else if (i == "fi_tot"){
    df <- get("fi_list")[[5]]
    
    } else if (i == "sfi_tot"){
    df <- get("sfi_list")[[5]]  
    }
  
  results <- 
  bind_cols(participant_n, df) %>%
  mutate(randomization.factor = ifelse(is.na(randomization.factor), "Dfference in change", as.character(randomization.factor))) %>%
  select(-Outcome) %>%
  mutate(Estimate = ifelse(Comparison == "DID", str_c(round(Estimate,2), " (", round(Lower,2), " to ", round(Upper,2), ")"), 
                           str_c(round(Estimate,2), " (", round(StdErr,2), ")"))) %>%
  select(-StdErr, -Lower, -Upper) %>%
  mutate(Probt = ifelse(Comparison == "DID", round(Probt,2), NA))
  

tab_3 <- bind_cols(results[1:2,2:3] %>% t(.),
          (results[1, 5]%>%
             add_row()),
          results[2, 5]%>%
             add_row(),
          (results[3,5:6] %>%
             add_row())) %>%
  mutate(outcome = i, event = c("Pre","Post")) %>%
  select(outcome, event, `...1`, `Estimate...3`, `...2`, `Estimate...4`, everything()) %>%
  rename(control_n = `...1`,
         intervention_n = `...2`,
         control_estimate_se = `Estimate...3`,
         intervention_estimate_se = `Estimate...4`,
         did_95_ci = `Estimate...5`)

final_table <- bind_rows(final_table, tab_3)
}


write_csv(final_table, na = "", file = here("tables", "complete_case", "mixed_mod_results_table.csv"))



