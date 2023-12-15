# Generate plots and tables 

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

# MBI EE Sum -------------------------------------------------------------------
# Run the function
mbi_ee_list <- disp_stats_plot("mbi_ee_sum")

# Display plot
mbi_ee_list[[4]]

# Save plot
ggsave(here("figures", "mbi_ee_sum.png"), 
       height = 4, 
       width = 5.5, 
       device='png', 
       dpi=600)


# MBI DP Sum -------------------------------------------------------------------
# Run the function
mbi_dp_list <- disp_stats_plot("mbi_dp_sum")

# Display plot
mbi_dp_list[[4]]

# Save plot
ggsave(here("figures", "mbi_dp_sum.png"), 
       height = 4, 
       width = 5.5, 
       device='png', 
       dpi=600)

# MBI PA Sum -------------------------------------------------------------------
# Run the function
mbi_pa_list <- disp_stats_plot("mbi_pa_sum")

# Display plot
mbi_pa_list[[4]]

# Save plot
ggsave(here("figures", "mbi_pa_sum.png"), 
       height = 4, 
       width = 5.5, 
       device='png', 
       dpi=600)


# Moral Injury Syndrome Scale --------------------------------------------------
# Run the function
misshp_list <- disp_stats_plot("misshp_tot")

# Display plot
misshp_list[[4]]

# Save plot
ggsave(here("figures", "misshp.png"), 
       height = 4, 
       width = 5.5, 
       device='png', 
       dpi=600)

# Young's Imposter Syndrome Scale ----------------------------------------------
yissum_list <- disp_stats_plot("yissum")

# Display plot
yissum_list[[4]]

# Save plot
ggsave(here("figures", "yissum.png"), 
       height = 4, 
       width = 5.5, 
       device='png', 
       dpi=600)


# UCLA Loneliness Scale --------------------------------------------------------
ls3_list <- disp_stats_plot("ls3_tot")

# Display plot
ls3_list[[4]]

# Save plot
ggsave(here("figures", "ls3.png"), 
       height = 4, 
       width = 5.5, 
       device='png', 
       dpi=600)


# Neff's Self-Compassion Scale Short Form --------------------------------------
scssf_list <- disp_stats_plot("scssf_sum_tot")

# Display plot
scssf_list[[4]]

# Save plot
ggsave(here("figures", "scssf.png"), 
       height = 4, 
       width = 5.5, 
       device='png', 
       dpi=600)


# Flourishing Index ------------------------------------------------------------
fi_list <- disp_stats_plot("fi_tot")

# Display plot
fi_list[[4]]

# Save plot
ggsave(here("figures", "fi.png"), 
       height = 4, 
       width = 5.5, 
       device='png', 
       dpi=600)


# Secure Flourishing Index -----------------------------------------------------
sfi_list <- disp_stats_plot("sfi_tot")

# Display plot
sfi_list[[4]]

# Save plot
ggsave(here("figures", "sfi.png"), 
       height = 4, 
       width = 5.5, 
       device='png', 
       dpi=600)


# Put together Tables ----------------------------------------------------------

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

gt::gtsave(long_table, "estimate_ci_p.rtf", path = here("tables"))


# delta_con <- 
#   long_table %>%
#   filter(Comparison == "Change in Control") %>%
#   pivot_wider(names_from = Comparison, values_from = Test) %>%
#   select(Outcome, "Change in Control", p) %>%
#   rename("Change in Control (95% CI)" = "Change in Control")
# 
# delta_int <- 
#   long_table %>%
#   filter(Comparison == "Change in Intervention") %>%
#   pivot_wider(names_from = Comparison, values_from = Test) %>%
#   select(Outcome, "Change in Intervention", p) %>%
#   rename("Change in Intervention (95% CI)" = "Change in Intervention") %>%
#   select(-Outcome)
# 
# 
# delta_int_vs_con <- 
#   long_table %>%
#   filter(Comparison == "DID") %>%
#   pivot_wider(names_from = Comparison, values_from = Test) %>%
#   select(Outcome, "DID", p) %>%
#   rename("Difference in change (95% CI)" = "DID") %>%
#   select(-Outcome)




