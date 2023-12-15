
disp_stats_plot <- function(var, analysis){
  #var <- "yis_ispos"
  
  # For predicted probabilities of binary DVs
  if (var == "yis_ispos"){
      pred_out <- str_c(var, "_predout")
      
      # Import data ------------------------------------------------------------
      assign("pred_out", haven::read_sas(here("data", str_c(pred_out, ".sas7bdat"))))
      
      # Plots ------------------------------------------------------------------
      # Set position dodge argument
      pd <- position_dodge(0.1)
      
      # Set plot title & ylab
      if (var == "yis_ispos"){
        title <- "Young's Imposter Syndrome Scale"
        ylab <- "Probability of Imposter Syndrome"
        }
      

      # Generate plot of predicted probabilities
      p <- 
        pred_out %>%
        group_by(randomization, redcap_event_name) %>%
        summarise(n = n(),
                  mean = mean(predProbs),
                  sd = sd(predProbs),
                  .groups = "drop") %>%
        mutate(se = sd / sqrt(n),
               redcap_event_name = factor(redcap_event_name, levels = c("Pre", "Post"))) %>%
        ggplot(., aes(x = redcap_event_name, y = mean, color = randomization, group = randomization)) +
        geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width = .15, position = pd) +
        geom_point(position = pd) +
        geom_line(position = pd) +
        theme_minimal() +
        theme(axis.line = element_line(color = "grey70")) +
        theme(legend.position = "top") +
        labs(title = title,
             x = NULL,
             y = ylab) +
        theme(plot.caption = element_text(hjust = 0))
      
        # Generate a plot of the probabilities
        # pred_out %>%
        # group_by(randomization, redcap_event_name) %>%
        # summarise(n = n(),
        #           mean = mean(!!rlang::sym(var), na.rm = TRUE),
        #           sd = sd(!!rlang::sym(var), na.rm = TRUE),
        #           .groups = "drop") %>%
        # mutate(se = sd / sqrt(n),
        #        redcap_event_name = factor(redcap_event_name, levels = c("Pre", "Post"))) %>%
        # ggplot(., aes(x = redcap_event_name, y = mean, color = randomization, group = randomization)) +
        # geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width = .15, position = pd) +
        # geom_point(position = pd) +
        # geom_line(position = pd) +
        # theme_minimal() +
        # theme(axis.line = element_line(color = "grey70")) +
        # theme(legend.position = "top") +
        # labs(title = title,
        #      x = NULL,
        #      y = ylab) +
        # theme(plot.caption = element_text(hjust = 0))
      return(p)
      
    } else {

  
    
    # find files that start with var and end with .sas7bdat
    sol <- str_c(var, "_sol")
    t3 <- str_c(var, "_t3")
    lsm <- str_c(var, "_lsmeans")
    diff <- str_c(var, "_diffs")
    did <- str_c(var, "_did")
    
  
    # Import data --------------------------------------------------------------

    
    # Import the solution table
    assign("sol", haven::read_sas(here("data", analysis, str_c(sol, ".sas7bdat"))))
    
    # Import the Type 3 Fixed effects table
    assign("t3", haven::read_sas(here("data", analysis, str_c(t3, ".sas7bdat"))))
    
    # Import the least square means
    assign("lsm", haven::read_sas(here("data", analysis, str_c(lsm, ".sas7bdat"))))
    
    # Import the differences in lsmeans
    assign("diffs", haven::read_sas(here("data", analysis, str_c(diff, ".sas7bdat"))))
  
    # Import the difference in differences
    assign("did", haven::read_sas(here("data", analysis, str_c(did, ".sas7bdat"))))
    
    # Create a table -----------------------------------------------------------
    # row 6 change in control
    # row 1 change in intervention 
    
    deltas <- 
    bind_rows(
      diffs[c(6,1),] %>% select(Estimate, StdErr, Probt, Lower, Upper),
      did %>% select(Estimate, StdErr, Probt, Lower, Upper)
    ) %>%
      mutate(Outcome = var,
             Comparison = c("Change in Control", "Change in Intervention", "DID")) %>%
      select(Outcome, Comparison, everything())
    
    # Analyses Output ----------------------------------------------------------
    solution <- 
      sol %>%
      mutate(across(Estimate:Probt, ~ round(., 4))) %>%
      DT::datatable(., options = list(dom = 't'))
  
    type3fx <- 
      t3 %>%
      mutate(across(FValue:ProbF, ~ round(., 4))) %>%
      DT::datatable(., options = list(dom = 't'))
  
    lsmeans <- 
    lsm %>%
      mutate(across(Estimate:Upper, ~ round(., 4))) %>%
      DT::datatable(., options = list(dom = 't'))
    
  
    # Plots ----------------------------------------------------------------------
    # Set plot title
    if (var == "mbi_ee_sum"){
      title <- "Emotional Exhaustion"
      } else if (var == "mbi_dp_sum") {
        title <- "Depersonalization"
      } else if (var == "mbi_pa_sum"){
        title <- "Personal Accomplishment"
      } else if (var == "misshp_tot"){
        title <- "Moral Injury"
      } else if (var == "yissum") {
        title <-  "Imposter Syndrome"
      } else if (var == "ls3_tot") {
        title <- "Loneliness"
      } else if (var == "scssf_tot" | var == "scssf_sum_tot"){
        title <-  "Self Compassion"
      } else if (var == "fi_tot"){
        title <- "Flourishing Index"
      } else if (var == "sfi_tot"){
        title <- "Secure Flourishing Index"
      }
    
    # Set plot caption
    cap <- str_c("Post-intervention Difference: ",
                 round(diffs$Estimate[2], 2), " (95% CI, ", 
                 round(diffs$Lower[2], 2), " to ", 
                 round(diffs$Upper[2], 2), "); P = ",
                 round(diffs$Probt[2], 2))
      
    # Set position dodge argument
    pd <- position_dodge(0.1)
  
    
    # Generate plot of least square means
    p <- lsm %>%
      mutate(redcap_event_name = factor(redcap_event_name, levels = c("Pre", "Post")),
             randomization = recode(randomization, "Waitlist" = "Control")) %>%
      ggplot(.,aes(x = redcap_event_name, y = Estimate, color = randomization, group = randomization)) +
      geom_errorbar(aes(ymin=Estimate-StdErr, ymax=Estimate+StdErr), width=.15, position = pd) +
      geom_point(position = pd) +
      geom_line(position = pd) +
      theme_minimal() +
      theme(axis.line = element_line(color = "grey70")) +
      theme(legend.position = "top") +
      scale_colour_brewer(palette = "Set1") +
      labs(color = NULL,
           title = title,
           #caption = str_wrap(cap, 100),
           x = NULL,
           y ="Score") +
      theme(plot.caption = element_text(hjust = 0))
    
    ## Set shaded regions for EE ----
    # if (var == "mbi_ee_sum"){
    #   p <-
    #     p +
    #     annotate('rect', xmin=-Inf, xmax=Inf, ymin=30, ymax=Inf, alpha=.2, fill='grey70') +
    #     annotate(geom = "text", x = 0.5, y = 32, label = "High", size = 4, angle = 0)
    #   #return(p)
    # }
  
    # Set shaded region for DP ----
    # if (var == "mbi_dp_sum"){
    #   p <-
    #     p +
    #     annotate('rect', xmin=-Inf, xmax=Inf, ymin=13, ymax=20, alpha=.2, fill='grey70') +
    #     annotate(geom = "text", x = 0.5, y = 17.5, label = "High", size = 4, angle = 0)
    #   #return(p)
    # }
  
  
    # Set shaded regions for PA ----
    # if (var == "mbi_pa_sum"){
    #   p <-
    #     p +
    #     annotate('rect', xmin=-Inf, xmax=Inf, ymin=-Inf, ymax=33, alpha=.2, fill='grey70')
    #   #return(p)
    # }
  
    # Set shaded regions for MISS ----
    # if (var == "misshp_tot"){
    #   p <-
    #     p +
    #     annotate('rect', xmin=-Inf, xmax=Inf, ymin=35, ymax=Inf, alpha=.2, fill='grey70') +
    #     annotate(geom = "text", x = 0.6, y = 37.5, label = "MI Present", size = 4, angle = 0)
    #   #return(p)
    # }
  
      # Set shaded regions for YISS ----
    # if (var == "yissum"){
    #   p <-
    #     p +
    #     annotate('rect', xmin=-Inf, xmax=Inf, ymin=5, ymax=6, alpha=.2, fill='grey70') +
    #     annotate(geom = "text", x = 0.6, y = 5.5, label = "IS Present", size = 4, angle = 0)
    #   #return(p)
    # }
  
  
    # Set shaded regions for SCSSF need three, low, mid high
    # if (var == "scssf_sum_tot"){
    #   p <-
    #     p +
    #     annotate('rect', xmin=-Inf, xmax=Inf, ymin=-Inf, ymax=30, alpha=.2, fill='red') +
    #     annotate('rect', xmin=-Inf, xmax=Inf, ymin=30, ymax=42, alpha=.2, fill='grey80') +
    #     annotate('rect', xmin=-Inf, xmax=Inf, ymin=42, ymax=Inf, alpha=.2, fill='blue')
    #    return(p)
    # }
    
    # Tables -------------------------------------------------------------------
    
    
    
    
    return(list(solution, type3fx, lsmeans, p, deltas))
    }
}
