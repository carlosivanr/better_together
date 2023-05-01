# Load libraries ---------------------------------------------------------------
pacman::p_load(REDCapR,
               here,
               Hmisc,
               tidyverse,
               magrittr)


# Load project data ------------------------------------------------------------
source(here("scripts", "functions", "get_redcap_data.R"))
project_id <- 26979
data <- get_redcap_data(project_id)

# Gender Identity --------------------------------------------------------------

# KB: checking original frequencies of this
with(data, table(gender_identity))

# One person reported Transgender in gender_identity, change their data values
# to preserve anonymity
data %<>%
  mutate(gender_identity = ifelse(gender_identity %in% c(3,4,5), NA, gender_identity))

# Remove Rows where there are no randomization events --------------------------
data %<>% 
  filter(!identifier %in%
                            (data %>%
                              group_by(identifier) %>%
                              count() %>%
                              filter(n != 2) %>%
                              pull(identifier))
         )

# Fill in randomization variables ----------------------------------------------
# Fill in from event name randomization_arm_1 into pre_arm_1
# Only works if the value to be filled in is an NA. Therefore, if randomization
# is NA, then set randomization_complete to NA.
data %<>%
  mutate(randomization_complete = ifelse(is.na(randomization), NA, randomization_complete)) %>%
  group_by(identifier) %>% 
  fill(randomization_complete, .direction = "up") %>%
  fill(randomization, .direction = "up") %>%
  ungroup()
 

# KB: Filter out the rows for randomization_arm_1 ------------------------------

data <- data %>%
  filter(redcap_event_name != "randomization_arm_1")


# KB: Calculate instrument scores - MBI ----------------------------------------

# Note: option "na.rm=TRUE" allows missing items but excludes them from the mean. 
# If we don't want to allow missing items (set subscale score to NA if any item is NA), then use na.rm=FALSE

data2 <- data %>%
  mutate(
    
    ## Maslach Burnout Inventory (MBI)
    # Emotional Exhaustion (EE) subscale - items 1, 2, 3, 6, 8, 13, 14, 16, 20
    mbi_ee = rowMeans(across(num_range("mbi", c(1:3, 6, 8, 13, 14, 16, 20))), na.rm=TRUE), # currently this allows missing items
    # Depersonalization (DP) subscale - items 5, 10, 11, 15, 22
    mbi_dp = rowMeans(across(num_range("mbi", c(5, 10, 11, 15, 22))), na.rm=TRUE),
    # Personal Accomplishment (PA) subscale - items 4, 7, 9, 12, 17, 18, 19, 21
    mbi_pa = rowMeans(across(num_range("mbi", c(4, 7, 9, 12, 17:19, 21))), na.rm=TRUE),
    
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
    scssf_kind = rowMeans(across(num_range("scssfnew", c(2, 6))), na.rm=FALSE), # For now set to FALSE so subscale score not calculated if any item is missing
    scssf_judg = rowMeans(across(num_range("scssfnew", c(11, 12))), na.rm=FALSE),
    scssf_human = rowMeans(across(num_range("scssfnew", c(5, 10))), na.rm=FALSE),
    scssf_isol = rowMeans(across(num_range("scssfnew", c(4, 8))), na.rm=FALSE),
    scssf_mind = rowMeans(across(num_range("scssfnew", c(3, 7))), na.rm=FALSE),
    scssf_ident = rowMeans(across(num_range("scssfnew", c(1, 9))), na.rm=FALSE),
    scssf_tot = rowMeans(across(starts_with("scssf_")), na.rm=FALSE), # For now I set this to FALSE so we don't allow a missing subscale score for the total
      # at least one participant (110) only answered one item (scs1) so if we allow any number of missing items, their over-identification and total scores are both based on that one item 
      # TO DO: we should decide our own rules for missing items because the scoring instructions don't say 
      # (and each subscale is only 2 items so if either is missing then the subscale is one item)
    
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
# %>%
#   select(!(starts_with("scsnew"), starts_with("missnew")))

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
  print(n = 23)

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
 


# # Address any missingness -------------------------------------------------------
# clean_subscale_means <- function(data, subscale_columns, subscale_name){
#   # Get the ids of those with NAs in the subscale columns
#   #print(subscale_columns)
#   
#   ids_w_na <-
#   data %>%
#   select(identifier, all_of(subscale_columns)) %>%
#   mutate(N_nas = rowSums(is.na(.))) %>%
#   select(identifier, N_nas) %>%
#   filter(N_nas >= 1) %>%
#   pull(identifier)
#   
#   #Set the subscale mean to NA if its in the list of Ids
#   data %<>%
#     mutate({{subscale_name}} := ifelse(identifier %in% ids_w_na, NA, {{subscale_name}}))
#   
#   return(data)
#   }
# 
# ## MBI ----
# # Emotional Exhaustion: sum of items 1, 2, 3, 6, 8, 13, 14, 16, 20
# # Get the identifiers that NAs in the emotional exhaustion subscale
# mbi_ee <- c("mbi1", "mbi2", "mbi3", "mbi6", "mbi8", "mbi13", "mbi14", "mbi16", "mbi20")
# data <- clean_subscale_means(data, mbi_ee, "mbieemean")
# 
# # Depersonalization: sum of items 4, 7, 9, 12, 17, 18, 19, 21
# mbi_dp <- c("mbi4", "mbi7", "mbi9", "mbi12", "mbi17", "mbi18", "mbi19", "mbi21")
# data <- clean_subscale_means(data, mbi_dp, "mbidpmean")
# 
# # Personal accomplishment: sum of items 5, 10, 11, 15, 22
# mbi_pa <- c("mbi5", "mbi10", "mbi11", "mbi15", "mbi22")
# data <- clean_subscale_means(data, mbi_pa, "mbipamean")
# 
# # SCS
# 
# # YIS
# 
# # MISS
# 
# # SFI
# 
# # UCLA
# 
# ## SCS -------------------------------------------------------------------------
# ### LEFT OFF HERE ######################
# ### Need to identify which values go into the calculation of mean, and so set an 
# # NA if there are any missing values
# # Need to also change what file this script writes to, and possibly rename
# # this script to indicate it's only for the data at time point 1.
# # 
# scsoveridentificationmean
# # calc scs1, scs9, reverse coded 
# 
# 
# selfjudgmentmean
# # calc scs11, scs12, reverse coded
# 
# scsisolationscoremean
# # calc scs4, scs8, reverse coded
# 
# 
# scsselfkindnessmean
# # calc scs2, scs6
# 
# scscommonhumanitymean
# # calc scs5, scs10
# 
# 
# scsmindfulnessmean
# # calc scs3, scs7
# 
# #Total
# scs_total <- names(data %>% select(scsoveridentificationmean:scsmindfulnessmean))
# data <- clean_subscale_means(data, scs_total, "scstotalmean")



# RedCap data processing -------------------------------------------------------

# Setting Labels

label(data$identifier)="Identifier"
label(data$redcap_event_name)="Event Name"
#label(data$redcap_survey_identifier)="Survey Identifier"
#label(data$consent_timestamp)="Survey Timestamp"
label(data$first_name)="First Name"
label(data$last_name)="Last Name"
label(data$work_email)="What is your work email address?"
label(data$enrollment_agreement)="I agree to participate in this research and to provide my electronic signature."
label(data$enrollment_signature)="Please sign your name to indicate you provide consent to participate in this study."
label(data$consent_complete)="Complete?"
#label(data$demographics_timestamp)="Survey Timestamp"
label(data$firstname)="First Name"
label(data$age)="What is your age?"
label(data$years_since_training)="How many years have you been in clinical practice since finishing your clinical training? "
label(data$clinic)="What clinic do you practice at?"
label(data$years_at_clinic)="How many years have you been at this clinical practice?"
label(data$bhc)="Are you a behavioral health clinician? "
label(data$specialty)="What is your specialty?"
label(data$specialty_other)="Other specialty:"
label(data$department_randomization)="For randomization purposes, please select the group you belong to:"
label(data$degree)="What is your degree?"
label(data$degree_other)="Other degree:"
label(data$gender_identity)="What is your current gender identity?"
label(data$gender_self_describe)="Self-describe gender identity:"
label(data$gender_randomization)="For randomization purposes please select the group containing the gender identity you most closely identify with:"
label(data$race_ethnicity___1)="American Indian, Alaska Native or First Nations"
label(data$race_ethnicity___2)="Asian"
label(data$race_ethnicity___3)="Black or African American"
label(data$race_ethnicity___4)="Hispanic or Latinx"
label(data$race_ethnicity___5)="Native Hawaiian and Pacific Islander"
label(data$race_ethnicity___6)="Middle Eastern or North African"
label(data$race_ethnicity___7)="White"
label(data$race_ethnicity___8)="Prefer to self-describe"
label(data$race_ethnicity___9)="Prefer not to answer"
label(data$race_ethn_self_describe)="Self-describe racial/ethnic identity:"
label(data$sexual_orientation___1)="Asexual"
label(data$sexual_orientation___2)="Bisexual"
label(data$sexual_orientation___3)="Gay or Lesbian"
label(data$sexual_orientation___4)="Heterosexual or Straight"
label(data$sexual_orientation___5)="Pansexual"
label(data$sexual_orientation___6)="Queer"
label(data$sexual_orientation___7)="Prefer to self-describe"
label(data$sexual_orientation___8)="Prefer not to answer"
label(data$sex_orien_self_describe)="Self-describe sexual orientation:"
label(data$marital_status)="What is your marital status?"
label(data$living_situation___0)="Alone"
label(data$living_situation___1)="Significant Other/Spouse/Partner"
label(data$living_situation___2)="Your Children"
label(data$living_situation___3)="Your Siblings"
label(data$living_situation___4)="Your Parents/In-laws"
label(data$living_situation___5)="Extended Family"
label(data$living_situation___6)="Roommates"
label(data$living_situation___7)="Friends"
label(data$living_situation___8)="Other"
label(data$living_situation___9)="Prefer not to answer"
label(data$caregiving___0)="Not a caregiver"
label(data$caregiving___1)="Children"
label(data$caregiving___2)="Adult (i.e. parents/grandparents)"
label(data$caregiving___3)="Other"
label(data$caregiving___4)="Prefer not to answer"
label(data$clinical_fte)="What is your current clinical FTE? "
label(data$total_fte)="What is your current total FTE?"
label(data$demographics_complete)="Complete?"
#label(data$better_together_survey_timestamp)="Survey Timestamp"
label(data$program_support___0)="In what ways, if any, do you feel supported by your department and/or health system (check all that apply):  (choice=None)"
label(data$program_support___1)="In what ways, if any, do you feel supported by your department and/or health system (check all that apply):  (choice=Department Leadership)"
label(data$program_support___2)="In what ways, if any, do you feel supported by your department and/or health system (check all that apply):  (choice=SOM Leadership)"
label(data$program_support___3)="In what ways, if any, do you feel supported by your department and/or health system (check all that apply):  (choice=Colleagues and Staff)"
label(data$program_support___4)="In what ways, if any, do you feel supported by your department and/or health system (check all that apply):  (choice=UC Health)"
label(data$program_support___5)="In what ways, if any, do you feel supported by your department and/or health system (check all that apply):  (choice=Clinic Leadership)"
label(data$program_support___6)="In what ways, if any, do you feel supported by your department and/or health system (check all that apply):  (choice=Mental Health Resources)"
label(data$program_support___7)="In what ways, if any, do you feel supported by your department and/or health system (check all that apply):  (choice=Educational/Learning Resources)"
label(data$intent_to_leave)="What is the likelihood that you will leave your current position within two years?"
label(data$intent_to_leave_why)="Why or why not?"
label(data$mbi1)="I feel emotionally drained from my work"
label(data$mbi2)="I feel used up at the end of the workday"
label(data$mbi3)="I feel fatigued when I get up in the morning and have to face another day at work"
label(data$mbi4)="I can easily understand how my patients feel about things"
label(data$mbi5)="I feel I treat some patients as if they were impersonal objects"
label(data$mbi6)="Working with people all day is really a strain for me"
label(data$mbi7)="I deal very effectively with the problems of my patients"
label(data$mbi8)="I feel burned out from my work"
label(data$mbi9)="I feel I'm positively influencing other people's lives through my work"
label(data$mbi10)="I've become more callous towards patients since I started this job"
label(data$mbi11)="I worry that this job is hardening me emotionally"
label(data$mbi12)="I feel very energetic"
label(data$mbi13)="I feel frustrated by my job"
label(data$mbi14)="I feel Im working too hard at my job"
label(data$mbi15)="I don't really care what happens to some patients"
label(data$mbi16)="Working with people directly puts too much stress on me"
label(data$mbi17)="I can easily create a relaxed atmosphere with my patients"
label(data$mbi18)="I feel exhilarated after working closely with my patients"
label(data$mbi19)="I have accomplished many worthwhile things in this job"
label(data$mbi20)="I feel like I'm at the end of my rope"
label(data$mbi21)="In my work I deal with emotional problems very calmly"
label(data$mbi22)="I feel patients blame me for some of their problems"
label(data$scs1)="When I fail at something important to me, I become consumed by feelings of inadequacy."
label(data$scs2)="I try to be understanding and patient towards those aspects of my personality I don't like."
label(data$scs3)="When something painful happens I try to take a balanced view of the situation."
label(data$scs4)="When I'm feeling down, I tend to feel like most other people are probably happier than I am."
label(data$scs5)="I try to see my failings as part of the human condition."
label(data$scs6)="When I'm going through a very hard time, I give myself the caring and tenderness I need."
label(data$scs7)="When something upsets me I try to keep my emotions in balance."
label(data$scs8)="When I fail at something that's important to me, I tend to feel alone in my failure."
label(data$scs9)="When I'm feeling down I tend to obsess and fixate on everything that's wrong."
label(data$scs10)="When I feel inadequate in some way, I try to remind myself that feelings of inadequacy are shared by most people."
label(data$scs11)="I'm disapproving and judgmental about my own flaws and inadequacies."
label(data$scs12)="I'm intolerant and impatient towards those aspects of my personality I don't like."
label(data$yis1)="Do you secretly worry that others will find out you're not as bright and capable as they think you are?"
label(data$yis2)="Do you sometimes shy away from challenges because of nagging self doubt?"
label(data$yis3)="Do you tend to chalk your accomplishments up to being a 'fluke', 'no big deal' or the fact that people just 'like' you?"
label(data$yis4)="Do you hate making a mistake, not being fully prepared or not doing things perfectly?"
label(data$yis5)="Do you tend to feel crushed by constructive criticism, seeing it as evidence of your ineptness?"
label(data$yis6)="When you do succeed, do you think 'Phew! I fooled them this time, but may not be so lucky next time'?"
label(data$yis7)="Do you believe that your peers are smarter and more capable than you?"
label(data$yis8)="Do you live in fear of being found out, discovered or unmasked?"
label(data$miss1)="I feel betrayed by other health professionals whom I once trusted."
label(data$miss2)="I feel guilt over failing to save someone from being seriously injured or dying."
label(data$miss3)="I feel ashamed about what I've done or not done when providing care to my patients."
label(data$miss4)="I am troubled by having acted in ways that violated my own morals or values."
label(data$miss5)="Most people with whom I work as a health professional are trustworthy."
label(data$miss6)="I have a good sense of what makes my life meaningful as a health professional."
label(data$miss7)="I have forgiven myself for whats happened to me or to others whom I have cared for."
label(data$miss8)="All in all, I am inclined to feel that I'm a failure in my work as a health professional."
label(data$miss9)="I sometimes feel I am being punished for what I've done or not done while caring for patients."
label(data$miss10)="Compared to before I went through these experiences, my religious/spiritual faith has strengthened."
label(data$sfi1)="Overall, how satisfied are you with life as a whole these days?"
label(data$sfi2)="In general, how happy or unhappy do you usually feel?"
label(data$sfi3)="In general, how would you rate your physical health?"
label(data$sfi4)="How would you rate your overall mental health?"
label(data$sfi5)="Overall, to what extent do you feel the things you do in your life are worthwhile?"
label(data$sfi6)="I understand my purpose in life."
label(data$sfi7)="I always act to promote good in all circumstances, even in difficult and challenging situations."
label(data$sfi8)="I am always able to give up some happiness now for greater happiness later."
label(data$sfi9)="I am content with my friendships and relationships."
label(data$sfi10)="My relationships are as satisfying as I would want them to be."
label(data$sfi11)="How often do you worry about being able to meet normal monthly living expenses?"
label(data$sfi12)="How often do you worry about safety, food, or housing?"
label(data$ls_lack_companionship)="How often do you feel that you lack companionship?"
label(data$ls_left_out)="How often do you feel left out?"
label(data$ls_isolated)="How often do you feel isolated from others?"
label(data$better_together_survey_complete)="Complete?"
#label(data$post_test_additional_questions_timestamp)="Survey Timestamp"
label(data$cohort)="Which cohort are you in  (i.e., during which months were you offered coaching)?"
label(data$workbook)="How often did you read or write in your online workbook?"
label(data$recorded_calls)="How often did you watch recorded calls?"
label(data$webinars)="How often did you watch the weekly webinars?"
label(data$live_coaching)="How often did you join the live coaching calls (regardless of if you got coached)?"
label(data$coached)="How many times did you get coached on Zoom?"
label(data$pe_num_sessions)="Better Together had an appropriate number of sessions."
label(data$pe_quality)="The Better Together facilitators provided a quality experience."
label(data$pe_wellness)="Participating in Better Together supported my wellness."
label(data$pe_colleagues)="I was positively impacted by the presence of my colleagues in these coaching sessions."
label(data$pe_recommend)="I would recommend the Better Together Program to others."
label(data$barriers)="Please tell us about any barriers or challenges that made participation challenging:"
label(data$post_test_additional_questions_complete)="Complete?"
label(data$randomization)="Randomization group"
label(data$randomization_complete)="Complete?"

# KB ADDED these for the new calculated instrument scores
label(data$mbi_ee) = "MBI: Emotional Exhaustion subscale score"
label(data$mbi_dp) = "MBI: Depersonalization subscale score"
label(data$mbi_pa) = "MBI: Personal Accomplishment subscale score"
label(data$scssf_kind) = "SCS-SF: Self-kindness subscale score"
label(data$scssf_judg) = "SCS-SF: Self-judgment subscale score"
label(data$scssf_human) = "SCS-SF: Common humanity subscale score"
label(data$scssf_isol) = "SCS-SF: Isolation subscale score"
label(data$scssf_mind) = "SCS-SF: Mindfulness subscale score"
label(data$scssf_ident) = "SCS-SF: Over-identification subscale score"
label(data$scssf_tot) = "SCS-SF: Self-compassion total score"
label(data$misshp_tot) = "MISS-HP: Moral injury (healthcare professionals) total score"
label(data$yis_count) = "YIS: Sum score (count of 'Yes' responses)"
label(data$yis_ispos) = "YIS: Impostor syndrome present"
label(data$ls3_tot) = "3-Item Loneliness Scale: Total (sum) score"
label(data$ls3_lonely) = "3-Item Loneliness Scale: Lonely"
label(data$sfi_d1) = "SFI/FI: D1 (Happiness and life satisfaction domain) score"
label(data$sfi_d2) = "SFI/FI: D2 (Mental and physical health domain) score"
label(data$sfi_d3) = "SFI/FI: D3 (Meaning and purpose domain) score"
label(data$sfi_d4) = "SFI/FI: D4 (Character and virtue domain) score"
label(data$sfi_d5) = "SFI/FI: D5 (Close social relationships domain) score"
label(data$sfi_d6) = "SFI: D6 (Financial and material stability domain) score"
label(data$sfi_tot) = "SFI: Total score"
label(data$fi_tot) = "FI: Total score"

# Setting Units


# Setting Factors (will create new variable for factors)

data$redcap_event_name.factor = factor(data$redcap_event_name,levels=c("pre_arm_1","post_arm_1","randomization_arm_1"))
data$enrollment_agreement.factor = factor(data$enrollment_agreement,levels=c("1","0"))
data$consent_complete.factor = factor(data$consent_complete,levels=c("0","1","2"))
data$bhc.factor = factor(data$bhc,levels=c("1","2","3"))
data$specialty.factor = factor(data$specialty,levels=c("1","2","3","4","5","6","7","8","9","10"))
data$department_randomization.factor = factor(data$department_randomization,levels=c("1","2","3","4"))
data$degree.factor = factor(data$degree,levels=c("1","2","3","4","5"))
data$gender_identity.factor = factor(data$gender_identity,levels=c("1","2","3","4","5","6","7"))
data$gender_randomization.factor = factor(data$gender_randomization,levels=c("1","2","3"))
data$race_ethnicity___1.factor = factor(data$race_ethnicity___1,levels=c("0","1"))
data$race_ethnicity___2.factor = factor(data$race_ethnicity___2,levels=c("0","1"))
data$race_ethnicity___3.factor = factor(data$race_ethnicity___3,levels=c("0","1"))
data$race_ethnicity___4.factor = factor(data$race_ethnicity___4,levels=c("0","1"))
data$race_ethnicity___5.factor = factor(data$race_ethnicity___5,levels=c("0","1"))
data$race_ethnicity___6.factor = factor(data$race_ethnicity___6,levels=c("0","1"))
data$race_ethnicity___7.factor = factor(data$race_ethnicity___7,levels=c("0","1"))
data$race_ethnicity___8.factor = factor(data$race_ethnicity___8,levels=c("0","1"))
data$race_ethnicity___9.factor = factor(data$race_ethnicity___9,levels=c("0","1"))
data$sexual_orientation___1.factor = factor(data$sexual_orientation___1,levels=c("0","1"))
data$sexual_orientation___2.factor = factor(data$sexual_orientation___2,levels=c("0","1"))
data$sexual_orientation___3.factor = factor(data$sexual_orientation___3,levels=c("0","1"))
data$sexual_orientation___4.factor = factor(data$sexual_orientation___4,levels=c("0","1"))
data$sexual_orientation___5.factor = factor(data$sexual_orientation___5,levels=c("0","1"))
data$sexual_orientation___6.factor = factor(data$sexual_orientation___6,levels=c("0","1"))
data$sexual_orientation___7.factor = factor(data$sexual_orientation___7,levels=c("0","1"))
data$sexual_orientation___8.factor = factor(data$sexual_orientation___8,levels=c("0","1"))
data$marital_status.factor = factor(data$marital_status,levels=c("0","1","2","3","4","5","6"))
data$living_situation___0.factor = factor(data$living_situation___0,levels=c("0","1"))
data$living_situation___1.factor = factor(data$living_situation___1,levels=c("0","1"))
data$living_situation___2.factor = factor(data$living_situation___2,levels=c("0","1"))
data$living_situation___3.factor = factor(data$living_situation___3,levels=c("0","1"))
data$living_situation___4.factor = factor(data$living_situation___4,levels=c("0","1"))
data$living_situation___5.factor = factor(data$living_situation___5,levels=c("0","1"))
data$living_situation___6.factor = factor(data$living_situation___6,levels=c("0","1"))
data$living_situation___7.factor = factor(data$living_situation___7,levels=c("0","1"))
data$living_situation___8.factor = factor(data$living_situation___8,levels=c("0","1"))
data$living_situation___9.factor = factor(data$living_situation___9,levels=c("0","1"))
data$caregiving___0.factor = factor(data$caregiving___0,levels=c("0","1"))
data$caregiving___1.factor = factor(data$caregiving___1,levels=c("0","1"))
data$caregiving___2.factor = factor(data$caregiving___2,levels=c("0","1"))
data$caregiving___3.factor = factor(data$caregiving___3,levels=c("0","1"))
data$caregiving___4.factor = factor(data$caregiving___4,levels=c("0","1"))
data$clinical_fte.factor = factor(data$clinical_fte,levels=c("1","2","3","4"))
data$total_fte.factor = factor(data$total_fte,levels=c("1","2","3","4"))
data$demographics_complete.factor = factor(data$demographics_complete,levels=c("0","1","2"))
data$program_support___0.factor = factor(data$program_support___0,levels=c("0","1"))
data$program_support___1.factor = factor(data$program_support___1,levels=c("0","1"))
data$program_support___2.factor = factor(data$program_support___2,levels=c("0","1"))
data$program_support___3.factor = factor(data$program_support___3,levels=c("0","1"))
data$program_support___4.factor = factor(data$program_support___4,levels=c("0","1"))
data$program_support___5.factor = factor(data$program_support___5,levels=c("0","1"))
data$program_support___6.factor = factor(data$program_support___6,levels=c("0","1"))
data$program_support___7.factor = factor(data$program_support___7,levels=c("0","1"))
data$intent_to_leave.factor = factor(data$intent_to_leave,levels=c("0","1","2","3","4"))
data$mbi1.factor = factor(data$mbi1,levels=c("0","1","2","3","4","5","6"))
data$mbi2.factor = factor(data$mbi2,levels=c("0","1","2","3","4","5","6"))
data$mbi3.factor = factor(data$mbi3,levels=c("0","1","2","3","4","5","6"))
data$mbi4.factor = factor(data$mbi4,levels=c("0","1","2","3","4","5","6"))
data$mbi5.factor = factor(data$mbi5,levels=c("0","1","2","3","4","5","6"))
data$mbi6.factor = factor(data$mbi6,levels=c("0","1","2","3","4","5","6"))
data$mbi7.factor = factor(data$mbi7,levels=c("0","1","2","3","4","5","6"))
data$mbi8.factor = factor(data$mbi8,levels=c("0","1","2","3","4","5","6"))
data$mbi9.factor = factor(data$mbi9,levels=c("0","1","2","3","4","5","6"))
data$mbi10.factor = factor(data$mbi10,levels=c("0","1","2","3","4","5","6"))
data$mbi11.factor = factor(data$mbi11,levels=c("0","1","2","3","4","5","6"))
data$mbi12.factor = factor(data$mbi12,levels=c("0","1","2","3","4","5","6"))
data$mbi13.factor = factor(data$mbi13,levels=c("0","1","2","3","4","5","6"))
data$mbi14.factor = factor(data$mbi14,levels=c("0","1","2","3","4","5","6"))
data$mbi15.factor = factor(data$mbi15,levels=c("0","1","2","3","4","5","6"))
data$mbi16.factor = factor(data$mbi16,levels=c("0","1","2","3","4","5","6"))
data$mbi17.factor = factor(data$mbi17,levels=c("0","1","2","3","4","5","6"))
data$mbi18.factor = factor(data$mbi18,levels=c("0","1","2","3","4","5","6"))
data$mbi19.factor = factor(data$mbi19,levels=c("0","1","2","3","4","5","6"))
data$mbi20.factor = factor(data$mbi20,levels=c("0","1","2","3","4","5","6"))
data$mbi21.factor = factor(data$mbi21,levels=c("0","1","2","3","4","5","6"))
data$mbi22.factor = factor(data$mbi22,levels=c("0","1","2","3","4","5","6"))
data$scs1.factor = factor(data$scs1,levels=c("1","2","3","4","5"))
data$scs2.factor = factor(data$scs2,levels=c("1","2","3","4","5"))
data$scs3.factor = factor(data$scs3,levels=c("1","2","3","4","5"))
data$scs4.factor = factor(data$scs4,levels=c("1","2","3","4","5"))
data$scs5.factor = factor(data$scs5,levels=c("1","2","3","4","5"))
data$scs6.factor = factor(data$scs6,levels=c("1","2","3","4","5"))
data$scs7.factor = factor(data$scs7,levels=c("1","2","3","4","5"))
data$scs8.factor = factor(data$scs8,levels=c("1","2","3","4","5"))
data$scs9.factor = factor(data$scs9,levels=c("1","2","3","4","5"))
data$scs10.factor = factor(data$scs10,levels=c("1","2","3","4","5"))
data$scs11.factor = factor(data$scs11,levels=c("1","2","3","4","5"))
data$scs12.factor = factor(data$scs12,levels=c("1","2","3","4","5"))
data$yis1.factor = factor(data$yis1,levels=c("1","0"))
data$yis2.factor = factor(data$yis2,levels=c("1","0"))
data$yis3.factor = factor(data$yis3,levels=c("1","0"))
data$yis4.factor = factor(data$yis4,levels=c("1","0"))
data$yis5.factor = factor(data$yis5,levels=c("1","0"))
data$yis6.factor = factor(data$yis6,levels=c("1","0"))
data$yis7.factor = factor(data$yis7,levels=c("1","0"))
data$yis8.factor = factor(data$yis8,levels=c("1","0"))
data$miss1.factor = factor(data$miss1,levels=c("1","2","3","4","5","6","7","8","9","10"))
data$miss2.factor = factor(data$miss2,levels=c("1","2","3","4","5","6","7","8","9","10"))
data$miss3.factor = factor(data$miss3,levels=c("1","2","3","4","5","6","7","8","9","10"))
data$miss4.factor = factor(data$miss4,levels=c("1","2","3","4","5","6","7","8","9","10"))
data$miss5.factor = factor(data$miss5,levels=c("1","2","3","4","5","6","7","8","9","10"))
data$miss6.factor = factor(data$miss6,levels=c("1","2","3","4","5","6","7","8","9","10"))
data$miss7.factor = factor(data$miss7,levels=c("1","2","3","4","5","6","7","8","9","10"))
data$miss8.factor = factor(data$miss8,levels=c("1","2","3","4","5","6","7","8","9","10"))
data$miss9.factor = factor(data$miss9,levels=c("1","2","3","4","5","6","7","8","9","10"))
data$miss10.factor = factor(data$miss10,levels=c("1","2","3","4","5","6","7","8","9","10"))
data$ls_lack_companionship.factor = factor(data$ls_lack_companionship,levels=c("1","2","3"))
data$ls_left_out.factor = factor(data$ls_left_out,levels=c("1","2","3"))
data$ls_isolated.factor = factor(data$ls_isolated,levels=c("1","2","3"))
data$better_together_survey_complete.factor = factor(data$better_together_survey_complete,levels=c("0","1","2"))
data$cohort.factor = factor(data$cohort,levels=c("1","2"))
data$workbook.factor = factor(data$workbook,levels=c("0","1","2","3","4","5","6"))
data$recorded_calls.factor = factor(data$recorded_calls,levels=c("0","1","2","3","4","5","6"))
data$webinars.factor = factor(data$webinars,levels=c("0","1","2","3","4","5","6"))
data$live_coaching.factor = factor(data$live_coaching,levels=c("0","1","2","3","4","5","6"))
data$coached.factor = factor(data$coached,levels=c("0","1","2","3","4","5","6"))
data$pe_num_sessions.factor = factor(data$pe_num_sessions,levels=c("1","2","3","4","5"))
data$pe_quality.factor = factor(data$pe_quality,levels=c("1","2","3","4","5"))
data$pe_wellness.factor = factor(data$pe_wellness,levels=c("1","2","3","4","5"))
data$pe_colleagues.factor = factor(data$pe_colleagues,levels=c("1","2","3","4","5"))
data$pe_recommend.factor = factor(data$pe_recommend,levels=c("1","2","3","4","5"))
data$post_test_additional_questions_complete.factor = factor(data$post_test_additional_questions_complete,levels=c("0","1","2"))
data$randomization.factor = factor(data$randomization,levels=c("0","1"))
data$randomization_complete.factor = factor(data$randomization_complete,levels=c("0","1","2"))

data$yis_ispos.factor = factor(data$yis_ispos,levels=c("0","1")) # KB ADDED - create factor variable for dichotomous impostor syndrome present/absent
label(data$yis_ispos.factor) = "YIS: Impostor syndrome present"

data$ls3_lonely.factor = factor(data$ls3_lonely,levels=c("0","1")) # KB ADDED - create factor variable for dichotomous lonely/not lonely
label(data$ls3_lonely.factor) = "3-Item Loneliness Scale: Lonely"

levels(data$redcap_event_name.factor)=c("Pre","Post","Randomization")
levels(data$enrollment_agreement.factor)=c("Yes","No")
levels(data$consent_complete.factor)=c("Incomplete","Unverified","Complete")
levels(data$bhc.factor)=c("Yes","No","Prefer not to answer")
levels(data$specialty.factor)=c("Behavioral Health","Family Medicine","Geriatrics","General Internal Medicine","Hospital Internal Medicine","Palliative Care","Pediatrics","Physical Medicine and Rehab","Other","Prefer not to answer")
levels(data$department_randomization.factor)=c("Pediatrics","Internal Medicine/Physical Medicine & Rehabilitation","Family Medicine","Other")
levels(data$degree.factor)=c("MD or DO","PhD","PsyD","Other","Prefer not to answer")
levels(data$gender_identity.factor)=c("Cis female","Cis male","Non-binary/third gender","Trans female","Trans male","Prefer to self-describe","Prefer not to answer")
levels(data$gender_randomization.factor)=c("Female (if you selected cis female or trans female above)","Male (if you selected cis male or trans male above)","Another gender identity (if you selected something other than cis/trans female or cis/trans male above)")
levels(data$race_ethnicity___1.factor)=c("Unchecked","Checked")
levels(data$race_ethnicity___2.factor)=c("Unchecked","Checked")
levels(data$race_ethnicity___3.factor)=c("Unchecked","Checked")
levels(data$race_ethnicity___4.factor)=c("Unchecked","Checked")
levels(data$race_ethnicity___5.factor)=c("Unchecked","Checked")
levels(data$race_ethnicity___6.factor)=c("Unchecked","Checked")
levels(data$race_ethnicity___7.factor)=c("Unchecked","Checked")
levels(data$race_ethnicity___8.factor)=c("Unchecked","Checked")
levels(data$race_ethnicity___9.factor)=c("Unchecked","Checked")
levels(data$sexual_orientation___1.factor)=c("Unchecked","Checked")
levels(data$sexual_orientation___2.factor)=c("Unchecked","Checked")
levels(data$sexual_orientation___3.factor)=c("Unchecked","Checked")
levels(data$sexual_orientation___4.factor)=c("Unchecked","Checked")
levels(data$sexual_orientation___5.factor)=c("Unchecked","Checked")
levels(data$sexual_orientation___6.factor)=c("Unchecked","Checked")
levels(data$sexual_orientation___7.factor)=c("Unchecked","Checked")
levels(data$sexual_orientation___8.factor)=c("Unchecked","Checked")
levels(data$marital_status.factor)=c("Single","Married","Engaged","Divorced","Partnered","Other","Prefer not to answer")
levels(data$living_situation___0.factor)=c("Unchecked","Checked")
levels(data$living_situation___1.factor)=c("Unchecked","Checked")
levels(data$living_situation___2.factor)=c("Unchecked","Checked")
levels(data$living_situation___3.factor)=c("Unchecked","Checked")
levels(data$living_situation___4.factor)=c("Unchecked","Checked")
levels(data$living_situation___5.factor)=c("Unchecked","Checked")
levels(data$living_situation___6.factor)=c("Unchecked","Checked")
levels(data$living_situation___7.factor)=c("Unchecked","Checked")
levels(data$living_situation___8.factor)=c("Unchecked","Checked")
levels(data$living_situation___9.factor)=c("Unchecked","Checked")
levels(data$caregiving___0.factor)=c("Unchecked","Checked")
levels(data$caregiving___1.factor)=c("Unchecked","Checked")
levels(data$caregiving___2.factor)=c("Unchecked","Checked")
levels(data$caregiving___3.factor)=c("Unchecked","Checked")
levels(data$caregiving___4.factor)=c("Unchecked","Checked")
levels(data$clinical_fte.factor)=c("0-25%","26-50%","51-75%",">76%")
levels(data$total_fte.factor)=c("0-25%","26-50%","51-75%",">76%")
levels(data$demographics_complete.factor)=c("Incomplete","Unverified","Complete")
levels(data$program_support___0.factor)=c("Unchecked","Checked")
levels(data$program_support___1.factor)=c("Unchecked","Checked")
levels(data$program_support___2.factor)=c("Unchecked","Checked")
levels(data$program_support___3.factor)=c("Unchecked","Checked")
levels(data$program_support___4.factor)=c("Unchecked","Checked")
levels(data$program_support___5.factor)=c("Unchecked","Checked")
levels(data$program_support___6.factor)=c("Unchecked","Checked")
levels(data$program_support___7.factor)=c("Unchecked","Checked")
levels(data$intent_to_leave.factor)=c("None","Slight","Moderate","Likely","Definitely")
levels(data$mbi1.factor)=c("Never","A few times per year or less","Once a month or less","A few times a month","Once a week","A few times a week","Every day")
levels(data$mbi2.factor)=c("Never","A few times per year or less","Once a month or less","A few times a month","Once a week","A few times a week","Every day")
levels(data$mbi3.factor)=c("Never","A few times per year or less","Once a month or less","A few times a month","Once a week","A few times a week","Every day")
levels(data$mbi4.factor)=c("Never","A few times per year or less","Once a month or less","A few times a month","Once a week","A few times a week","Every day")
levels(data$mbi5.factor)=c("Never","A few times per year or less","Once a month or less","A few times a month","Once a week","A few times a week","Every day")
levels(data$mbi6.factor)=c("Never","A few times per year or less","Once a month or less","A few times a month","Once a week","A few times a week","Every day")
levels(data$mbi7.factor)=c("Never","A few times per year or less","Once a month or less","A few times a month","Once a week","A few times a week","Every day")
levels(data$mbi8.factor)=c("Never","A few times per year or less","Once a month or less","A few times a month","Once a week","A few times a week","Every day")
levels(data$mbi9.factor)=c("Never","A few times per year or less","Once a month or less","A few times a month","Once a week","A few times a week","Every day")
levels(data$mbi10.factor)=c("Never","A few times per year or less","Once a month or less","A few times a month","Once a week","A few times a week","Every day")
levels(data$mbi11.factor)=c("Never","A few times per year or less","Once a month or less","A few times a month","Once a week","A few times a week","Every day")
levels(data$mbi12.factor)=c("Never","A few times per year or less","Once a month or less","A few times a month","Once a week","A few times a week","Every day")
levels(data$mbi13.factor)=c("Never","A few times per year or less","Once a month or less","A few times a month","Once a week","A few times a week","Every day")
levels(data$mbi14.factor)=c("Never","A few times per year or less","Once a month or less","A few times a month","Once a week","A few times a week","Every day")
levels(data$mbi15.factor)=c("Never","A few times per year or less","Once a month or less","A few times a month","Once a week","A few times a week","Every day")
levels(data$mbi16.factor)=c("Never","A few times per year or less","Once a month or less","A few times a month","Once a week","A few times a week","Every day")
levels(data$mbi17.factor)=c("Never","A few times per year or less","Once a month or less","A few times a month","Once a week","A few times a week","Every day")
levels(data$mbi18.factor)=c("Never","A few times per year or less","Once a month or less","A few times a month","Once a week","A few times a week","Every day")
levels(data$mbi19.factor)=c("Never","A few times per year or less","Once a month or less","A few times a month","Once a week","A few times a week","Every day")
levels(data$mbi20.factor)=c("Never","A few times per year or less","Once a month or less","A few times a month","Once a week","A few times a week","Every day")
levels(data$mbi21.factor)=c("Never","A few times per year or less","Once a month or less","A few times a month","Once a week","A few times a week","Every day")
levels(data$mbi22.factor)=c("Never","A few times per year or less","Once a month or less","A few times a month","Once a week","A few times a week","Every day")
levels(data$scs1.factor)=c("Almost never","Rarely","Sometimes","Often","Almost always")
levels(data$scs2.factor)=c("Almost never","Rarely","Sometimes","Often","Almost always")
levels(data$scs3.factor)=c("Almost never","Rarely","Sometimes","Often","Almost always")
levels(data$scs4.factor)=c("Almost never","Rarely","Sometimes","Often","Almost always")
levels(data$scs5.factor)=c("Almost never","Rarely","Sometimes","Often","Almost always")
levels(data$scs6.factor)=c("Almost never","Rarely","Sometimes","Often","Almost always")
levels(data$scs7.factor)=c("Almost never","Rarely","Sometimes","Often","Almost always")
levels(data$scs8.factor)=c("Almost never","Rarely","Sometimes","Often","Almost always")
levels(data$scs9.factor)=c("Almost never","Rarely","Sometimes","Often","Almost always")
levels(data$scs10.factor)=c("Almost never","Rarely","Sometimes","Often","Almost always")
levels(data$scs11.factor)=c("Almost never","Rarely","Sometimes","Often","Almost always")
levels(data$scs12.factor)=c("Almost never","Rarely","Sometimes","Often","Almost always")
levels(data$yis1.factor)=c("Yes","No")
levels(data$yis2.factor)=c("Yes","No")
levels(data$yis3.factor)=c("Yes","No")
levels(data$yis4.factor)=c("Yes","No")
levels(data$yis5.factor)=c("Yes","No")
levels(data$yis6.factor)=c("Yes","No")
levels(data$yis7.factor)=c("Yes","No")
levels(data$yis8.factor)=c("Yes","No")
levels(data$miss1.factor)=c("Strongly disagree  1","2","Mildly disagree  3","4","Neutral  5","6","Mildly agree  7","8","9","Strongly agree  10")
levels(data$miss2.factor)=c("Strongly disagree  1","2","Mildly disagree  3","4","Neutral  5","6","Mildly agree  7","8","9","Strongly agree  10")
levels(data$miss3.factor)=c("Strongly disagree  1","2","Mildly disagree  3","4","Neutral  5","6","Mildly agree  7","8","9","Strongly agree  10")
levels(data$miss4.factor)=c("Strongly disagree  1","2","Mildly disagree  3","4","Neutral  5","6","Mildly agree  7","8","9","Strongly agree  10")
levels(data$miss5.factor)=c("Strongly disagree  1","2","Mildly disagree  3","4","Neutral  5","6","Mildly agree  7","8","9","Strongly agree  10")
levels(data$miss6.factor)=c("Strongly disagree  1","2","Mildly disagree  3","4","Neutral  5","6","Mildly agree  7","8","9","Strongly agree  10")
levels(data$miss7.factor)=c("Strongly disagree  1","2","Mildly disagree  3","4","Neutral  5","6","Mildly agree  7","8","9","Strongly agree  10")
levels(data$miss8.factor)=c("Strongly disagree  1","2","Mildly disagree  3","4","Neutral  5","6","Mildly agree  7","8","9","Strongly agree  10")
levels(data$miss9.factor)=c("Strongly disagree  1","2","Mildly disagree  3","4","Neutral  5","6","Mildly agree  7","8","9","Strongly agree  10")
levels(data$miss10.factor)=c("Strongly disagree  1","2","Mildly disagree  3","4","Neutral  5","6","Mildly agree  7","8","9","Strongly agree  10")
levels(data$ls_lack_companionship.factor)=c("Hardly ever","Some of the time","Often")
levels(data$ls_left_out.factor)=c("Hardly ever","Some of the time","Often")
levels(data$ls_isolated.factor)=c("Hardly ever","Some of the time","Often")
levels(data$better_together_survey_complete.factor)=c("Incomplete","Unverified","Complete")
levels(data$cohort.factor)=c("February-May 2023","September-December 2023")
levels(data$workbook.factor)=c("Never","A few times in the 4 months","Monthly","A few times a month","Weekly","A few times a week","Daily")
levels(data$recorded_calls.factor)=c("Never","A few times in the 4 months","Monthly","A few times a month","Weekly","A few times a week","Daily")
levels(data$webinars.factor)=c("Never","A few times in the 4 months","Monthly","A few times a month","Weekly","A few times a week","Daily")
levels(data$live_coaching.factor)=c("Never","A few times in the 4 months","Monthly","A few times a month","Weekly","A few times a week","Daily")
levels(data$coached.factor)=c("Never","A few times in the 4 months","Monthly","A few times a month","Weekly","A few times a week","Daily")
levels(data$pe_num_sessions.factor)=c("Strongly disagree","Disagree","Neutral","Agree","Strongly agree")
levels(data$pe_quality.factor)=c("Strongly disagree","Disagree","Neutral","Agree","Strongly agree")
levels(data$pe_wellness.factor)=c("Strongly disagree","Disagree","Neutral","Agree","Strongly agree")
levels(data$pe_colleagues.factor)=c("Strongly disagree","Disagree","Neutral","Agree","Strongly agree")
levels(data$pe_recommend.factor)=c("Strongly disagree","Disagree","Neutral","Agree","Strongly agree")
levels(data$post_test_additional_questions_complete.factor)=c("Incomplete","Unverified","Complete")
levels(data$randomization.factor)=c("Waitlist","Intervention")
levels(data$randomization_complete.factor)=c("Incomplete","Unverified","Complete")

levels(data$yis_ispos.factor) = c("No","Yes") # KB ADDED - create factor variable for dichotomous impostor syndrome present/absent
levels(data$ls3_lonely.factor) = c("No","Yes") # KB ADDED - create factor variable for dichotomous lonely/not lonely

# Remove identifying information
data %<>% 
  select(-first_name, -last_name, -work_email, -firstname, -enrollment_signature)




  



# KB: missing data issues for participant 110 now already addressed above

# Participant 110
# Participant 110 does not have complete data for several instruments.
# data %>%
#   select(identifier, scs1:scs12) %>%
#   mutate(scs_N_nas = rowSums(is.na(.))) %>%
#   select(identifier, scs_N_nas) %>%
#   filter(scs_N_nas >=1)

# data %<>% mutate(across(starts_with("scs"), ~ifelse(identifier == 110, NA, .x)))



# This data should only be for the first time point. A second script should be
# set up for the second time point.
# Save data --------------------------------------------------------------------
save(data, file = here("data/pre_arm_1_prepped_data.Rdata"))
save(data, file = here("data/prepped_data.Rdata"))

# 
# # Load Data --------------------------------------------------------------------
# load(here("data", "prepped_data.Rdata"))
# 
# # Filter to the first time point and only those that are complete in randomization
# # will omit any that are unverified
# data %<>%
#   filter(redcap_event_name == "pre_arm_1") %>%
#   filter(randomization_complete.factor == "Complete")