# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Carlos Rodriguez, PhD. Dept. of Family Medicine, CU Anschutz Medical Campus
# July 21, 2023
# Better Together
# Apply Redcap processing
# Descriptiong: This script applies the Redcap processing commands to add labels
# create factored columns, and set levels.

# N.B. Many commands have been commented because not all factored columns need
# to be created. 
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


# Setting Labels ---------------------------------------------------------------
label(data$identifier)="Identifier"
label(data$redcap_event_name)="Event Name"
#label(data$redcap_survey_identifier)="Survey Identifier"
#label(data$consent_timestamp)="Survey Timestamp"
#label(data$first_name)="First Name"
label(data$last_name)="Last Name"
#label(data$work_email)="What is your work email address?"
label(data$enrollment_agreement)="I agree to participate in this research and to provide my electronic signature."
#label(data$enrollment_signature)="Please sign your name to indicate you provide consent to participate in this study."
label(data$consent_complete)="Complete?"
#label(data$demographics_timestamp)="Survey Timestamp"
#label(data$firstname)="First Name"
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
# label(data$sexual_orientation___1)="Asexual"
# label(data$sexual_orientation___2)="Bisexual"
# label(data$sexual_orientation___3)="Gay or Lesbian"
# label(data$sexual_orientation___4)="Heterosexual or Straight"
# label(data$sexual_orientation___5)="Pansexual"
# label(data$sexual_orientation___6)="Queer"
# label(data$sexual_orientation___7)="Prefer to self-describe"
# label(data$sexual_orientation___8)="Prefer not to answer"
# label(data$sex_orien_self_describe)="Self-describe sexual orientation:"
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

# label(data$workbook)="How often did you read or write in your online workbook?"
# label(data$recorded_calls)="How often did you watch recorded calls?"
# label(data$webinars)="How often did you watch the weekly webinars?"
# label(data$live_coaching)="How often did you join the live coaching calls (regardless of if you got coached)?"
# label(data$coached)="How many times did you get coached on Zoom?"
# label(data$pe_num_sessions)="Better Together had an appropriate number of sessions."
# label(data$pe_quality)="The Better Together facilitators provided a quality experience."
# label(data$pe_wellness)="Participating in Better Together supported my wellness."
# label(data$pe_colleagues)="I was positively impacted by the presence of my colleagues in these coaching sessions."
# label(data$pe_recommend)="I would recommend the Better Together Program to others."
# label(data$barriers)="Please tell us about any barriers or challenges that made participation challenging:"
# label(data$post_test_additional_questions_complete)="Complete?"
# label(data$randomization)="Randomization group"
# label(data$randomization_complete)="Complete?"

# KB ADDED these for the new calculated instrument scores
# label(data$mbi_ee_avg) = "MBI: Emotional Exhaustion subscale score (average)"
# label(data$mbi_dp_avg) = "MBI: Depersonalization subscale score (average)"
# label(data$mbi_pa_avg) = "MBI: Personal Accomplishment subscale score (average)"

# label(data$mbi_avg_ee) = "MBI: Emotional Exhaustion subscale score (average)"
# label(data$mbi_avg_dp) = "MBI: Depersonalization subscale score (average)"
# label(data$mbi_avg_pa) = "MBI: Personal Accomplishment subscale score (average)"

label(data$mbi_ee_sum) = "MBI: Emotional Exhaustion subscale score (sum)"
label(data$mbi_dp_sum) = "MBI: Depersonalization subscale score (sum)"
label(data$mbi_pa_sum) = "MBI: Personal Accomplishment subscale score (sum)"
 
# label(data$mbi_sum_ee) = "MBI: Emotional Exhaustion subscale score (sum)"
# label(data$mbi_sum_dp) = "MBI: Depersonalization subscale score (sum)"
# label(data$mbi_sum_pa) = "MBI: Personal Accomplishment subscale score (sum)"

# label(data$scssf_kind) = "SCS-SF: Self-kindness subscale score"
# label(data$scssf_judg) = "SCS-SF: Self-judgment subscale score"
# label(data$scssf_human) = "SCS-SF: Common humanity subscale score"
# label(data$scssf_isol) = "SCS-SF: Isolation subscale score"
# label(data$scssf_mind) = "SCS-SF: Mindfulness subscale score"
# label(data$scssf_ident) = "SCS-SF: Over-identification subscale score"
#label(data$scssf_tot) = "SCS-SF: Self-compassion total score (average)"

label(data$scssf_avg_kind) = "SCS-SF: Self-kindness subscale score (average)"
label(data$scssf_avg_judg) = "SCS-SF: Self-judgment subscale score (average)"
label(data$scssf_avg_human) = "SCS-SF: Common humanity subscale score (average)"
label(data$scssf_avg_isol) = "SCS-SF: Isolation subscale score (average)"
label(data$scssf_avg_mind) = "SCS-SF: Mindfulness subscale score (average)"
label(data$scssf_avg_ident) = "SCS-SF: Over-identification subscale score (average)"
label(data$scssf_avg_tot) = "SCS-SF: Self-compassion total score (average)"

# label(data$scssf_sum_kind) = "SCS-SF: Self-kindness subscale score (sum)"
# label(data$scssf_sum_judg) = "SCS-SF: Self-judgment subscale score (sum)"
# label(data$scssf_sum_human) = "SCS-SF: Common humanity subscale score (sum)"
# label(data$scssf_sum_isol) = "SCS-SF: Isolation subscale score (sum)"
# label(data$scssf_sum_mind) = "SCS-SF: Mindfulness subscale score (sum)"
# label(data$scssf_sum_ident) = "SCS-SF: Over-identification subscale score (sum)"
label(data$scssf_sum_tot) = "SCS-SF: Self-compassion total score (sum)"

label(data$misshp_tot) = "MISS-HP: Moral injury (healthcare professionals) total score"
#label(data$yis_count) = "YIS: Sum score (count of 'Yes' responses)"
#label(data$yis_ispos) = "YIS: Impostor syndrome present"
#label(data$ls3_tot) = "3-Item Loneliness Scale: Total (sum) score"
#label(data$ls3_lonely) = "3-Item Loneliness Scale: Lonely"
label(data$sfi_d1) = "SFI/FI: D1 (Happiness and life satisfaction domain) score"
label(data$sfi_d2) = "SFI/FI: D2 (Mental and physical health domain) score"
label(data$sfi_d3) = "SFI/FI: D3 (Meaning and purpose domain) score"
label(data$sfi_d4) = "SFI/FI: D4 (Character and virtue domain) score"
label(data$sfi_d5) = "SFI/FI: D5 (Close social relationships domain) score"
label(data$sfi_d6) = "SFI: D6 (Financial and material stability domain) score"
label(data$sfi_tot) = "SFI: Total score"
label(data$fi_tot) = "FI: Total score"


# Setting Factors (will create new variable for factors) -----------------------
data$redcap_event_name.factor = factor(data$redcap_event_name,levels=c("pre_arm_1","post_arm_1","randomization_arm_1"))
data$enrollment_agreement.factor = factor(data$enrollment_agreement,levels=c("1","0"))
data$consent_complete.factor = factor(data$consent_complete,levels=c("0","1","2"))
data$bhc.factor = factor(data$bhc,levels=c("1","2","3"))
data$specialty.factor = factor(data$specialty,levels=c("1","2","3","4","5","6","7","8","9","10"))
data$department_randomization.factor = factor(data$department_randomization,levels=c("1","2","3","4"))
data$degree.factor = factor(data$degree,levels=c("1","2","3","4","5"))
data$gender_identity.factor = factor(data$gender_identity,levels=c("1","2","3","4","5","6","7"))
data$gender_randomization.factor = factor(data$gender_randomization,levels=c("1","2","3"))

# data$race_ethnicity___1.factor = factor(data$race_ethnicity___1,levels=c("0","1"))
# data$race_ethnicity___2.factor = factor(data$race_ethnicity___2,levels=c("0","1"))
# data$race_ethnicity___3.factor = factor(data$race_ethnicity___3,levels=c("0","1"))
# data$race_ethnicity___4.factor = factor(data$race_ethnicity___4,levels=c("0","1"))
# data$race_ethnicity___5.factor = factor(data$race_ethnicity___5,levels=c("0","1"))
# data$race_ethnicity___6.factor = factor(data$race_ethnicity___6,levels=c("0","1"))
# data$race_ethnicity___7.factor = factor(data$race_ethnicity___7,levels=c("0","1"))
# data$race_ethnicity___8.factor = factor(data$race_ethnicity___8,levels=c("0","1"))
# data$race_ethnicity___9.factor = factor(data$race_ethnicity___9,levels=c("0","1"))

# sexual_orientation___* columns collapsed into one.
# data$sexual_orientation___1.factor = factor(data$sexual_orientation___1,levels=c("0","1"))
# data$sexual_orientation___2.factor = factor(data$sexual_orientation___2,levels=c("0","1"))
# data$sexual_orientation___3.factor = factor(data$sexual_orientation___3,levels=c("0","1"))
# data$sexual_orientation___4.factor = factor(data$sexual_orientation___4,levels=c("0","1"))
# data$sexual_orientation___5.factor = factor(data$sexual_orientation___5,levels=c("0","1"))
# data$sexual_orientation___6.factor = factor(data$sexual_orientation___6,levels=c("0","1"))
# data$sexual_orientation___7.factor = factor(data$sexual_orientation___7,levels=c("0","1"))
# data$sexual_orientation___8.factor = factor(data$sexual_orientation___8,levels=c("0","1"))

data$marital_status.factor = factor(data$marital_status,levels=c("0","1","2","3","4","5","6"))

# data$living_situation___0.factor = factor(data$living_situation___0,levels=c("0","1"))
# data$living_situation___1.factor = factor(data$living_situation___1,levels=c("0","1"))
# data$living_situation___2.factor = factor(data$living_situation___2,levels=c("0","1"))
# data$living_situation___3.factor = factor(data$living_situation___3,levels=c("0","1"))
# data$living_situation___4.factor = factor(data$living_situation___4,levels=c("0","1"))
# data$living_situation___5.factor = factor(data$living_situation___5,levels=c("0","1"))
# data$living_situation___6.factor = factor(data$living_situation___6,levels=c("0","1"))
# data$living_situation___7.factor = factor(data$living_situation___7,levels=c("0","1"))
# data$living_situation___8.factor = factor(data$living_situation___8,levels=c("0","1"))
# data$living_situation___9.factor = factor(data$living_situation___9,levels=c("0","1"))

# data$caregiving___0.factor = factor(data$caregiving___0,levels=c("0","1"))
# data$caregiving___1.factor = factor(data$caregiving___1,levels=c("0","1"))
# data$caregiving___2.factor = factor(data$caregiving___2,levels=c("0","1"))
# data$caregiving___3.factor = factor(data$caregiving___3,levels=c("0","1"))
# data$caregiving___4.factor = factor(data$caregiving___4,levels=c("0","1"))

data$clinical_fte.factor = factor(data$clinical_fte,levels=c("1","2","3","4"))
data$total_fte.factor = factor(data$total_fte,levels=c("1","2","3","4"))

data$demographics_complete.factor = factor(data$demographics_complete,levels=c("0","1","2"))

# data$program_support___0.factor = factor(data$program_support___0,levels=c("0","1"))
# data$program_support___1.factor = factor(data$program_support___1,levels=c("0","1"))
# data$program_support___2.factor = factor(data$program_support___2,levels=c("0","1"))
# data$program_support___3.factor = factor(data$program_support___3,levels=c("0","1"))
# data$program_support___4.factor = factor(data$program_support___4,levels=c("0","1"))
# data$program_support___5.factor = factor(data$program_support___5,levels=c("0","1"))
# data$program_support___6.factor = factor(data$program_support___6,levels=c("0","1"))
# data$program_support___7.factor = factor(data$program_support___7,levels=c("0","1"))

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

# data$workbook.factor = factor(data$workbook,levels=c("0","1","2","3","4","5","6"))
# data$recorded_calls.factor = factor(data$recorded_calls,levels=c("0","1","2","3","4","5","6"))
# data$webinars.factor = factor(data$webinars,levels=c("0","1","2","3","4","5","6"))
# data$live_coaching.factor = factor(data$live_coaching,levels=c("0","1","2","3","4","5","6"))
# data$coached.factor = factor(data$coached,levels=c("0","1","2","3","4","5","6"))
# data$pe_num_sessions.factor = factor(data$pe_num_sessions,levels=c("1","2","3","4","5"))
# data$pe_quality.factor = factor(data$pe_quality,levels=c("1","2","3","4","5"))
# data$pe_wellness.factor = factor(data$pe_wellness,levels=c("1","2","3","4","5"))
# data$pe_colleagues.factor = factor(data$pe_colleagues,levels=c("1","2","3","4","5"))
# data$pe_recommend.factor = factor(data$pe_recommend,levels=c("1","2","3","4","5"))
# data$post_test_additional_questions_complete.factor = factor(data$post_test_additional_questions_complete,levels=c("0","1","2"))


levels(data$redcap_event_name.factor)=c("Pre","Post","Randomization")
levels(data$enrollment_agreement.factor)=c("Yes","No")
levels(data$consent_complete.factor)=c("Incomplete","Unverified","Complete")
levels(data$bhc.factor)=c("Yes","No","Prefer not to answer")
levels(data$specialty.factor)=c("Behavioral Health","Family Medicine","Geriatrics","General Internal Medicine","Hospital Internal Medicine","Palliative Care","Pediatrics","Physical Medicine and Rehab","Other","Prefer not to answer")
levels(data$department_randomization.factor)=c("Pediatrics","Internal Medicine/Physical Medicine & Rehabilitation","Family Medicine","Other")
levels(data$degree.factor)=c("MD or DO","PhD","PsyD","Other","Prefer not to answer")
levels(data$gender_identity.factor)=c("Cis female","Cis male","Non-binary/third gender","Trans female","Trans male","Prefer to self-describe","Prefer not to answer")
levels(data$gender_randomization.factor)=c("Female (if you selected cis female or trans female above)","Male (if you selected cis male or trans male above)","Another gender identity (if you selected something other than cis/trans female or cis/trans male above)")

# levels(data$race_ethnicity___1.factor)=c("Unchecked","Checked")
# levels(data$race_ethnicity___2.factor)=c("Unchecked","Checked")
# levels(data$race_ethnicity___3.factor)=c("Unchecked","Checked")
# levels(data$race_ethnicity___4.factor)=c("Unchecked","Checked")
# levels(data$race_ethnicity___5.factor)=c("Unchecked","Checked")
# levels(data$race_ethnicity___6.factor)=c("Unchecked","Checked")
# levels(data$race_ethnicity___7.factor)=c("Unchecked","Checked")
# levels(data$race_ethnicity___8.factor)=c("Unchecked","Checked")
# levels(data$race_ethnicity___9.factor)=c("Unchecked","Checked")

# levels(data$sexual_orientation___1.factor)=c("Unchecked","Checked")
# levels(data$sexual_orientation___2.factor)=c("Unchecked","Checked")
# levels(data$sexual_orientation___3.factor)=c("Unchecked","Checked")
# levels(data$sexual_orientation___4.factor)=c("Unchecked","Checked")
# levels(data$sexual_orientation___5.factor)=c("Unchecked","Checked")
# levels(data$sexual_orientation___6.factor)=c("Unchecked","Checked")
# levels(data$sexual_orientation___7.factor)=c("Unchecked","Checked")
# levels(data$sexual_orientation___8.factor)=c("Unchecked","Checked")

levels(data$marital_status.factor)=c("Single","Married","Engaged","Divorced","Partnered","Other","Prefer not to answer")

# levels(data$living_situation___0.factor)=c("Unchecked","Checked")
# levels(data$living_situation___1.factor)=c("Unchecked","Checked")
# levels(data$living_situation___2.factor)=c("Unchecked","Checked")
# levels(data$living_situation___3.factor)=c("Unchecked","Checked")
# levels(data$living_situation___4.factor)=c("Unchecked","Checked")
# levels(data$living_situation___5.factor)=c("Unchecked","Checked")
# levels(data$living_situation___6.factor)=c("Unchecked","Checked")
# levels(data$living_situation___7.factor)=c("Unchecked","Checked")
# levels(data$living_situation___8.factor)=c("Unchecked","Checked")
# levels(data$living_situation___9.factor)=c("Unchecked","Checked")

# levels(data$caregiving___0.factor)=c("Unchecked","Checked")
# levels(data$caregiving___1.factor)=c("Unchecked","Checked")
# levels(data$caregiving___2.factor)=c("Unchecked","Checked")
# levels(data$caregiving___3.factor)=c("Unchecked","Checked")
# levels(data$caregiving___4.factor)=c("Unchecked","Checked")

levels(data$clinical_fte.factor)=c("0-25%","26-50%","51-75%",">76%")
levels(data$total_fte.factor)=c("0-25%","26-50%","51-75%",">76%")
levels(data$demographics_complete.factor)=c("Incomplete","Unverified","Complete")

# levels(data$program_support___0.factor)=c("Unchecked","Checked")
# levels(data$program_support___1.factor)=c("Unchecked","Checked")
# levels(data$program_support___2.factor)=c("Unchecked","Checked")
# levels(data$program_support___3.factor)=c("Unchecked","Checked")
# levels(data$program_support___4.factor)=c("Unchecked","Checked")
# levels(data$program_support___5.factor)=c("Unchecked","Checked")
# levels(data$program_support___6.factor)=c("Unchecked","Checked")
# levels(data$program_support___7.factor)=c("Unchecked","Checked")

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

# levels(data$workbook.factor)=c("Never","A few times in the 4 months","Monthly","A few times a month","Weekly","A few times a week","Daily")
# levels(data$recorded_calls.factor)=c("Never","A few times in the 4 months","Monthly","A few times a month","Weekly","A few times a week","Daily")
# levels(data$webinars.factor)=c("Never","A few times in the 4 months","Monthly","A few times a month","Weekly","A few times a week","Daily")
# levels(data$live_coaching.factor)=c("Never","A few times in the 4 months","Monthly","A few times a month","Weekly","A few times a week","Daily")
# levels(data$coached.factor)=c("Never","A few times in the 4 months","Monthly","A few times a month","Weekly","A few times a week","Daily")
# levels(data$pe_num_sessions.factor)=c("Strongly disagree","Disagree","Neutral","Agree","Strongly agree")
# levels(data$pe_quality.factor)=c("Strongly disagree","Disagree","Neutral","Agree","Strongly agree")
# levels(data$pe_wellness.factor)=c("Strongly disagree","Disagree","Neutral","Agree","Strongly agree")
# levels(data$pe_colleagues.factor)=c("Strongly disagree","Disagree","Neutral","Agree","Strongly agree")
# levels(data$pe_recommend.factor)=c("Strongly disagree","Disagree","Neutral","Agree","Strongly agree")
# levels(data$post_test_additional_questions_complete.factor)=c("Incomplete","Unverified","Complete")

data$randomization.factor = factor(data$randomization,levels=c("0","1"))
data$randomization_complete.factor = factor(data$randomization_complete,levels=c("0","1","2"))

levels(data$randomization.factor)=c("Waitlist","Intervention")
levels(data$randomization_complete.factor)=c("Incomplete","Unverified","Complete")

# Process user-created columns -------------------------------------------------
#data$yis_ispos.factor = factor(data$yis_ispos,levels=c("0","1")) # KB ADDED - create factor variable for dichotomous impostor syndrome present/absent
# levels(data$yis_ispos.factor) = c("No","Yes") # KB ADDED - create factor variable for dichotomous impostor syndrome present/absent
# label(data$yis_ispos.factor) = "YIS: Impostor syndrome present"


# data$ls3_lonely.factor = factor(data$ls3_lonely,levels=c("0","1")) # KB ADDED - create factor variable for dichotomous lonely/not lonely
# label(data$ls3_lonely.factor) = "3-Item Loneliness Scale: Lonely"
# levels(data$ls3_lonely.factor) = c("No","Yes") # KB ADDED - create factor variable for dichotomous lonely/not lonely