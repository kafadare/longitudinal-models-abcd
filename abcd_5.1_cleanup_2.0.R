#Load libraries
library(dplyr)
library(magrittr)
library(ggplot2)
library(rlang)
library(gridExtra)
library(purrr)
library(tidyr)

#SET TO DIRECTORY FOR GITHUB REPO
setwd("/mnt/isilon/bgdlab_processing/Eren/ABCD-braincharts/ABCD_premie_code/")
#load function for reading csv/txt files into a named list
source("load_data_fct.R")
#SET TO OUTPUT FOLDER PATH
out_folder <- "/mnt/isilon/bgdlab_processing/Eren/ABCD-braincharts/CSV/process_tables_5.1_2.0/update/"
raw_tables_folder <- "/mnt/isilon/bgdlab_processing/Eren/ABCD-braincharts/CSV/raw_tables_5.1/"


#specify the locations for ABCD 5.1 data files
#imaging files
abcd_vol_aseg_file <- paste0(raw_tables_folder,"mri_y_smr_vol_aseg.csv")
abcd_vol_dsk_file <- paste0(raw_tables_folder,"mri_y_smr_vol_dsk.csv")
abcd_area_dsk_file <- paste0(raw_tables_folder,"mri_y_smr_area_dsk.csv")
abcd_thk_dsk_file <- paste0(raw_tables_folder,"mri_y_smr_thk_dsk.csv")
abcd_sulc_dsk_file <- paste0(raw_tables_folder,"mri_y_smr_sulc_dsk.csv")
abcd_mri_adm_file <- paste0(raw_tables_folder,"mri_y_adm_info.csv")
abcd_mri_qc_file <- paste0(raw_tables_folder,"mri_y_qc_incl.csv")
#demographics and health history files
abcd_demo_file <- paste0(raw_tables_folder,"abcd_p_demo.csv")
abcd_devhx_file <- paste0(raw_tables_folder,"ph_p_dhx.csv")
abcd_medhx_file <- paste0(raw_tables_folder,"ph_p_mhx.csv")
abcd_ltrack_file <- paste0(raw_tables_folder,"abcd_y_lt.csv")
abcd_screen_file <- paste0(raw_tables_folder,"abcd_p_screen.csv")
abcd_gen_file <- paste0(raw_tables_folder,"gen_y_pihat.csv")

#Load the data files into a named list
abcd_data <- load_data(names = c("abcd_vol_aseg", "abcd_vol_dsk","abcd_area_dsk", 
                                 "abcd_thk_dsk", "abcd_mri_adm", "abcd_mri_qc", 
                                 "abcd_demo", "abcd_devhx", "abcd_medhx", "abcd_ltrack"), 
                       c(abcd_vol_aseg_file, abcd_vol_dsk_file, abcd_area_dsk_file, 
                         abcd_thk_dsk_file, abcd_mri_adm_file, abcd_mri_qc_file, 
                         abcd_demo_file, abcd_devhx_file, abcd_medhx_file, abcd_ltrack_file))

## Jul 4th 2024 This field is populated with 'err', when compared against the gen_y_pihat data, it should be "52671" for birth ID. Also makes sense, because there is only one row with this family ID, so no option other than "1".
# Cross-checked all other rows for family ID and birth ID against gen_y_pihat data, they are all the same with the abcd_ltrack data. So I will use the abcd_ltrack data.
# Changing this field here manually to avoid issues down the line in case I decide to use birth ID for any exclusions/analyses.
abcd_gen_data <- read.csv(abcd_gen_file)
abcd_gen_data[which(abcd_gen_data$rel_family_id == 5267),"rel_birth_id"]
abcd_data$abcd_ltrack[which(abcd_data$abcd_ltrack$rel_family_id == 5267),"rel_birth_id"] <- 52671

#combine all data tables in the abcd_data list into one big dataframe, merged on subject id and event (timepoint)
abcd_long <- Reduce(function(x, y) merge(x, y, by = c("src_subject_id", "eventname"), all=TRUE), abcd_data, accumulate=FALSE)

#Merging screener separately because it should merge only on subject id, and not eventname.
#drop "eventname" column because the variables in the screener are one-time values (not longitudinally changing)
abcd_screener <- read.csv(abcd_screen_file) %>% select(-eventname)
#merge abcd_long data with screener variables
abcd_long <- merge(abcd_long, abcd_screener, by = "src_subject_id", all = TRUE)

#calculate total mri phenotypes by adding RH and LH for cerebellar WM, cerebral WM, and cerebellar GM
abcd_long$totalWM_cb <- abcd_long$smri_vol_scs_cbwmatterlh + abcd_long$smri_vol_scs_cbwmatterrh
abcd_long$totalWM_crb <- abcd_long$smri_vol_scs_crbwmatterlh + abcd_long$smri_vol_scs_crbwmatterrh
abcd_long$totalGM_crb <- abcd_long$smri_vol_scs_crbcortexlh + abcd_long$smri_vol_scs_crbcortexrh

#check number of rows for each timepoint ('event')
table(abcd_long$eventname)

#Get GA from the dhx (developmental history) survey at baseline.
##Compared baseline & 4yr f/up reporting, decided to use baseline reporting, very few people filled the fouryear survey.
baseline_dev <- abcd_data$abcd_devhx[abcd_data$abcd_devhx$eventname == "baseline_year_1_arm_1",]
#Frequency of responses to prematurity/gestational age questions.
table(baseline_dev$devhx_12a_p)#"Was your child born prematurely?" 0 - NO, 1 - YES, 999 - DON'T KNOW
table(baseline_dev$devhx_12_p)#If previous question is "YES", then "How many weeks prematurely from 40 weeks?", 999 - DON'T KNOW
##In a new column gestAge, reporting "DON'T KNOW"(999) to "premature or not?" (devhx_12a_p) is encoded as NA (N = 140).
## 2 subjects had NA recorded for this question. Total NA is N = 142.
## Reporting "DON'T KNOW" (999) to "How many weeks premature" (devhx_12_p) is N = 27. 
## These subjects will also be encoded as NA for gestAge column. 
## For premature vs not column ('PTB'), these will be considered "premature".
#Calculate gestAge column based on explanations above, and for subjects that are "premature" and have the # of weeks
# encoded under devhx_12_p, subtract the number of weeks reported from 40 weeks for gestAge.
baseline_dev$gestAge <- ifelse(baseline_dev$devhx_12a_p == "0", 40, 
                               ifelse(baseline_dev$devhx_12a_p == "999", NA, 
                                      ifelse(baseline_dev$devhx_12_p == "999", NA, 40 - baseline_dev$devhx_12_p)))
#frequency table of gestational age
table(baseline_dev$gestAge)
#Assign variable PTB as binary premature/not premature (based on the first question, not based on actual weeks of gestAge)
##Note - This variable is not used in any downstream analyses.
baseline_dev$PTB <- baseline_dev$devhx_12a_p 
baseline_dev <- baseline_dev %>% mutate(PTB = na_if(PTB, 999))
sum(is.na(baseline_dev$gestAge))#169

#check how many in the full data do not have developmental hx
sum(!(unique(abcd_long$src_subject_id) %in% baseline_dev$src_subject_id))#3
#Merge gestAge and PTB columns with ABCD long. Remove subjects who do not have developmental hx.
abcd_long <- merge(abcd_long, baseline_dev[,c("src_subject_id", "gestAge", "PTB")], by = c("src_subject_id"), all.y = TRUE)
dim(abcd_long %>% filter(eventname == "baseline_year_1_arm_1") %>% filter(is.na(PTB)))
#total 142 IDs where PTB is NA 
#2 had NA for the PTB question, 140 responded "don't know"
dim(abcd_long %>% filter(eventname == "baseline_year_1_arm_1") %>% filter(is.na(gestAge)))
#total of 172 IDs where gestAge is NA. 
#2 had NA for the PTB question, 140 responded "don't know" to PTB question, 
#27 responded "don't know" to how many weeks premature question.

#Pull sex at birth data, from baseline data
baseline_demo <-  abcd_data$abcd_demo[abcd_data$abcd_demo$eventname == "baseline_year_1_arm_1",]
sum(is.na(baseline_demo$demo_sex_v2)) #no NAs in baseline data
baseline_demo <- baseline_demo %>% rename(sex_baseline = demo_sex_v2)
table(baseline_demo$sex_baseline)
#remove sex other than 1/2 (M/F)
baseline_demo <- baseline_demo %>% filter(sex_baseline %in% c(1,2))
#convert 1/2 to M/F
baseline_demo$sex_baseline <- as.factor(ifelse(baseline_demo$sex_baseline %in% "1", "M", 
                                           ifelse(baseline_demo$sex_baseline %in% "2", "F", NA)))
#Merge sex info
abcd_long <- merge(abcd_long, baseline_demo[,c("src_subject_id", "sex_baseline")], by = c("src_subject_id"), all.y = TRUE)

#Sanity Check to make sure to subject-event pairs are repeated
dup_indices <- duplicated(abcd_long[, c("src_subject_id", "eventname")]) | duplicated(abcd_long[, c("src_subject_id", "eventname")], fromLast = TRUE)
sum(dup_indices) #0 duplicates - what we want.
rm(dup_indices)
rm(baseline_dev, baseline_demo)

#Keep events with imaging data
abcd_long <- abcd_long %>% filter(eventname %in% c("baseline_year_1_arm_1", "2_year_follow_up_y_arm_1", "4_year_follow_up_y_arm_1"))

#Remove rows without interview age. Same subject ID has no interview age for year 1 and year 2.
dim(abcd_long %>% filter(eventname == "baseline_year_1_arm_1") %>% filter(is.na(interview_age)))
dim(abcd_long %>% filter(eventname == "2_year_follow_up_y_arm_1") %>% filter(is.na(interview_age)))
dim(abcd_long %>% filter(eventname == "4_year_follow_up_y_arm_1") %>% filter(is.na(interview_age)))
abcd_long <- abcd_long %>% filter(!is.na(interview_age))
#Calculate post-menstrual-weeks at time of scan. Using 4.3 weeks/month (average) + gestAge (when available).
## Note - this was used as "age" in the old modeling scheme, 
## after changing sample to include those without gestAge available, we are using chronological age to model age.
abcd_long$PCW_at_scan <- as.numeric(abcd_long$interview_age)*4.34 + abcd_long$gestAge
#Define preterm categories
abcd_long$preterm <- as.factor(cut(abcd_long$gestAge, breaks = c(-Inf, 31.9, 36.9, Inf), labels = c("VPM", "LPM", "Term"), include.lowest = TRUE))

#Remove based on QC image inclusion recommendation for T1W images
#using the include/don't include label for t1w
#remove rows that aren't recommended for image inclusion
#baseline/timepoint 1
sum(is.na(abcd_long %>% filter(eventname == "baseline_year_1_arm_1") %>% select(imgincl_t1w_include))) #96
sum(is.na(abcd_long %>% filter(is.na(imgincl_t1w_include) & eventname == "baseline_year_1_arm_1") %>% select(smri_vol_scs_allventricles)))#96
#QC flag NAs do not have imaging measures
sum(is.na(abcd_long %>% filter(imgincl_t1w_include == 0 & eventname == "baseline_year_1_arm_1") %>% select(smri_vol_scs_allventricles)))
#43 with QC flag "don't include" have no imaging measures
dim(abcd_long %>% filter(eventname == "baseline_year_1_arm_1" & imgincl_t1w_include == 0 & !is.na(smri_vol_scs_allventricles)))#458
#458 with imaging measures have "don't include" QC flag

#timepoint 2
sum(is.na(abcd_long %>% filter(eventname == "2_year_follow_up_y_arm_1") %>% select(imgincl_t1w_include)))#2846
sum(is.na(abcd_long %>% filter(is.na(imgincl_t1w_include) & eventname == "2_year_follow_up_y_arm_1") %>% select(smri_vol_scs_allventricles)))#2846
#2846 do not have imaging measures & have NA for inclusion flag
sum(is.na(abcd_long %>% filter(imgincl_t1w_include == 0 & eventname == "2_year_follow_up_y_arm_1") %>% select(smri_vol_scs_allventricles)))
#31 with QC flag "don't include" have no imaging measures
dim(abcd_long %>% filter(eventname == "2_year_follow_up_y_arm_1" & imgincl_t1w_include == 0 & !is.na(smri_vol_scs_allventricles)))#196
#196 with imaging measures have "don't include" QC flag

#timepoint 3 -- not included in analyses
sum(is.na(abcd_long %>% filter(eventname == "4_year_follow_up_y_arm_1") %>% select(imgincl_t1w_include)))#1709
sum(is.na(abcd_long %>% filter(is.na(imgincl_t1w_include) & eventname == "4_year_follow_up_y_arm_1") %>% select(smri_vol_scs_allventricles)))#1709
#1709 do not have imaging measures & have NA for inclusion flag
sum(is.na(abcd_long %>% filter(imgincl_t1w_include == 0 & eventname == "4_year_follow_up_y_arm_1") %>% select(smri_vol_scs_allventricles)))
#11 with QC flag "don't include" have no imaging measures
dim(abcd_long %>% filter(eventname == "4_year_follow_up_y_arm_1" & imgincl_t1w_include == 0 & !is.na(smri_vol_scs_allventricles)))#34
#34 with imaging measures have "don't include" QC flag

#Keep rows with imgincl flag == 1 ("include")
abcd_long <- abcd_long %>% filter(imgincl_t1w_include == 1)
table(abcd_long$eventname)
#only keep subjects that have timepoint 1 data
baseline_subject_ids <- abcd_long %>% filter(eventname == "baseline_year_1_arm_1") %>% select(src_subject_id)
dim(abcd_long %>% filter(eventname == "2_year_follow_up_y_arm_1" & !(src_subject_id %in% baseline_subject_ids$src_subject_id)))
dim(abcd_long %>% filter(eventname == "4_year_follow_up_y_arm_1" & !(src_subject_id %in% baseline_subject_ids$src_subject_id)))
abcd_long <- abcd_long %>% filter(src_subject_id %in% baseline_subject_ids$src_subject_id)
table(abcd_long$eventname)


#Assign variables for twin status -- currently not used in downstream analysis.
#Subset baseline data to flag singleton status
baseline_data <- abcd_long %>% filter(eventname == "baseline_year_1_arm_1") #rows = 11264
#For twin/non-singleton status, using rel_birth_ID because it is available for all subjects.
#rel_family_id and rel_birth_id columns in the abcd_ltrack table are only populated for baseline data. Jul 4th 24 (checked)
## Here we define two "twin status (inclusive of triplets) variables, twin_statusFAM one based on duplicate rel_birth_id, 
## twin_statusP based on the question in the developmental hx questionnaire "does your child child have a twin?"
baseline_data$twin_statusFAM <- duplicated(baseline_data$rel_birth_id) | duplicated(baseline_data$rel_birth_id, fromLast=TRUE) #1982 TRUE
#get triplet info from family and birth ID, encode triple status separately based on if there are >2 duplicate rel_birth_ids
table <- table(baseline_data$rel_birth_id)
triplet_ids <- baseline_data[baseline_data$rel_birth_id %in% names(table[table > 2]), "rel_birth_id"] %>% 
  unique()
length(triplet_ids)#8
baseline_data$triplet_statusFAM <- baseline_data$rel_birth_id %in% triplet_ids
baseline_data$twin_statusP <- baseline_data$devhx_5_p == 1#2115 TRUE
##The rel_birth_IDs and developmental hx don't agree, to resolve this disagreement 
##we mark all subjects based on rel_birth_id and developmental questionnaire as "nonSingletons"
##Create variable "nonsingleton". Use matching birth ID (twin_statusFAM) for non-singleton (1) and also add people who reported "yes" on the devhx question (twin_statusP) to capture those with twins not in the study.
#1 means not singleton (twin or triplet), 0 means singleton.
baseline_data$nonSingleton <- as.integer(rowSums(baseline_data[,which(names(baseline_data) %in%                                         c("twin_statusFAM", "triplet_statusFAM", "twin_statusP"))]) > 0)
table(baseline_data$nonSingleton)
#2142 NON-SINGLETON,, 9122 singleton
sum(is.na(baseline_data$nonSingleton))#0, good.

#Calculate total birthweight in ounces.
#if oz data is missing, add 8oz to reflect most likely average weight and maximize inclusion
sum(is.na(baseline_data$birth_weight_lbs))#487
sum(!is.na(baseline_data$birth_weight_lbs) & is.na(baseline_data$birth_weight_oz))#693
baseline_data$birth_weight_oz_adj <- baseline_data$birth_weight_oz #have adjusted values in a separate column, keep original column
baseline_data[which(is.na(baseline_data$birth_weight_oz) & !is.na(baseline_data$birth_weight_lbs)), "birth_weight_oz_adj"] <- 8
baseline_data$birth_weight_oz_sum_adj <- baseline_data$birth_weight_lbs*16 + baseline_data$birth_weight_oz_adj #column for total birth weight, in ounces, after adjustment
sum(is.na(baseline_data$birth_weight_oz_sum_adj))#487, sanity check.
#birthweight in ounces without adjustment (NA in subjects without oz data reported)
baseline_data$birth_weight_oz_sum <-  baseline_data$birth_weight_lbs*16 + baseline_data$birth_weight_oz
sum(is.na(baseline_data$birth_weight_oz_sum))#1180, sanity check.

#Merge nonsingleton and adjusted birthweight variables with full table.
abcd_long <- merge(abcd_long, baseline_data[,c("src_subject_id", "twin_statusFAM", "twin_statusP",
                                               "triplet_statusFAM", "nonSingleton", "birth_weight_oz_adj",
                                               "birth_weight_oz_sum_adj", "birth_weight_oz_sum")], by = c("src_subject_id"), all = TRUE) 

#Sanity Check to make sure to subject-event pairs are not repeated
dup_indices <- duplicated(abcd_long[, c("src_subject_id", "eventname")]) | duplicated(abcd_long[, c("src_subject_id", "eventname")], fromLast = TRUE)
sum(dup_indices) #0 duplicates - what we want.
rm(dup_indices)

#Rename factor levels of the eventname column
abcd_long <- abcd_long %>% 
  mutate(eventname = as.factor(eventname))
levels(abcd_long$eventname) <- c("t2", "t3", "t1")

#Add extra variables for other downstream analyses
filename_prs <- "/mnt/isilon/bgdlab_processing/Eren/ABCD-braincharts/CSV/abcd_clean.csv"
filename_psych1 <- "/mnt/isilon/bgdlab_processing/Eren/ABCD-braincharts/CSV/abcd_longitudinal_psychopathology_factors_scores_full_sample.csv"
filename_psych2 <- "/mnt/isilon/bgdlab_processing/Eren/ABCD-braincharts/CSV/psy_var.RData"
filename_anthro_vars <- "/mnt/isilon/bgdlab_processing/Eren/ABCD-braincharts/CSV/raw_tables_5.1/ph_y_anthro.csv"

abcd_prs <- read.csv(filename_prs)

#read in p-factor data and rename eventname column for merge.
abcd_psych <- read.csv(filename_psych1)
abcd_psych$eventname <- as.factor(abcd_psych$eventname)
levels(abcd_psych$eventname) <- c("t1", "t2")

load(filename_psych2)
#these measures are from baseline data, so adding a "t1" column for merging.
psy$eventname <- rep("t1", dim(psy)[1])

#read in anthropometric data and rename eventname column for merge
abcd_anthro <- read.csv(filename_anthro_vars) %>% filter(eventname %in% c("baseline_year_1_arm_1", "2_year_follow_up_y_arm_1", "4_year_follow_up_y_arm_1"))
abcd_anthro$eventname <- as.factor(abcd_anthro$eventname)
levels(abcd_anthro$eventname) <- c("t2", "t3", "t1")

#Calculate a composite variable for total of neonatal complications
baseline_data <- abcd_long %>% filter(eventname == "t1") #rows = 11264
neonatal_vars <- c("devhx_14a3_p", "devhx_14b3_p", "devhx_14c3_p", "devhx_14d3_p", "devhx_14e3_p", "devhx_14f3_p", "devhx_14g3_p", "devhx_14h3_p")
write.csv(baseline_data %>% select(all_of(c("src_subject_id", neonatal_vars))), 
          paste0(out_folder,"abcd_long_afterQC_neonatal_baseline.csv"))
#table for NAs and "don't know"s for each question
lapply(baseline_data[, neonatal_vars], table, useNA = "ifany")
#Replace "Don't Know" with NA.
abcd_long[neonatal_vars] <- lapply(abcd_long[neonatal_vars], function(x) replace(x, x==999, NA))
#sum neonatal variables, ignoring NAs. Keep NA for rows where all questions are NA.
abcd_long$neonatal_comp_total <- rowSums(abcd_long[neonatal_vars], na.rm = TRUE)
abcd_long$neonatal_comp_total <- replace(abcd_long$neonatal_comp_total, rowSums(is.na(abcd_long[neonatal_vars])) == 8, NA)
dim(abcd_long %>% filter(eventname == "t1" & is.na(neonatal_comp_total))) #154 NAs

#Regress out the first ten PCs from the PRS scores
#model_bw <- lm(bw_pgs ~ PC1 + PC2 + PC3 + PC4 + PC5 + PC6 + PC7 + PC8 + PC9 + PC10, data = abcd_prs)
#model_ga <- lm(ga_pgs ~ PC1 + PC2 + PC3 + PC4 + PC5 + PC6 + PC7 + PC8 + PC9 + PC10, data = abcd_prs)
#abcd_prs$bw_pgs_resd <- model_bw$residuals
#abcd_prs$ga_pgs_resd <- model_ga$residuals
#vars vector of 10 PCs for merge
ten_PCs <- paste0("PC", 1:10)

#Match participant names to other dataframes for merge
abcd_prs$participant <- sub("sub-NDAR", "NDAR_", abcd_prs$participant)

#Merge dataframes
abcd_long <- merge(abcd_long, abcd_psych %>% select(-c("rel_family_id", "interview_age", "site_id_l_br", "sex_M")), by = c("src_subject_id", "eventname"), all.x = TRUE)

abcd_long <- merge(abcd_long, psy %>% select (-c("age", "sex")), by.x = c("src_subject_id", "eventname"), by.y = c("IID", "eventname"), all.x = TRUE)

abcd_long <- merge(abcd_long, abcd_anthro , by = c("src_subject_id", "eventname"), all.x = TRUE)

abcd_long <- merge(abcd_long, abcd_prs[,c("bw_pgs", "ga_pgs", "bw", ten_PCs, "participant")], by.x = "src_subject_id", by.y = "participant", all.x = TRUE)


#Save table with all variables
write.csv(abcd_long, file = paste0(out_folder,"abcd5.1_long_full_2.0", Sys.Date(),".csv"))

#select useful variables for future analysis
vars_to_save <- c("src_subject_id", "eventname", "sex_baseline", "gestAge", "PTB", "preterm", "devhx_5_p",
                  "PCW_at_scan", "interview_age", "site_id_l", "twin_statusFAM", 
                  "twin_statusP","triplet_statusFAM","rel_family_id", "rel_birth_id", "nonSingleton",
                  "imgincl_t2w_include", "imgincl_t1w_include","mri_info_deviceserialnumber", "mri_info_softwareversion",
                  "scrn_birthcomp","birth_weight_lbs", "birth_weight_oz",  "birth_weight_oz_adj", "birth_weight_oz_sum", 
                  "birth_weight_oz_sum_adj","neonatal_comp_total","race_ethnicity",
                  "bw_pgs", "ga_pgs", "Factor1", "Factor2", "Factor3", "Factor4", 
                  "Factor5", "Factor6", "Factor7", "Factor8", "General_p", "anthroheightcalc", "anthroweightcalc", 
                  "anthro_waist_cm", "anthro_timestamp", "anxdep", "withdep", "somatic", "social", "thought", 
                  "attention", "rulebreak", "aggressive", "totprob", ten_PCs, 
                  names(abcd_long)[grep("smri", names(abcd_long))], "totalWM_cb", "totalWM_crb", "totalGM_crb")

saveRDS(vars_to_save, file = "/mnt/isilon/bgdlab_processing/Eren/ABCD-braincharts/ABCD_premie_code/vars_to_save_ABCD_2.0.rds")

abcd_long_toSave <- abcd_long %>% select(all_of(vars_to_save)) #326 variables

write.csv(abcd_long_toSave, file = paste0(out_folder,"abcd5.1_long_selectVars_2.0", Sys.Date(),".csv"))


#convert imaging variables to wide
scrn_names <- names(abcd_screener[,2:81]) #screener questions aren't by event, so separate them out
values_cols <- setdiff(names(abcd_long), 
                       c("eventname", "src_subject_id", "gestAge", "PTB", "pretem", "sex_baseline", 
                         "twin_statusFAM", "twin_statusP", "triplet_statusFAM", "nonSingleton",
                         "rel_family_id", "rel_birth_id", "birth_weight_lbs", "birth_weight_oz", 
                         "birth_weight_oz_adj", "birth_weight_sum_oz", "birth_weight_oz_sum_adj",
                         "neonatal_comp_total", "race_ethnicity", "bw_pgs", "ga_pgs", "bw_pgs_resd",
                         "ga_pgs_resd", ten_PCs, scrn_names)) #define the variable names that should not be converted to wide

abcd_wide <- abcd_long %>% select(c("src_subject_id", "eventname", "sex_baseline", 
                                    "gestAge", "PTB", "preterm","PCW_at_scan", "interview_age", 
                                    "site_id_l", "nonSingleton",  
                                    names(abcd_long)[grep("smri", names(abcd_long))], 
                                    "totalWM_cb", "totalWM_crb", "totalGM_crb")) %>%
  pivot_wider(., names_from = "eventname", values_from = any_of(values_cols), names_sep = ".")

write.csv(abcd_wide, file = paste0(out_folder,"abcd5.1_wide_imaging_2.0", Sys.Date(),".csv"))


#DEFINE SPLITS for split-half model-fitting
#read in abcd reproducible matched halves table (ARMS)
abcd_split <- read.table('/mnt/isilon/bgdlab_processing/Eren/ABCD-braincharts/CSV/participants.tsv', sep = "\t", header = T) %>% 
  mutate(participant_id = gsub("sub-", "", .$participant_id)) %>% 
  mutate(participant_id = gsub("NDAR", "NDAR_", .$participant_id)) %>%
  rename(src_subject_id = participant_id, site_id_l = site)

#Choosing split IDs based on timepoint 1 data, then add these to the greater dataset for all timepoints.

#merge processed data and ARMS table by participant ID and site (duplicate participant IDs in the participant.tsv table with different site IDs)
#only keep those rows that match on subject id & site id in our baseline dataframe.
#7 subjects did not have matching subject & site ID pairs in the split-defining participants.tsv file
baseline_all <- merge(baseline_data, abcd_split %>% select(all_of(c("src_subject_id",
                                                                       "matched_group", "handedness", 
                                                                       "parental_education", "income", 
                                                                       "anesthesia_exposure", "participant_education", 
                                                                    "site_id_l"))), 
                  by = c("src_subject_id", "site_id_l"), 
                  all.x = FALSE, all.y = FALSE)


splitOne <- baseline_all[which(baseline_all$matched_group == 1),]
splitTwo <- baseline_all[which(baseline_all$matched_group == 2),]

#check how many repeated family IDs in each split group
paste0("Train, # of unique family IDS:",length(unique(splitOne$rel_family_id)),
       "  unique # of participants:",length(unique(splitOne$src_subject_id)),
       "  # of rows in the dataframe:",dim(splitOne)[1])
paste0("Test, # of unique family IDS:",length(unique(splitTwo$rel_family_id)),
       "  unique # of participants:",length(unique(splitTwo$src_subject_id)),
       "  # of rows in the dataframe:",dim(splitTwo)[1])


#Test sets
#Remove subjects with duplicate family IDs between splitOne test and splitTwo test groups
splitOne_test <- splitOne %>%
  filter(!(rel_family_id %in% splitTwo$rel_family_id))#got rid of 73 people in the split one test set

sum(duplicated(splitOne_test$rel_family_id))#886 duplicates within the split one test set

splitTwo_test <- splitTwo %>%
  filter(!(rel_family_id %in% splitOne$rel_family_id))#got rid of 68 people in the split two test set

sum(duplicated(splitTwo_test$rel_family_id))#897 duplicates within the split two train set


#sanity check
sum(splitOne_test$rel_family_id %in% splitTwo$rel_family_id) #0
sum(splitTwo_test$rel_family_id %in% splitOne$rel_family_id) #0

#Save the two test set IDs
splitOne_test_IDs <- splitOne_test$src_subject_id
splitTwo_test_IDs <- splitTwo_test$src_subject_id
abcd_long <- merge(abcd_long, baseline_all %>% select(all_of(c("src_subject_id",
                                                                    "matched_group", "handedness", 
                                                                    "parental_education", "income", 
                                                                    "anesthesia_exposure", 
                                                           "participant_education"))), by = c("src_subject_id"), 
                                                      all.y = T)
abcd_long_Onetest <- abcd_long %>% filter(src_subject_id %in% splitOne_test_IDs)
abcd_long_Twotest <- abcd_long %>% filter(src_subject_id %in% splitTwo_test_IDs)

vars_to_save_splits <- c(vars_to_save, "matched_group", "handedness", 
                         "parental_education", "income", 
                         "anesthesia_exposure", 
                         "participant_education")

saveRDS(vars_to_save_splits, file = "/mnt/isilon/bgdlab_processing/Eren/ABCD-braincharts/ABCD_premie_code/vars_to_save_splits_ABCD_2.0.rds")

abcd_long_Onetest <- abcd_long_Onetest %>% select(all_of(vars_to_save_splits))
abcd_long_Twotest <- abcd_long_Twotest %>% select(all_of(vars_to_save_splits))

write.csv(abcd_long_Onetest, file = paste0(out_folder,
                                          "abcd_long_Onetest", 
                                           Sys.Date(),".csv"))
write.csv(abcd_long_Twotest, file = paste0(out_folder,
                                          "abcd_long_Twotest", 
                                           Sys.Date(),".csv"))

write.csv(splitOne_test_IDs, file = paste0(out_folder,
                                           "splitOne_test_IDs_2.0", 
                                           Sys.Date(),".csv"))
write.csv(splitTwo_test_IDs, file = paste0(out_folder,
                                           "splitTwo_test_IDs_2.0", 
                                           Sys.Date(),".csv"))

#FOR TRAIN SETS, REMOVE DUP FAMILY ID within set(select only one person from each family)

#Create first train set
set.seed(42)
splitOne_train <- splitOne_test %>%
  group_by(rel_family_id) %>%
  slice_sample(n = 1) #got rid of 886 people in the train 1 set


#Create the second train set 
set.seed(42)
splitTwo_train <- splitTwo_test %>%
  group_by(rel_family_id) %>%
  slice_sample(n = 1) #got rid of 897 people in the train 2 set


#sanity check
sum(duplicated(splitOne_train$rel_family_id))#0
sum(duplicated(splitTwo_train$rel_family_id))#0
sum(splitTwo_train$rel_family_id %in% splitOne$rel_family_id) #0
sum(splitOne_train$rel_family_id %in% splitTwo$rel_family_id) #0


#Save the two train set IDs
splitOne_train_IDs <- splitOne_train$src_subject_id
splitTwo_train_IDs <- splitTwo_train$src_subject_id

abcd_long_Onetrain <- abcd_long %>% filter(src_subject_id %in% splitOne_train_IDs)
abcd_long_Twotrain <- abcd_long %>% filter(src_subject_id %in% splitTwo_train_IDs)


abcd_long_Onetrain <- abcd_long_Onetrain %>% select(all_of(vars_to_save_splits))
abcd_long_Twotrain <- abcd_long_Twotrain %>% select(all_of(vars_to_save_splits))

write.csv(abcd_long_Onetrain, file = paste0(out_folder,
                                           "abcd_long_Onetrain", 
                                           Sys.Date(),".csv"))
write.csv(abcd_long_Twotrain, file = paste0(out_folder,
                                           "abcd_long_Twotrain", 
                                           Sys.Date(),".csv"))

write.csv(splitOne_train_IDs, file = paste0(out_folder,
                                           "splitOne_train_IDs_2.0", 
                                           Sys.Date(),".csv"))
write.csv(splitTwo_train_IDs, file = paste0(out_folder,
                                           "splitTwo_train_IDs_2.0", 
                                           Sys.Date(),".csv"))