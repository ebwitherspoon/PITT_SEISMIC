#SEISMIC AP Data Cleaning
if (!require("pacman")) install.packages("pacman")
library(pacman)
pacman::p_load("tidyverse", "data.table", "psych", "summarytools", "haven", "Hmisc")

# IMPORT DATASETS ####
df_grades <- read.csv("2191_MATH_StudentGrades.csv")
df_dist <- read.csv("2191_GradeDistALL.csv")
df_stable <- read.csv("2191_MATH_StudentStable.csv")
df_tests <- read.csv("2191_MATH_StudentTests.csv")

# CLEAN DATASETS ####
df_dist <- select(df_dist, -USERNAME_H, -EMPLID_H)

#Remove duplicate observations
df_stable_unq <- distinct(df_stable)

#Reshape wide and subset to only AP Tests
df_tests_ap <- df_tests %>%
  subset(TEST_ID == "AP") %>%
  select(EMPLID_H, TEST_COMPONENT, SCORE) %>%
  mutate(AP_CRED = 1) %>%
  group_by(EMPLID_H,TEST_COMPONENT,add = TRUE) %>%
  summarize(SCORE = max(SCORE)) %>%
  spread(TEST_COMPONENT,SCORE)

df_tests_ap <- df_tests_ap %>%
  select(EMPLID_H, BY, CH, PY1, PY2, PHCE, PHCM, MAB, MBC)

# MERGE DATASETS ####
df_grade_dist <- df_grades %>%
  inner_join(df_dist, by =c("SUBJECT_CD", "CATALOG_NBR", "CLASS_TITLE", "TERM_CD", "CLASS_NBR"))

df_full <- df_stable %>%
  select(-USERNAME_H, -COURSE_TYPE, -FIRST_GENERATION_DESCR) %>%
  inner_join(df_grade_dist, by="EMPLID_H") %>%
  inner_join(df_tests_ap, by="EMPLID_H")
  

#Replace ID as string
df_full <- df_full %>%
  mutate_if(is.factor, as.character)

#Remove Unecessary Data Frames
rm(df_dist, df_grades, df_grade_dist, df_stable, df_stable_unq, df_tests, df_tests_ap)

#Write FULL dataframe to .csv
write.csv(df_full, file = "2191_MATH_FULL.csv")



#===================================#
# >>>> ANALYSES START HERE! <<<< ####
#Load FULL Dataframe
df_full <- read.csv("2191_MATH_FULL.csv")
names(df_full)

# INCLUSION/EXCLUSION CRITERIA ####
df_sub <- df_full %>%
  subset(FIRST_TIME_FRESHMAN == "Y") %>%
  subset(CITIZENSHIP_STATUS_DESCR == "U.S. Citizen") %>%
  subset(TOT_TRNSFR_CREDITS <= 16) %>%
  subset(START_TRM_CD >= 2144) %>%
  subset(CAMPUS_CD == "PIT")

# GENERATE VARIABLES ####
#Demographics
df_sub <- df_sub %>%
  mutate(FEMALE = recode(GENDER_CD, "F"=1, "M"=0, "m"=0, "U" = 2)) %>%
  mutate(FG = recode(FIRST_GENERATION_DESCR, "First Generation" = 1, "Not First Generation" = 0)) %>%
  mutate(RACE_ETHNICITY = recode(ETHNIC_GROUP_CD, "HISPA" = 1, "BLACK" = 1, "AMIND" = 1, "PACIF" = 1, "ASIAN" = 2, "WHITE" = 0)) %>%
  mutate(URM = recode(ETHNIC_GROUP_CD, "HISPA" = 1, "BLACK" = 1, "AMIND" = 1, "PACIF" = 1, "ASIAN" = 0, "WHITE" = 0)) %>%
  mutate(AGI = abs(AGI)) %>%
  mutate(LOW_INCOME = if_else(AGI <= 46435, 1, 0)) %>%
  mutate(ELL = if_else(TOEFL_SCORE > 0, 0, 1))

#AP Credits
df_sub <- df_sub %>%
  mutate(BY_CR = ifelse(is.na(BY), 0, 1)) %>%
  mutate(CH_CR = ifelse(is.na(CH), 0, 1)) %>%
  mutate(PY1_CR = ifelse(is.na(PY1), 0, 1)) %>%
  mutate(PY2_CR = ifelse(is.na(PY2), 0, 1)) %>%
  mutate(PHCE_CR = ifelse(is.na(PHCE), 0, 1)) %>%
  mutate(PHCM_CR = ifelse(is.na(PHCM), 0, 1)) %>%
  mutate(MAB_CR = ifelse(is.na(MAB), 0, 1)) %>%
  mutate(MBC_CR = ifelse(is.na(MBC), 0, 1))

#AP Credits
df_sub <- df_sub %>%
  mutate(BIO1_SK = ifelse(BY >= 4 & !is.na(BY), 1, 0)) %>%
  mutate(BIO2_SK = ifelse(BY == 5 & !is.na(BY), 1, 0)) %>%
  mutate(GCHEM1_SK = ifelse(CH >= 3 & !is.na(CH), 1, 0)) %>%
  mutate(GCHEM1_SK = ifelse(CH == 5 & !is.na(CH), 1, 0)) %>%
  mutate(PHYS1_SK_CE = ifelse(PHCE == 5 & !is.na(PHCE), 1,0)) %>%
  mutate(PHYS1_SK_CM = ifelse(PHCM == 5  & !is.na(PHCM), 1, 0))

#STD Course Terms
df_sub <- df_sub %>%
  separate(as.character("START_TRM_CD"), c("YEAR", "SEMESTER"), 3, remove = FALSE) %>%
  separate(as.character("YEAR"), c("DEC", "YEAR"), 1) %>%
  mutate(TERM_REF = START_TRM_CD-TERM_CD) %>%
  mutate(TERM_STD = if_else(SEMESTER == "1" & TERM_REF == "0", 1,
                                          if_else(TERM_REF == -3, 1.5,
                                          if_else(TERM_REF == -6, 1.66,
                                          if_else(TERM_REF == -10, 2,
                                          if_else(TERM_REF == -13, 2.5,
                                          if_else(TERM_REF == -16, 2.66,
                                          if_else(TERM_REF == -20, 3,
                                          if_else(TERM_REF == -23, 3.5,
                                          if_else(TERM_REF == -26, 3.66,
                                          if_else(TERM_REF == -30, 4,
                                          if_else(TERM_REF == -33, 4.5,
                                          if_else(TERM_REF == -36, 4.66, 
                      if_else(SEMESTER == "4" & TERM_REF == "0", 1,
                                          if_else(TERM_REF == -3, 1.5,
                                          if_else(TERM_REF == -7, 1.66,
                                          if_else(TERM_REF == -10, 2,
                                          if_else(TERM_REF == -13, 2.5,
                                          if_else(TERM_REF == -17, 2.66,
                                          if_else(TERM_REF == -20, 3,
                                          if_else(TERM_REF == -23, 3.5,
                                          if_else(TERM_REF == -27, 3.66,
                                          if_else(TERM_REF == -30, 4,
                                          if_else(TERM_REF == -33, 4.5,
                                          if_else(TERM_REF == -37, 4.66, 
                      if_else(SEMESTER == "7" & TERM_REF == "0", 1,
                                          if_else(TERM_REF == -4, 1.5,
                                          if_else(TERM_REF == -7, 1.66,
                                          if_else(TERM_REF == -10, 2,
                                          if_else(TERM_REF == -14, 2.5,
                                          if_else(TERM_REF == -17, 2.66,
                                          if_else(TERM_REF == -20, 3,
                                          if_else(TERM_REF == -24, 3.5,
                                          if_else(TERM_REF == -26, 3.66,
                                          if_else(TERM_REF == -30, 4,
                                          if_else(TERM_REF == -34, 4.5,
                                          if_else(TERM_REF == -36, 4.66, NaN )))))))))))))
                                          )))))))))))))
                                          )))))))))))
                            

#Course Enrolled
df_sub <- df_sub %>%
  mutate(PHYS1 = ifelse(
    SUBJECT_CD == "PHYS" & CATALOG_NBR == "0174" & COURSE_GRADE_CD != "W", 1, 0)) %>%
  mutate(PHYS2 = ifelse(
    SUBJECT_CD == "PHYS" & CATALOG_NBR == "0175" & COURSE_GRADE_CD != "W", 1, 0)) %>%
  mutate(GCHEM1 = ifelse(
    SUBJECT_CD == "CHEM" & CATALOG_NBR == "0110" & COURSE_GRADE_CD != "W", 1, 0)) %>%
  mutate(GCHEM2 = ifelse(
    SUBJECT_CD == "CHEM" & CATALOG_NBR == "0120" & COURSE_GRADE_CD != "W", 1, 0)) %>%
  mutate(BIO1 = ifelse(
    SUBJECT_CD == "BIOSC" & CATALOG_NBR == "0150" & COURSE_GRADE_CD != "W", 1, 0)) %>%
  mutate(BIO2 = ifelse(
    SUBJECT_CD == "BIOSC" & CATALOG_NBR == "0160" & COURSE_GRADE_CD != "W", 1, 0))

#Course Grade
df_sub <- df_sub %>%
  mutate(PHYS1_GPA = ifelse(
    SUBJECT_CD == "PHYS" & CATALOG_NBR == "0174", GRADE_POINTS/UNITS_TAKEN, NA)) %>%
  mutate(PHYS2_GPA = ifelse(
    SUBJECT_CD == "PHYS" & CATALOG_NBR == "0175", GRADE_POINTS/UNITS_TAKEN, NA)) %>%
  mutate(GCHEM1_GPA = ifelse(
    SUBJECT_CD == "CHEM" & CATALOG_NBR == "0110", GRADE_POINTS/UNITS_TAKEN, NA)) %>%
  mutate(GCHEM2_GPA = ifelse(
    SUBJECT_CD == "CHEM" & CATALOG_NBR == "0120", GRADE_POINTS/UNITS_TAKEN, NA)) %>%
  mutate(BIO1_GPA = ifelse(
    SUBJECT_CD == "BIOSC" & CATALOG_NBR == "0150", GRADE_POINTS/UNITS_TAKEN, NA)) %>%
  mutate(BIO2_GPA = ifelse(
    SUBJECT_CD == "BIOSC" & CATALOG_NBR == "0160", GRADE_POINTS/UNITS_TAKEN, NA))

#Course Withdrawn
df_sub <- df_sub %>%
  mutate(PHYS1_W = ifelse(
    SUBJECT_CD == "PHYS" & CATALOG_NBR == "0174" & COURSE_GRADE_CD == "W", 1, 0)) %>%
  mutate(PHYS2_W = ifelse(
    SUBJECT_CD == "PHYS" & CATALOG_NBR == "0175" & COURSE_GRADE_CD == "W", 1, 0)) %>%
  mutate(GCHEM1_W = ifelse(
    SUBJECT_CD == "CHEM" & CATALOG_NBR == "0110" & COURSE_GRADE_CD == "W", 1, 0)) %>%
  mutate(GCHEM2_W = ifelse(
    SUBJECT_CD == "CHEM" & CATALOG_NBR == "0120" & COURSE_GRADE_CD == "W", 1, 0)) %>%
  mutate(BIO1_W = ifelse(
    SUBJECT_CD == "BIOSC" & CATALOG_NBR == "0150" & COURSE_GRADE_CD == "W", 1, 0)) %>%
  mutate(BIO2_W = ifelse(
    SUBJECT_CD == "BIOSC" & CATALOG_NBR == "0160" & COURSE_GRADE_CD == "W", 1, 0))

#Summer Course
df_sub <- df_sub %>%
  mutate(PHYS1_Sum = ifelse(
    SUBJECT_CD == "PHYS" & CATALOG_NBR == "0174" & endsWith(as.character(TERM_CD),"7"), 1, 0)) %>%
  mutate(PHYS2_Sum = ifelse(
    SUBJECT_CD == "PHYS" & CATALOG_NBR == "0175" & endsWith(as.character(TERM_CD),"7"), 1, 0)) %>%
  mutate(GCHEM1_Sum = ifelse(
    SUBJECT_CD == "CHEM" & CATALOG_NBR == "0110" & endsWith(as.character(TERM_CD),"7"), 1, 0)) %>%
  mutate(GCHEM2_Sum = ifelse(
    SUBJECT_CD == "CHEM" & CATALOG_NBR == "0120" & endsWith(as.character(TERM_CD),"7"), 1, 0)) %>%
  mutate(BIO1_Sum = ifelse(
    SUBJECT_CD == "BIOSC" & CATALOG_NBR == "0150" & endsWith(as.character(TERM_CD),"7"), 1, 0)) %>%
  mutate(BIO2_Sum = ifelse(
    SUBJECT_CD == "BIOSC" & CATALOG_NBR == "0160" & endsWith(as.character(TERM_CD),"7"), 1, 0)) 

#Course Terms (STD)
df_sub <- df_sub %>%
  mutate(PHYS1_TRM = ifelse(
    SUBJECT_CD == "PHYS" & CATALOG_NBR == "0174", TERM_STD, NaN)) %>%
  mutate(PHYS2_TRM = ifelse(
    SUBJECT_CD == "PHYS" & CATALOG_NBR == "0175" , TERM_STD, NaN)) %>%
  mutate(GCHEM1_TRM = ifelse(
    SUBJECT_CD == "CHEM" & CATALOG_NBR == "0110" , TERM_STD, NaN)) %>%
  mutate(GCHEM2_TRM = ifelse(
    SUBJECT_CD == "CHEM" & CATALOG_NBR == "0120", TERM_STD, NaN)) %>%
  mutate(BIO1_TRM = ifelse(
    SUBJECT_CD == "BIOSC" & CATALOG_NBR == "0150", TERM_STD, NaN)) %>%
  mutate(BIO2_TRM = ifelse(
    SUBJECT_CD == "BIOSC" & CATALOG_NBR == "0160" , TERM_STD, NaN))

#Course Terms (YEAR)
df_sub <- df_sub %>%
  mutate(PHYS1_YR = ifelse(
    SUBJECT_CD == "PHYS" & CATALOG_NBR == "0174", YEAR, NaN)) %>%
  mutate(PHYS2_YR = ifelse(
    SUBJECT_CD == "PHYS" & CATALOG_NBR == "0175" , YEAR, NaN)) %>%
  mutate(GCHEM1_YR = ifelse(
    SUBJECT_CD == "CHEM" & CATALOG_NBR == "0110" , YEAR, NaN)) %>%
  mutate(GCHEM2_YR = ifelse(
    SUBJECT_CD == "CHEM" & CATALOG_NBR == "0120", YEAR, NaN)) %>%
  mutate(BIO1_YR = ifelse(
    SUBJECT_CD == "BIOSC" & CATALOG_NBR == "0150", YEAR, NaN)) %>%
  mutate(BIO2_YR = ifelse(
    SUBJECT_CD == "BIOSC" & CATALOG_NBR == "0160" , YEAR, NaN))

#Summary Variables
df_sub <- df_sub %>%
  group_by(EMPLID_H) %>%
  mutate(mPHYS1 = max(PHYS1)) %>%
  mutate(mPHYS2 = max(PHYS2)) %>%
  mutate(mGCHEM1 = max(GCHEM1)) %>%
  mutate(mGCHEM2 = max(GCHEM2)) %>%
  mutate(mBIO1 = max(BIO1)) %>%
  mutate(mBIO2 = max(BIO2))

df_sub <- df_sub %>%
  group_by(EMPLID_H) %>%
  mutate(mGPA = mean(GRADE_POINTS/UNITS_TAKEN))

#df_sub <- df_sub %>%                               #Code not running
 # group_by(EMPLID_H) %>% 
  #summarize(
   # mGPA_1 = mean(PHYS1_GPA, BIO1_GPA, GCHEM1_GPA),
    #mGPA_2 = mean(PHYS2_GPA, BIO2_GPA, GCHEM2_GPA))
                
df_sub <- df_sub %>%
  group_by(EMPLID_H) %>%
  mutate(mPHYS1_W = max(PHYS1_W)) %>%
  mutate(mPHYS2_W = max(PHYS2_W)) %>%
  mutate(mGCHEM1_W = max(GCHEM1_W)) %>%
  mutate(mGCHEM2_W = max(GCHEM2_W)) %>%
  mutate(mBIO1_W = max(BIO1_W)) %>%
  mutate(mBIO2_W = max(BIO2_W))

df_sub <- df_sub %>%
  group_by(EMPLID_H, TERM_CD) %>%
  mutate(mCRED = mean(UNITS_TAKEN))

# DESCRIPTIVE STATS ####
df_sub %>%
  group_by(EMPLID_H) %>%
  select(  BY_CR, CH_CR, PY1_CR, PY2_CR, PHCE_CR, PHCM_CR,
           PHYS1, PHYS2, BIO1, BIO2, GCHEM1, GCHEM2,
           PHYS1_GPA, PHYS2_GPA, BIO1_GPA, BIO2_GPA, GCHEM1_GPA, GCHEM2_GPA,
           PHYS1_W, PHYS2_W, GCHEM1_W, GCHEM2_W, BIO1_W, BIO2_W,
           PHYS1_Sum, PHYS2_Sum, BIO1_Sum, BIO2_Sum, GCHEM1_Sum, GCHEM2_Sum,
           PHYS1_TRM, PHYS2_TRM, BIO1_TRM, BIO2_TRM, GCHEM1_TRM, GCHEM2_TRM,
           PHYS1_YR, PHYS2_YR, BIO1_YR, BIO2_YR, GCHEM1_YR, GCHEM2_YR,
           PHYS1_SK_CE, PHYS1_SK_CM, BIO1_SK, GCHEM1_SK,
           FEMALE, ETHNIC_GROUP_CD, RACE_ETHNICITY, FIRST_GENERATION_DESCR, AGI, HS_GPA, SAT_HIGH_VERBAL, SAT_HIGH_WRIT, SAT_HIGH_MATH,  
           DEC, YEAR, SEMESTER, START_TRM_CD, TERM_CD, TERM_STD, UNITS_TAKEN, GRADE_POINTS,
           BY, CH, PY1, PY2, PHCE, PHCM,
           mCRED, mPHYS1_W, mPHYS2_W, mGCHEM1_W, mGCHEM2_W, mBIO1_W, mBIO2_W,
           mPHYS1, mPHYS2, mBIO1, mBIO2, mGCHEM1, mGCHEM2) %>%
          describe()

view(dfSummary(df_sub))
describe(df_sub)
descr(df_sub)


