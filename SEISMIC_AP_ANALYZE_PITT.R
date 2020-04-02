#SEISMIC AP Analysis

#### Setup ####
# Load packages and data
if (!require("pacman")) install.packages("pacman")
library(pacman)
pacman::p_load("tidyverse", "data.table", "psych", "summarytools", "haven", "Hmisc", "forcats", 
               "naniar", "QuantPsyc", "epiDisplay", "corrplot", "tidyselect", "mctest", 
               "MatchIt", "WeightIt", "cobalt", "survey", "jtools","mice")

# Functions and settings ####
# Use dplyr for 'select'
select <- dplyr::select
# Turn off scientific notation up to 7 digits
options(scipen = 7)
# VIF function 
VIF <- function(linear.model, no.intercept=FALSE, all.diagnostics=FALSE, plot=FALSE) {
  require(mctest)
  if(no.intercept==FALSE) design.matrix <- model.matrix(linear.model)[,-1]
  if(no.intercept==TRUE) design.matrix <- model.matrix(linear.model)
  if(plot==TRUE) mc.plot(design.matrix,linear.model$model[1])
  if(all.diagnostics==FALSE) output <- imcdiag(design.matrix,linear.model$model[1], method='VIF')$idiags[,1]
  if(all.diagnostics==TRUE) output <- imcdiag(design.matrix,linear.model$model[1])
  output
}

# Load CLEAN Dataframes ####
df_sub <- read.csv("SEISMIC_AP_CLEAN.csv")
names(df_sub)

# Subset Dataframes 
# Took 2nd course in sequence
df_bio <- df_sub %>%
  subset(course == "BIO") 

df_gchem <- df_sub %>%
  subset(course == "CHEM") %>%
  subset(cohort_2014 != 1) 

df_phys <- df_ind %>%
  subset(PHYS2 == 1) %>%
  subset(YEAR_2014 != 1) %>%
  subset(YEAR_2015 != 1) %>%
  subset(YEAR_2019 !=1)

# Took AP 
df_BYtakers <- df_bio2 %>%
  subset(BY_CR > 0)
df_CHtakers <- df_gchem2 %>%
  subset(CH_CR > 0)
df_PHtakers <- df_phys2 %>%
  subset(PHCM_CR > 0)

# Skip eligible
df_BYeligible<- df_bio2 %>%
  subset(BIO1_TH == 1)
df_BYeligible.4 <- df_bio2 %>%
  subset(BY == 4)
df_BYeligible.5 <- df_bio2 %>%
  subset(BY == 5)

df_CHeligible<- df_gchem2 %>%
  subset(GCHEM1_TH == 1)
df_CHeligible.3 <- df_gchem2 %>%
  subset(CH == 3)
df_CHeligible.4 <- df_gchem2 %>%
  subset(CH == 4)
df_CHeligible.5 <- df_gchem2 %>%
  subset(CH == 5)

df_PHeligible<- df_phys2 %>%
  subset(PHYS1_TH == 1)
df_PHeligible.5 <- df_phys2 %>%
  subset(PHCE == 5 | PHCM == 5)

# Descriptive Stats ####
# Correlations among key predictors #
correl1 <- df_sub %>%
  ungroup() %>%
  select(FG, LOW_INCOME, FEMALE, URM, 
         HS_GPA, SAT_HIGH_MATH, SAT_HIGH_VERBAL, 
         YEAR_2014, YEAR_2015, YEAR_2016, YEAR_2017, YEAR_2018, YEAR_2019)
corrplot1 <- cor(correl1, use="pairwise.complete.obs")
corrplot(corrplot1,type = "lower")

#### RQ1 ####
# What student characteristics are associated with student participation and success in AP courses 
# for students enrolled at the selected universities?

# Model 1a: Credits ####
#Bio
m1.a_BY <- glm(BY_CR ~ factor(FG) + scale(LOW_INCOME)  + factor(FEMALE) + factor(URM) +
                 scale(HS_GPA) + scale(SAT_HIGH_MATH) + scale(SAT_HIGH_VERBAL) + 
                 factor(YEAR_2014) + factor(YEAR_2015) + factor(YEAR_2016) + factor(YEAR_2017) + factor(YEAR_2018),
               binomial(link = "logit"), df_bio2)
summary(m1.a_BY)
exp(cbind("Odds Ratio" = coef(m1.a_BY), confint.default(m1.a_BY, level = 0.95)))
logistic.display(m1.a_BY)

#Chem
m1.a_CH <- glm(CH_CR ~ factor(FG) + scale(LOW_INCOME)  + factor(FEMALE) + factor(URM) +
                 scale(HS_GPA) + scale(SAT_HIGH_MATH) + scale(SAT_HIGH_VERBAL) + 
                 factor(YEAR_2015) + factor(YEAR_2016) + factor(YEAR_2017) + factor(YEAR_2018),
               binomial(link = "logit"), df_gchem2)
summary(m1.a_CH)   
exp(cbind("Odds Ratio" = coef(m1.a_CH), confint.default(m1.a_CH, level = 0.95)))
logistic.display(m1.a_CH)

#Physics
m1.a_PHCM <- glm(PHCM_CR ~ factor(FG) + scale(LOW_INCOME)  + factor(FEMALE) + factor(URM) +
                   scale(HS_GPA) + scale(SAT_HIGH_MATH) + scale(SAT_HIGH_VERBAL) + 
                   factor(YEAR_2016) + factor(YEAR_2017) + factor(YEAR_2018),
                 binomial(link = "logit"), df_phys2)
summary(m1.a_PHCM)
exp(cbind("Odds Ratio" = coef(m1.a_PHCM), confint.default(m1.a_PHCM, level = 0.95)))
logistic.display(m1.a_PHCM)


sink("m1_output.csv")
print(summary(m1.a_BY))
print(summary(m1.a_CH))
print(summary(m1.a_PHCM))
sink()

sink("m1_output_OR.csv")
print(exp(cbind("Odds Ratio" = coef(m1.a_BY), confint.default(m1.a_BY, level = 0.95))))
print(exp(cbind("Odds Ratio" = coef(m1.a_CH), confint.default(m1.a_CH, level = 0.95))))
print(exp(cbind("Odds Ratio" = coef(m1.a_PHCM), confint.default(m1.a_PHCM, level = 0.95))))
sink()

# Model 1b: Score ####
#Bio
df_BYtakers <- df_bio2 %>%
  subset(BY_CR > 0)
m1.b_BY <- lm(scale(BY) ~ FG + scale(LOW_INCOME)  + FEMALE + URM +
                scale(HS_GPA) + scale(SAT_HIGH_MATH) + scale(SAT_HIGH_VERBAL) + 
                YEAR_2014 + YEAR_2015 + YEAR_2016 + YEAR_2017 + YEAR_2018, 
              df_BYtakers)
summary(m1.b_BY)
cbind("Beta" = coef(m1.b_BY), confint.default(m1.b_BY, level = 0.95))

#Chem
df_CHtakers <- df_gchem2 %>%
  subset(CH_CR > 0)
m1.b_CH <- lm(scale(CH) ~ FG + scale(LOW_INCOME)  + FEMALE + URM +
                scale(HS_GPA) + scale(SAT_HIGH_MATH) + scale(SAT_HIGH_VERBAL) + 
                YEAR_2015 + YEAR_2016 + YEAR_2017 + YEAR_2018, 
              df_CHtakers)
summary(m1.b_CH)   
cbind("Beta" = coef(m1.b_CH), confint.default(m1.b_CH, level = 0.95))

#Physics
df_PHCMtakers <- df_phys2 %>%
  subset(PHCM_CR > 0)
m1.b_PHCM <- lm(scale(PHCM) ~ FG + scale(LOW_INCOME)  + FEMALE + URM +
                  scale(HS_GPA) + scale(SAT_HIGH_MATH) + scale(SAT_HIGH_VERBAL) + 
                  YEAR_2016 + YEAR_2017 + YEAR_2018, 
                df_PHCMtakers)
summary(m1.b_PHCM)   
cbind("Beta" = coef(m1.b_PHCM), confint.default(m1.b_PHCM, level = 0.95))

sink("m2_output.csv")
print(summary(m1.b_BY))
print(summary(m1.b_CH))
print(summary(m1.b_PHCM))
sink()

sink("m2_output_CI.csv")
print(cbind("Beta" = coef(m1.b_BY), confint.default(m1.b_BY, level = 0.95)))
print(cbind("Beta" = coef(m1.b_CH), confint.default(m1.b_CH, level = 0.95)))
print(cbind("Beta" = coef(m1.b_PHCM), confint.default(m1.b_PHCM, level = 0.95)))
sink()


# Model 1c: Eligible to Skip ####
#Bio
m1.c_BY <- glm(BIO1_TH ~ factor(FG) + scale(LOW_INCOME)  + factor(FEMALE) + factor(URM) +
                 scale(HS_GPA) + scale(SAT_HIGH_MATH) + scale(SAT_HIGH_VERBAL) + 
                 factor(YEAR_2014) + factor(YEAR_2015) + factor(YEAR_2016) + factor(YEAR_2017) + factor(YEAR_2018), 
               binomial(link = "logit"), df_bio2)
summary(m1.c_BY)
exp(cbind("Odds Ratio" = coef(m1.c_BY), confint.default(m1.c_BY, level = 0.95)))
logistic.display(m1.c_BY)

#Chem
m1.c_CH <- glm(GCHEM1_TH ~ factor(FG) + scale(LOW_INCOME)  + factor(FEMALE) + factor(URM) +
                 scale(HS_GPA) + scale(SAT_HIGH_MATH) + scale(SAT_HIGH_VERBAL) + 
                 factor(YEAR_2015) + factor(YEAR_2016) + factor(YEAR_2017) + factor(YEAR_2018), 
               binomial(link = "logit"), df_gchem2)
summary(m1.c_CH)   
exp(cbind("OddsRatio" = coef(m1.c_CH), confint.default(m1.c_CH, level = 0.95)))
logistic.display(m1.c_CH)

#Physics
# Skip w/ CE or CM
m1.c_PHCM <- glm(PHYS1_TH ~ factor(FG) + scale(LOW_INCOME) + factor(FEMALE) + factor(URM) +
                   scale(HS_GPA) + scale(SAT_HIGH_MATH) + scale(SAT_HIGH_VERBAL) + 
                   factor(YEAR_2016) + factor(YEAR_2017) + factor(YEAR_2018), 
                 binomial(link = "logit"), df_phys2)
summary(m1.c_PHCM)   
exp(cbind("Odds Ratio" = coef(m1.c_PHCM), confint.default(m1.c_PHCM, level = 0.95)))
logistic.display(m1.c_PHCM)

sink("m2_output.csv")
print(summary(m1.c_BY))
print(summary(m1.c_CH))
print(summary(m1.c_PHCM))
sink()

sink("m2_output_CI.csv")
print(exp(cbind("Odds Ratio" = coef(m1.c_BY), confint.default(m1.c_BY, level = 0.95))))
print(exp(cbind("Odds Ratio" = coef(m1.c_CH), confint.default(m1.c_CH, level = 0.95))))
print(exp(cbind("Odds Ratio" = coef(m1.c_PHCM), confint.default(m1.c_PHCM, level = 0.95))))
sink()


#### RQ2 ####
# Model 2a: Skipped if eligible ####
#Bio
m2.a_BY <- glm(BIO1_SK ~ factor(FG) + scale(LOW_INCOME)  + factor(FEMALE) + factor(URM) +
                 scale(HS_GPA) + scale(SAT_HIGH_MATH) + scale(SAT_HIGH_VERBAL) + 
                 factor(YEAR_2014) + factor(YEAR_2015) + factor(YEAR_2016) + factor(YEAR_2017) + factor(YEAR_2018), 
               binomial(link = "logit"), df_BYeligible)
summary(m2.a_BY)
exp(cbind("Odds Ratio" = coef(m2.a_BY), confint.default(m2.a_BY, level = 0.95)))
logistic.display(m2.a_BY)

describe(df_BYeligible$BIO1_SK)

#Chem
m2.a_CH <- glm(GCHEM1_SK ~ factor(FG) + scale(LOW_INCOME)  + factor(FEMALE) + factor(URM) +
                 scale(HS_GPA) + scale(SAT_HIGH_MATH) + scale(SAT_HIGH_VERBAL) + 
                 factor(YEAR_2015) + factor(YEAR_2016) + factor(YEAR_2017) + factor(YEAR_2018), 
               binomial(link = "logit"), df_CHeligible)
summary(m2.a_CH)   
exp(cbind("Odds Ratio" = coef(m2.a_CH), confint.default(m2.a_CH, level = 0.95)))
logistic.display(m2.a_CH)

describe(df_CHeligible$GCHEM1_SK)

#Physics
# Skip w/ CE or CM
m2.a_PHCM <- glm(PHYS1_SK ~ factor(FG) + scale(LOW_INCOME) + factor(FEMALE) + factor(URM) +
                   scale(HS_GPA) + scale(SAT_HIGH_MATH) + scale(SAT_HIGH_VERBAL) + 
                   factor(YEAR_2016) + factor(YEAR_2017) + factor(YEAR_2018), 
                 binomial(link = "logit"), df_PHeligible)
summary(m2.a_PHCM)   
exp(cbind("Odds Ratio" = coef(m2.a_PHCM), confint.default(m2.a_PHCM, level = 0.95)))
logistic.display(m2.a_PHCM)

describe(df_PHeligible$PHYS1_SK)

sink("m2_output.csv")
print(summary(m2.a_BY))
print(summary(m2.a_CH))
print(summary(m2.a_PHCM))
sink()

sink("m2_output_CI.csv")
print(exp(cbind("Odds Ratio" = coef(m2.a_BY), confint.default(m2.a_BY, level = 0.95))))
print(exp(cbind("Odds Ratio" = coef(m2.a_CH), confint.default(m2.a_CH, level = 0.95))))
print(exp(cbind("Odds Ratio" = coef(m2.a_PHCM), confint.default(m2.a_PHCM, level = 0.95))))
sink()


# Grade in second course if took AP?
# Model 2b: Grade for AP takers ####
#Bio
m2.b_BY <- lm(scale(BIO2_GPA) ~ BIO1_SK + scale(BY) + FG + scale(LOW_INCOME)  + FEMALE + URM +
                scale(HS_GPA) + scale(SAT_HIGH_MATH) + scale(SAT_HIGH_VERBAL) + 
                YEAR_2014 + YEAR_2015 + YEAR_2016 + YEAR_2017 + YEAR_2018, 
              df_BYtakers)
summary(m2.b_BY)
cbind("Beta" = coef(m2.b_BY), confint.default(m2.b_BY, level = 0.95))
VIF(m2.b_BY)

#Bio eligible
m2.b.el_BY <- lm(scale(BIO2_GPA) ~ BIO1_SK + FG + scale(LOW_INCOME)  + FEMALE + URM +
                   scale(HS_GPA) + scale(SAT_HIGH_MATH) + scale(SAT_HIGH_VERBAL) + 
                   YEAR_2014 + YEAR_2015 + YEAR_2016 + YEAR_2017 + YEAR_2018, 
                 df_BYeligible)
summary(m2.b.el_BY)
cbind("Beta" = coef(m2.b.el_BY), confint.default(m2.b.el_BY, level = 0.95))
VIF(m2.b.el_BY)

#Bio if BY=4
m2.b4_BY <- lm(scale(BIO2_GPA) ~ BIO1_SK + FG + scale(LOW_INCOME)  + FEMALE + URM +
                 scale(HS_GPA) + scale(SAT_HIGH_MATH) + scale(SAT_HIGH_VERBAL) + 
                 YEAR_2014 + YEAR_2015 + YEAR_2016 + YEAR_2017 + YEAR_2018, 
               df_BYeligible.4)
summary(m2.b4_BY)
cbind("Beta" = coef(m2.b4_BY), confint.default(m2.b4_BY, level = 0.95))
VIF(m2.b4_BY)

#Bio if BY=5
m2.b5_BY <- lm(scale(BIO2_GPA) ~ BIO1_SK + FG + scale(LOW_INCOME)  + FEMALE + URM +
                 scale(HS_GPA) + scale(SAT_HIGH_MATH) + scale(SAT_HIGH_VERBAL) + 
                 YEAR_2014 + YEAR_2015 + YEAR_2016 + YEAR_2017 + YEAR_2018, 
               df_BYeligible.5)
summary(m2.b5_BY)
cbind("Beta" = coef(m2.b5_BY), confint.default(m2.b5_BY, level = 0.95))
VIF(m2.b5_BY)

#Chem
m2.b_CH <- lm(scale(GCHEM2_GPA) ~ GCHEM1_SK + scale(CH) + FG + scale(LOW_INCOME)  + FEMALE + URM +
                scale(HS_GPA) + scale(SAT_HIGH_MATH) + scale(SAT_HIGH_VERBAL) + 
                YEAR_2015 + YEAR_2016 + YEAR_2017 + YEAR_2018, 
              df_CHtakers)
summary(m2.b_CH)
cbind("Beta" = coef(m2.b_CH), confint.default(m2.b_CH, level = 0.95))
VIF(m2.b_CH)

#Chem eligible
m2.b.el_CH <- lm(scale(GCHEM2_GPA) ~ GCHEM1_SK + FG + scale(LOW_INCOME)  + FEMALE + URM +
                   scale(HS_GPA) + scale(SAT_HIGH_MATH) + scale(SAT_HIGH_VERBAL) + 
                   YEAR_2015 + YEAR_2016 + YEAR_2017 + YEAR_2018, 
                 df_CHeligible)
summary(m2.b.el_CH)
cbind("Beta" = coef(m2.b.el_CH), confint.default(m2.b.el_CH, level = 0.95))
VIF(m2.b.el_CH)

#Chem if CH = 3
m2.b3_CH <- lm(scale(GCHEM2_GPA) ~ GCHEM1_SK + FG + scale(LOW_INCOME)  + FEMALE + URM +
                 scale(HS_GPA) + scale(SAT_HIGH_MATH) + scale(SAT_HIGH_VERBAL) + 
                 YEAR_2015 + YEAR_2016 + YEAR_2017 + YEAR_2018, 
               df_CHeligible.3)
summary(m2.b3_CH)
cbind("Beta" = coef(m2.b3_CH), confint.default(m2.b3_CH, level = 0.95))
VIF(m2.b3_CH)

#Chem if CH = 4
m2.b4_CH <- lm(scale(GCHEM2_GPA) ~ GCHEM1_SK + FG + scale(LOW_INCOME)  + FEMALE + URM +
                 scale(HS_GPA) + scale(SAT_HIGH_MATH) + scale(SAT_HIGH_VERBAL) + 
                 YEAR_2015 + YEAR_2016 + YEAR_2017 + YEAR_2018, 
               df_CHeligible.4)
summary(m2.b4_CH)
cbind("Beta" = coef(m2.b4_CH), confint.default(m2.b4_CH, level = 0.95))
VIF(m2.b4_CH)

#Chem if CH = 5
m2.b5_CH <- lm(scale(GCHEM2_GPA) ~ GCHEM1_SK + FG + scale(LOW_INCOME)  + FEMALE + URM +
                 scale(HS_GPA) + scale(SAT_HIGH_MATH) + scale(SAT_HIGH_VERBAL) + 
                 YEAR_2015 + YEAR_2016 + YEAR_2017 + YEAR_2018, 
               df_CHeligible.5)
summary(m2.b5_CH)
cbind("Beta" = coef(m2.b5_CH), confint.default(m2.b5_CH, level = 0.95))
VIF(m2.b5_CH)

#Phys
m2.b_PHCM <- lm(scale(PHYS2_GPA) ~ PHYS1_SK + scale(PHCM) + FG + scale(LOW_INCOME)  + FEMALE + URM +
                  scale(HS_GPA) + scale(SAT_HIGH_MATH) + scale(SAT_HIGH_VERBAL) + 
                  YEAR_2016 + YEAR_2017 + YEAR_2018, 
                df_PHtakers)
summary(m2.b_PHCM)
cbind("Beta" = coef(m2.b_PHCM), confint.default(m2.b_PHCM, level = 0.95))
VIF(m2.b_PHCM)

#Phys eligible
m2.b.el_PHCM <- lm(scale(PHYS2_GPA) ~ PHYS1_SK + FG + scale(LOW_INCOME)  + FEMALE + URM +
                     scale(HS_GPA) + scale(SAT_HIGH_MATH) + scale(SAT_HIGH_VERBAL) + 
                     YEAR_2016 + YEAR_2017 + YEAR_2018, 
                   df_PHeligible)
summary(m2.b.el_PHCM)
cbind("Beta" = coef(m2.b.el_PHCM), confint.default(m2.b.el_PHCM, level = 0.95))
VIF(m2.b.el_PHCM)

#Phys if PH = 5
m2.b5_PHCM <- lm(scale(PHYS2_GPA) ~ PHYS1_SK + FG + scale(LOW_INCOME)  + FEMALE + URM +
                   scale(HS_GPA) + scale(SAT_HIGH_MATH) + scale(SAT_HIGH_VERBAL) + 
                   YEAR_2016 + YEAR_2017 + YEAR_2018, 
                 df_PHeligible.5)
summary(m2.b5_PHCM)
cbind("Beta" = coef(m2.b5_PHCM), confint.default(m2.b5_PHCM, level = 0.95))
VIF(m2.b5_PHCM)

# Model 2c: Grade for everyone ####
#Bio
m2.c_BY <- lm(scale(BIO2_GPA) ~ BIO1_SK + scale(BY_ALL) + FG + scale(LOW_INCOME)  + FEMALE + URM +
                scale(HS_GPA) + scale(SAT_HIGH_MATH) + scale(SAT_HIGH_VERBAL) + 
                YEAR_2014 + YEAR_2015 + YEAR_2016 + YEAR_2017 + YEAR_2018, 
              df_bio2)
summary(m2.c_BY)
cbind("Beta" = coef(m2.c_BY), confint.default(m2.c_BY, level = 0.95))
VIF(m2.c_BY)

#Chem
m2.c_CH <- lm(scale(GCHEM2_GPA) ~ GCHEM1_SK + scale(CH_ALL) + FG + scale(LOW_INCOME)  + FEMALE + URM +
                scale(HS_GPA) + scale(SAT_HIGH_MATH) + scale(SAT_HIGH_VERBAL) + 
                YEAR_2015 + YEAR_2016 + YEAR_2017 + YEAR_2018, 
              df_bio2)
summary(m2.c_CH)
cbind("Beta" = coef(m2.c_CH), confint.default(m2.c_CH, level = 0.95))
VIF(m2.c_CH)

#Phys
m2.c_PHCM <- lm(scale(PHYS2_GPA) ~ PHYS1_SK + scale(PH_ALL) + FG + scale(LOW_INCOME)  + FEMALE + URM +
                  scale(HS_GPA) + scale(SAT_HIGH_MATH) + scale(SAT_HIGH_VERBAL) + 
                  YEAR_2016 + YEAR_2017 + YEAR_2018, 
                df_phys2)
summary(m2.c_PHCM)
cbind("Beta" = coef(m2.c_PHCM), confint.default(m2.c_PHCM, level = 0.95))
VIF(m2.c_PHCM)

# Model 2d: Grade for AP takers (c.skipped, no score)####
#Bio
m2.d_BY <- lm(scale(BIO2_GPA) ~ BIO1_SK + FG + scale(LOW_INCOME)  + FEMALE + URM +
                scale(HS_GPA) + scale(SAT_HIGH_MATH) + scale(SAT_HIGH_VERBAL) + 
                YEAR_2014 + YEAR_2015 + YEAR_2016 + YEAR_2017 + YEAR_2018, 
              df_BYtakers)
summary(m2.d_BY)
cbind("Beta" = coef(m2.d_BY), confint.default(m2.d_BY, level = 0.95))
VIF(m2.d_BY)

#Chem
m2.d_CH <- lm(scale(GCHEM2_GPA) ~ GCHEM1_SK + FG + scale(LOW_INCOME)  + FEMALE + URM +
                scale(HS_GPA) + scale(SAT_HIGH_MATH) + scale(SAT_HIGH_VERBAL) + 
                YEAR_2015 + YEAR_2016 + YEAR_2017 + YEAR_2018, 
              df_CHtakers)
summary(m2.d_CH)
cbind("Beta" = coef(m2.d_CH), confint.default(m2.d_CH, level = 0.95))
VIF(m2.d_CH)

#Phys
m2.d_PHCM <- lm(scale(PHYS2_GPA) ~ PHYS1_SK + FG + scale(LOW_INCOME)  + FEMALE + URM +
                  scale(HS_GPA) + scale(SAT_HIGH_MATH) + scale(SAT_HIGH_VERBAL) + 
                  YEAR_2016 + YEAR_2017 + YEAR_2018, 
                df_PHtakers)
summary(m2.d_PHCM)
cbind("Beta" = coef(m2.d_PHCM), confint.default(m2.d_PHCM, level = 0.95))
VIF(m2.d_PHCM)

# Model 2e: Grade for everyone (c.skipped, no score) ####
#Bio
m2.e_BY <- lm(scale(BIO2_GPA) ~ BIO1_SK + FG + scale(LOW_INCOME)  + FEMALE + URM +
                scale(HS_GPA) + scale(SAT_HIGH_MATH) + scale(SAT_HIGH_VERBAL) + 
                YEAR_2014 + YEAR_2015 + YEAR_2016 + YEAR_2017 + YEAR_2018, 
              df_bio2)
summary(m2.e_BY)
cbind("Beta" = coef(m2.e_BY), confint.default(m2.e_BY, level = 0.95))
VIF(m2.e_BY)

#Chem
m2.e_CH <- lm(scale(GCHEM2_GPA) ~ GCHEM1_SK + FG + scale(LOW_INCOME)  + FEMALE + URM +
                scale(HS_GPA) + scale(SAT_HIGH_MATH) + scale(SAT_HIGH_VERBAL) + 
                YEAR_2015 + YEAR_2016 + YEAR_2017 + YEAR_2018, 
              df_gchem2)
summary(m2.e_CH)
cbind("Beta" = coef(m2.e_CH), confint.default(m2.e_CH, level = 0.95))
VIF(m2.e_CH)

#Phys
m2.e_PHCM <- lm(scale(PHYS2_GPA) ~ PHYS1_SK + FG + scale(LOW_INCOME)  + FEMALE + URM +
                  scale(HS_GPA) + scale(SAT_HIGH_MATH) + scale(SAT_HIGH_VERBAL) + 
                  YEAR_2016 + YEAR_2017 + YEAR_2018, 
                df_phys2)
summary(m2.e_PHCM)
cbind("Beta" = coef(m2.e_PHCM), confint.default(m2.e_PHCM, level = 0.95))
VIF(m2.e_PHCM)

# Model 2f: Grade for AP takers (c.skip eligible, no score)####
#Bio
m2.f_BY <- lm(scale(BIO2_GPA) ~ BIO1_TH + FG + scale(LOW_INCOME)  + FEMALE + URM +
                scale(HS_GPA) + scale(SAT_HIGH_MATH) + scale(SAT_HIGH_VERBAL) + 
                YEAR_2014 + YEAR_2015 + YEAR_2016 + YEAR_2017 + YEAR_2018, 
              df_BYtakers)
summary(m2.f_BY)
cbind("Beta" = coef(m2.f_BY), confint.default(m2.f_BY, level = 0.95))
VIF(m2.f_BY)

#Chem
m2.f_CH <- lm(scale(GCHEM2_GPA) ~ GCHEM1_TH + FG + scale(LOW_INCOME)  + FEMALE + URM +
                scale(HS_GPA) + scale(SAT_HIGH_MATH) + scale(SAT_HIGH_VERBAL) + 
                YEAR_2015 + YEAR_2016 + YEAR_2017 + YEAR_2018, 
              df_CHtakers)
summary(m2.f_CH)
cbind("Beta" = coef(m2.f_CH), confint.default(m2.f_CH, level = 0.95))
VIF(m2.f_CH)

#Phys
m2.f_PHCM <- lm(scale(PHYS2_GPA) ~ PHYS1_TH + FG + scale(LOW_INCOME)  + FEMALE + URM +
                  scale(HS_GPA) + scale(SAT_HIGH_MATH) + scale(SAT_HIGH_VERBAL) + 
                  YEAR_2016 + YEAR_2017 + YEAR_2018, 
                df_PHtakers)
summary(m2.f_PHCM)
cbind("Beta" = coef(m2.f_PHCM), confint.default(m2.f_PHCM, level = 0.95))
VIF(m2.f_PHCM)


# Model 2g: Grade for everyone (c.skip eligible, no score) ####
#Bio
m2.g_BY <- lm(scale(BIO2_GPA) ~ BIO1_TH + FG + scale(LOW_INCOME)  + FEMALE + URM +
                scale(HS_GPA) + scale(SAT_HIGH_MATH) + scale(SAT_HIGH_VERBAL) + 
                YEAR_2014 + YEAR_2015 + YEAR_2016 + YEAR_2017 + YEAR_2018, 
              df_bio2)
summary(m2.g_BY)
cbind("Beta" = coef(m2.g_BY), confint.default(m2.g_BY, level = 0.95))
VIF(m2.g_BY)

#Chem
m2.g_CH <- lm(scale(GCHEM2_GPA) ~ GCHEM1_TH + FG + scale(LOW_INCOME)  + FEMALE + URM +
                scale(HS_GPA) + scale(SAT_HIGH_MATH) + scale(SAT_HIGH_VERBAL) + 
                YEAR_2015 + YEAR_2016 + YEAR_2017 + YEAR_2018, 
              df_gchem2)
summary(m2.g_CH)
cbind("Beta" = coef(m2.g_CH), confint.default(m2.g_CH, level = 0.95))
VIF(m2.g_CH)

#Phys
m2.g_PHCM <- lm(scale(PHYS2_GPA) ~ PHYS1_TH + FG + scale(LOW_INCOME)  + FEMALE + URM +
                  scale(HS_GPA) + scale(SAT_HIGH_MATH) + scale(SAT_HIGH_VERBAL) + 
                  YEAR_2016 + YEAR_2017 + YEAR_2018, 
                df_phys2)
summary(m2.g_PHCM)
cbind("Beta" = coef(m2.g_PHCM), confint.default(m2.g_PHCM, level = 0.95))
VIF(m2.g_PHCM)



##### Weighting ####
# Bio Weighting ####
#Data with no missing
df_bio2_comp <- df_bio2 %>%  
  select(BIO2_GPA, BY_ALL, BIO1_TH, BIO1_SK, FG, LOW_INCOME, FEMALE, URM, 
         HS_GPA, SAT_HIGH_MATH, SAT_HIGH_VERBAL, 
         YEAR_2014, YEAR_2015, YEAR_2016, YEAR_2017, YEAR_2018) %>%
  filter(complete.cases(.))

# Check balance before weighting
bal.tab(BIO1_SK ~ + FG + scale(LOW_INCOME) + FEMALE + URM +
          scale(HS_GPA) + scale(SAT_HIGH_MATH) + scale(SAT_HIGH_VERBAL) + 
          YEAR_2014 + YEAR_2015 + YEAR_2016 + YEAR_2017 + YEAR_2018,
        data = df_bio2_comp, estimand = "ATT", m.threshold = .05)

# Estimate weights
bio.out <- weightit(BIO1_SK ~ FG + scale(LOW_INCOME) + FEMALE + URM +
                      scale(HS_GPA) + scale(SAT_HIGH_MATH) + scale(SAT_HIGH_VERBAL) + 
                      YEAR_2014 + YEAR_2015 + YEAR_2016 + YEAR_2017 + YEAR_2018,
                    data = df_bio2_comp, estimand = "ATT")
summary(bio.out) 

# Check balance after weighting
bal.tab(bio.out, m.threshold = .05, disp.v.ratio = TRUE)

#Extract weights
bio.w <- svydesign(ids = ~1, weights = bio.out$weights,
                   data = df_bio2_comp)

# Chem Weighting ####
#Data with no missing
df_gchem2_comp <- df_gchem2 %>%  
  select(GCHEM2_GPA, CH_ALL, GCHEM1_TH, GCHEM1_SK, FG, LOW_INCOME, FEMALE, URM, 
         HS_GPA, SAT_HIGH_MATH, SAT_HIGH_VERBAL, 
         YEAR_2014, YEAR_2015, YEAR_2016, YEAR_2017, YEAR_2018) %>%
  filter(complete.cases(.))

# Check balance before weighting
bal.tab(GCHEM1_SK ~ + FG + scale(LOW_INCOME) + FEMALE + URM +
          scale(HS_GPA) + scale(SAT_HIGH_MATH) + scale(SAT_HIGH_VERBAL) + 
          YEAR_2015 + YEAR_2016 + YEAR_2017 + YEAR_2018,
        data = df_gchem2_comp, estimand = "ATT", m.threshold = .05)

# Estimate weights
chem.out <- weightit(GCHEM1_SK ~ FG + scale(LOW_INCOME) + FEMALE + URM +
                       scale(HS_GPA) + scale(SAT_HIGH_MATH) + scale(SAT_HIGH_VERBAL) + 
                       YEAR_2015 + YEAR_2016 + YEAR_2017 + YEAR_2018,
                     data = df_gchem2_comp, estimand = "ATT")
summary(chem.out) 

# Check balance after weighting
bal.tab(chem.out, m.threshold = .05, disp.v.ratio = TRUE)

#Extract weights
chem.w <- svydesign(ids = ~1, weights = chem.out$weights,
                    data = df_gchem2_comp)

# Phys Weighting ####
#Data with no missing
df_phys2_comp <- df_phys2 %>%  
  select(PHYS2_GPA, PH_ALL, PHYS1_TH, PHYS1_SK, FG, LOW_INCOME, FEMALE, URM, 
         HS_GPA, SAT_HIGH_MATH, SAT_HIGH_VERBAL, 
         YEAR_2014, YEAR_2015, YEAR_2016, YEAR_2017, YEAR_2018) %>%
  filter(complete.cases(.))

# Check balance before weighting
bal.tab(PHYS1_SK ~ + FG + scale(LOW_INCOME) + FEMALE + URM +
          scale(HS_GPA) + scale(SAT_HIGH_MATH) + scale(SAT_HIGH_VERBAL) + 
          YEAR_2016 + YEAR_2017 + YEAR_2018,
        data = df_phys2_comp, estimand = "ATT", m.threshold = .05)

# Estimate weights
phys.out <- weightit(PHYS1_SK ~ FG + scale(LOW_INCOME) + FEMALE + URM +
                       scale(HS_GPA) + scale(SAT_HIGH_MATH) + scale(SAT_HIGH_VERBAL) + 
                       YEAR_2016 + YEAR_2017 + YEAR_2018,
                     data = df_phys2_comp, estimand = "ATT")
summary(phys.out) 

# Check balance after weighting
bal.tab(phys.out, m.threshold = .05, disp.v.ratio = TRUE)

#Extract weights
phys.w <- svydesign(ids = ~1, weights = phys.out$weights,
                    data = df_phys2_comp)

# Model 2e.2: Grade for everyone (c.skipped, no score) ####
#Bio
m2e.2_BY <- svyglm(scale(BIO2_GPA) ~ BIO1_SK + FG + scale(LOW_INCOME) + FEMALE + URM +
                     scale(HS_GPA) + scale(SAT_HIGH_MATH) + scale(SAT_HIGH_VERBAL) + 
                     YEAR_2014 + YEAR_2015 + YEAR_2016 + YEAR_2017 + YEAR_2018, design = bio.w)
summary(m2e.2_BY)
cbind("Beta" = coef(m2e.2_BY), confint.default(m2e.2_BY, level = 0.95))

# w/ Robust SE 
summ(m2e.2_BY, confint = TRUE, 
     model.fit = FALSE, model.info = FALSE) 

#Chem
m2e.2_CH <- svyglm(scale(GCHEM2_GPA) ~ GCHEM1_SK + FG + scale(LOW_INCOME) + FEMALE + URM +
                     scale(HS_GPA) + scale(SAT_HIGH_MATH) + scale(SAT_HIGH_VERBAL) + 
                     YEAR_2015 + YEAR_2016 + YEAR_2017 + YEAR_2018, design = chem.w)
summary(m2e.2_CH)
cbind("Beta" = coef(m2e.2_CH), confint.default(m2e.2_CH, level = 0.95))

# w/ Robust SE 
summ(m2e.2_CH, confint = TRUE, 
     model.fit = FALSE, model.info = FALSE) 

#Phys
m2e.2_PH <- svyglm(scale(PHYS2_GPA) ~ PHYS1_SK + FG + scale(LOW_INCOME) + FEMALE + URM +
                     scale(HS_GPA) + scale(SAT_HIGH_MATH) + scale(SAT_HIGH_VERBAL) + 
                     YEAR_2016 + YEAR_2017 + YEAR_2018, design = phys.w)
summary(m2e.2_PH)
cbind("Beta" = coef(m2e.2_PH), confint.default(m2e.2_PH, level = 0.95))

# w/ Robust SE 
summ(m2e.2_PH, confint = TRUE, 
     model.fit = FALSE, model.info = FALSE) 

# Model 2g.2: Grade for everyone (c.skip eligible, no score) ####
#Bio
m2g.2_BY <- svyglm(scale(BIO2_GPA) ~ BIO1_TH + FG + scale(LOW_INCOME) + FEMALE + URM +
                     scale(HS_GPA) + scale(SAT_HIGH_MATH) + scale(SAT_HIGH_VERBAL) + 
                     YEAR_2014 + YEAR_2015 + YEAR_2016 + YEAR_2017 + YEAR_2018, design = bio.w)
summary(m2g.2_BY)
cbind("Beta" = coef(m2g.2_BY), confint.default(m2g.2_BY, level = 0.95))

summ(m2g.2_BY, confint = TRUE, 
     model.fit = FALSE, model.info = FALSE) 

#Chem
m2g.2_CH <- svyglm(scale(GCHEM2_GPA) ~ GCHEM1_TH + FG + scale(LOW_INCOME) + FEMALE + URM +
                     scale(HS_GPA) + scale(SAT_HIGH_MATH) + scale(SAT_HIGH_VERBAL) + 
                     YEAR_2015 + YEAR_2016 + YEAR_2017 + YEAR_2018, design = chem.w)
summary(m2g.2_CH)
cbind("Beta" = coef(m2g.2_CH), confint.default(m2g.2_CH, level = 0.95))

# w/ Robust SE 
summ(m2e.2_CH, confint = TRUE, 
     model.fit = FALSE, model.info = FALSE) 

#Phys
m2e.2_PH <- svyglm(scale(PHYS2_GPA) ~ PHYS1_TH + FG + scale(LOW_INCOME) + FEMALE + URM +
                     scale(HS_GPA) + scale(SAT_HIGH_MATH) + scale(SAT_HIGH_VERBAL) + 
                     YEAR_2016 + YEAR_2017 + YEAR_2018, design = phys.w)
summary(m2e.2_PH)
cbind("Beta" = coef(m2e.2_PH), confint.default(m2e.2_PH, level = 0.95))

# w/ Robust SE 
summ(m2e.2_PH, confint = TRUE, 
     model.fit = FALSE, model.info = FALSE) 


#### PSM ####
# Bio Matching ####
# Covariates for PSM
AP_bio_cov <- c('FEMALE', 'FG', 'LOW_INCOME', 'SAT_HIGH_VERBAL', 'HS_GPA', 'SAT_HIGH_MATH', 'YEAR_2015', 'YEAR_2016', 'YEAR_2017', 'YEAR_2018', 'BY')

# Estimate propensity score
bio.psm <- glm(BIO1_SK ~ FEMALE + SAT_HIGH_VERBAL + SAT_HIGH_MATH + BY,
               family = binomial(), data = df_bio2)

# Create dataframe with propensity scores
df_bio2_psm <- data.frame(pr_score = predict(bio.psm, type = "response"),
                          BIO1_SK = bio.psm$model$BIO1_SK)

# Matched dataset (no missing)
df_bio2_nomiss <- df_bio2 %>%  
  select(BIO2_GPA, BY, BIO1_SK, one_of(AP_cov)) %>%
  na.omit()

bio_matched <- matchit(BIO1_SK ~ FEMALE + SAT_HIGH_VERBAL + SAT_HIGH_MATH + BY,
                       method = "nearest", data = df_bio2_nomiss)

df_bio2_matched <- match.data(bio_matched)

# Examine pre-matched and matched differences
df_bio2 %>%
  group_by(BIO1_SK) %>%
  select(one_of(AP_cov), BY) %>%
  summarise_all(funs(mean(., na.rm = T)))

df_bio2_matched %>%
  group_by(BIO1_SK) %>%
  select(one_of(AP_cov), BY) %>%
  summarise_all(funs(mean(., na.rm = T)))


# Gchem Matching ####
# Covariates for PSM
AP_gchem_cov <- c('FEMALE', 'FG', 'LOW_INCOME', 'SAT_HIGH_VERBAL', 'HS_GPA', 'SAT_HIGH_MATH', 'YEAR_2015', 'YEAR_2016', 'YEAR_2017', 'YEAR_2018', 'CH')

# Estimate propensity score
gchem.psm <- glm(GCHEM1_SK ~ FEMALE + SAT_HIGH_VERBAL + SAT_HIGH_MATH + CH,
                 family = binomial(), data = df_gchem2)

# Create dataframe with propensity scores
df_gchem2_psm <- data.frame(pr_score = predict(gchem.psm, type = "response"),
                            GCHEM1_SK = gchem.psm$model$GCHEM1_SK)

# Matched dataset (no missing)
df_gchem2_nomiss <- df_gchem2 %>%  
  select(GCHEM2_GPA, CH, GCHEM1_SK, one_of(AP_gchem_cov)) %>%
  na.omit()

gchem_matched <- matchit(GCHEM1_SK ~ FEMALE + SAT_HIGH_VERBAL + SAT_HIGH_MATH + CH,
                         method = "nearest", data = df_gchem2_nomiss)

df_gchem2_matched <- match.data(gchem_matched)

# Examine pre-matched and matched differences
df_gchem2 %>%
  group_by(GCHEM1_SK) %>%
  select(one_of(AP_gchem_cov), CH) %>%
  summarise_all(funs(mean(., na.rm = T)))

df_gchem2_matched %>%
  group_by(GCHEM1_SK) %>%
  select(one_of(AP_gchem_cov), CH) %>%
  summarise_all(funs(mean(., na.rm = T)))

# Phys Matching ####
# Covariates for PSM
AP_phys_cov <- c('FEMALE', 'FG', 'LOW_INCOME', 'SAT_HIGH_VERBAL', 'HS_GPA', 'SAT_HIGH_MATH', 'YEAR_2015', 'YEAR_2016', 'YEAR_2017', 'YEAR_2018', 'PHCM')

# Estimate propensity score
phys.psm <- glm(PHYS1_SK ~ FEMALE + SAT_HIGH_VERBAL + SAT_HIGH_MATH + PHCM,
                family = binomial(), data = df_phys2)

# Create dataframe with propensity scores
df_phys2_psm <- data.frame(pr_score = predict(phys.psm, type = "response"),
                           PHYS1_SK = phys.psm$model$PHYS1_SK)

# Matched dataset (no missing)
df_phys2_nomiss <- df_phys2 %>%  
  select(PHYS2_GPA, PHCM, PHYS1_SK, one_of(AP_cov)) %>%
  na.omit()

phys_matched <- matchit(PHYS1_SK ~ FEMALE + SAT_HIGH_VERBAL + SAT_HIGH_MATH + PHCM,
                        method = "nearest", data = df_phys2_nomiss)

df_phys2_matched <- match.data(phys_matched)

# Examine pre-matched and matched differences
df_phys2 %>%
  group_by(PHYS1_SK) %>%
  select(one_of(AP_cov), PHCM) %>%
  summarise_all(funs(mean(., na.rm = T)))

df_phys2_matched %>%
  group_by(PHYS1_SK) %>%
  select(one_of(AP_cov), PHCM) %>%
  summarise_all(funs(mean(., na.rm = T)))
