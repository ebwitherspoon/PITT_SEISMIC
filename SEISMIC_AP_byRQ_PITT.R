#### Setup ####
# Load packages and data
if (!require("pacman")) install.packages("pacman")
library(pacman)
pacman::p_load("tidyverse")

# Load Clean Dataframe ####
df_clean <- read.csv("~/Box Sync/LSAP_LRDC/Research Projects/SEISMIC/AP/SEISMIC_AP/SEISMIC_AP_CLEAN.csv")
names(df_clean)

# Generate Subset Dataframes ####
df_full <- df_clean %>%
  filter(discipline == "BIO" & apyear >= 2013 |
           discipline == "CHEM" & apyear >= 2014 |
           discipline == "PHYS" & apyear >= 2015)

df_aptakers <- df_clean %>%
  filter(discipline == "BIO" & apyear >= 2013 & aptaker == 1 |
           discipline == "CHEM" & apyear >= 2014 & aptaker == 1 |
           discipline == "PHYS" & apyear >= 2015 & aptaker == 1) 

df_skipeligible <- df_clean %>%
  filter(discipline == "BIO" & apyear >= 2013 & aptaker == 1 & apskipper == 1 |
           discipline == "CHEM" & apyear >= 2014 & aptaker == 1 & apskipper == 1 |
           discipline == "PHYS" & apyear >= 2015 & aptaker == 1 & apskipper == 1) 

# Model 1a: Credits ####
RQ1a <- df_full %>%
  group_by(discipline) %>%
  do(mod = glm(aptaker ~ factor(firstgen) + factor(lowincomflag)  + factor(female) + factor(urm) +
                 scale(hsgpa) + scale(mathsr) + scale(englsr) + factor(cohort),
               family = binomial, data = .))

RQ1a <- RQ1a %>%
  do(data.frame(
    discipline = .$discipline,
    var = names(coef(.$mod)),
    coef(summary(.$mod)),
    confint.default(.$mod, level = 0.95))
  )

# For Logistic: Create OR and 95% CI for OR 
RQ1a <- RQ1a %>%
  mutate(OR = exp(Estimate)) %>%
  mutate(OR_CILow = exp(X2.5..)) %>%
  mutate(OR_CIHi = exp(X97.5..))

write.csv(RQ1a, file = "SEISMIC_AP_Output_RQ1a.csv")

# Model 1b: Score ####
RQ1b <- df_aptakers %>%
  filter(discipline == "BIO" & apyear >= 2013 & aptaker == 1 |
           discipline == "CHEM" & apyear >= 2014 & aptaker == 1 |
           discipline == "PHYS" & apyear >= 2015 & aptaker == 1) %>%
  group_by(discipline) %>%
  do(mod = lm(scale(apscore) ~ firstgen + factor(lowincomflag)  + female + urm +
                scale(hsgpa) + scale(mathsr) + scale(englsr) + factor(crs_term),
                data = .))

RQ1b <- RQ1b %>%
  do(data.frame(
    discipline = .$discipline,
    var = names(coef(.$mod)),
    coef(summary(.$mod)),
    confint.default(.$mod, level = 0.95))
  )

write.csv(RQ1b, file = "SEISMIC_AP_Output_RQ1b.csv")

# Model 1c: Eligible to Skip ####
RQ1c <- df_full %>%
  filter(discipline == "BIO" & apyear >= 2013 |
           discipline == "CHEM" & apyear >= 2014 |
           discipline == "PHYS" & apyear >= 2015) %>%
  group_by(discipline) %>%
  do(mod = glm(apskipper ~ factor(firstgen) + factor(lowincomflag)  + factor(female) + factor(urm) +
                 scale(hsgpa) + scale(mathsr) + scale(englsr) + factor(cohort),
               family = binomial, data = .))

RQ1c <- RQ1c %>%
  do(data.frame(
    discipline = .$discipline,
    var = names(coef(.$mod)),
    coef(summary(.$mod)),
    confint.default(.$mod, level = 0.95))
  )

# For Logistic: Create OR and 95% CI for OR 
RQ1c <- RQ1c %>%
  mutate(OR = exp(Estimate)) %>%
  mutate(OR_CILow = exp(X2.5..)) %>%
  mutate(OR_CIHi = exp(X97.5..))

write.csv(RQ1c, file = "SEISMIC_AP_Output_RQ1c.csv")

#### RQ2 ####
# Model 2a: Skipped if eligible ####
RQ2a <- df_skipeligible %>%
  filter(discipline == "BIO" & apyear >= 2013 |
           discipline == "CHEM" & apyear >= 2014 |
           discipline == "PHYS" & apyear >= 2015) %>%
  group_by(discipline) %>%
  do(mod = glm(skipped ~ factor(firstgen) + factor(lowincomflag)  + factor(female) + factor(urm) +
                 scale(hsgpa) + scale(mathsr) + scale(englsr) + factor(enrl_from_cohort),
               family = binomial, data = .))

RQ2a_coef <- RQ2a %>%
  do(data.frame(
    discipline = .$discipline,
    var = names(coef(.$mod)),
    coef(summary(.$mod)),
    confint.default(.$mod, level = 0.95))
  )

# For Logistic: Create OR and 95% CI for OR 
RQ2a_coef <- RQ2a_coef %>%
  mutate(OR = exp(Estimate)) %>%
  mutate(OR_CILow = exp(X2.5..)) %>%
  mutate(OR_CIHi = exp(X97.5..))

write.csv(RQ2a_coef, file = "SEISMIC_AP_Output_RQ2a.csv")

# Model 2b: Grade for AP takers ####
RQ2b <- df_aptakers %>%
  filter(discipline == "BIO" & apyear >= 2013 |
           discipline == "CHEM" & apyear >= 2014 |
           discipline == "PHYS" & apyear >= 2015) %>%
  group_by(discipline) %>%
  do(mod = lm(scale(numgrade_2) ~ skipped + scale(apscore) + firstgen + factor(lowincomflag)  + female + urm +
                scale(hsgpa) + scale(mathsr) + scale(englsr) + factor(enrl_from_cohort),
                 data = .))

RQ2b <- RQ2b %>%
  do(data.frame(
    discipline = .$discipline,
    var = names(coef(.$mod)),
    coef(summary(.$mod)),
    confint.default(.$mod, level = 0.95))
  )

write.csv(RQ2b, file = "SEISMIC_AP_Output_RQ2b.csv")

# Model 2c: Grade for AP takers ####
RQ2c <- df_aptakers %>%
  filter(discipline == "BIO" & apyear >= 2013 |
           discipline == "CHEM" & apyear >= 2014 |
           discipline == "PHYS" & apyear >= 2015) %>%
  group_by(discipline) %>%
  do(mod = lm(scale(numgrade_2) ~ skipped + scale(apscore) + firstgen + factor(lowincomflag)  + female + urm +
                scale(hsgpa) + scale(mathsr) + scale(englsr) + factor(enrl_from_cohort),
              data = .))

RQ2b <- RQ2b %>%
  do(data.frame(
    discipline = .$discipline,
    var = names(coef(.$mod)),
    coef(summary(.$mod)),
    confint.default(.$mod, level = 0.95))
  )

write.csv(RQ2b, file = "SEISMIC_AP_Output_RQ2b.csv")
