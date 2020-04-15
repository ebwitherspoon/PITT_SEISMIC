#### Setup ####
# Load packages and data
if (!require("pacman")) install.packages("pacman")
library(pacman)
pacman::p_load("tidyverse")

# Load Clean Dataframe ####
df_clean <- read.csv("~/Box Sync/LSAP_LRDC/Research Projects/SEISMIC/AP/SEISMIC_AP/SEISMIC_AP_CLEAN.csv")
names(df_clean)

# Model 1a: Credits ####
RQ1a <- df_clean %>%
  filter(discipline == "BIO" & apyear >= 2013 |
           discipline == "CHEM" & apyear >= 2014 |
           discipline == "PHYS" & apyear >= 2015) %>%
  group_by(discipline) %>%
  do(mod = glm(aptaker ~ factor(firstgen) + factor(lowincomflag)  + factor(female) + factor(urm) +
                 scale(hsgpa) + scale(mathsr) + scale(englsr) + factor(enrl_from_cohort),
               family = binomial, data = .))

RQ1a_coef <- RQ1a %>%
  do(data.frame(
    discipline = .$discipline,
    var = names(coef(.$mod)),
    coef(summary(.$mod)),
    confint.default(.$mod, level = 0.95))
  )

# For Logistic: Create OR and 95% CI for OR 
RQ1a_coef <- RQ1a_coef %>%
  mutate(OR = exp(Estimate)) %>%
  mutate(OR_CILow = exp(X2.5..)) %>%
  mutate(OR_CIHi = exp(X97.5..))

write.csv(RQ1a_coef, file = "SEISMIC_AP_Output_RQ1a.csv")