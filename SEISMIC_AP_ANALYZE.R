#SEISMIC AP Analysis
if (!require("pacman")) install.packages("pacman")
library(pacman)
pacman::p_load("tidyverse", "data.table", "psych", "summarytools", "haven", "Hmisc", "QuantPsyc")

#### RQ1 ####
# What student characteristics are associated with student participation and success in AP courses 
# for students enrolled at the selected universities?

# Model 1: Credits ####
#Bio
m1_BY <- glm(BY_CR ~ FG + LOW_INCOME  + FEMALE + URM +
            scale(HS_GPA) + scale(SAT_HIGH_MATH) + scale(SAT_HIGH_VERBAL), binomial(link = "logit"),df_sub)
summary(m1_BY)
exp(cbind("Odds Ratio" = coef(m1_BY), confint.default(m1_BY, level = 0.95)))

#Chem
m1_CH <- glm(CH_CR ~ FG + LOW_INCOME  + FEMALE + URM +
               scale(HS_GPA) + scale(SAT_HIGH_MATH) + scale(SAT_HIGH_VERBAL), binomial(link = "logit"),df_sub)
summary(m1_CH)   
exp(cbind("Odds Ratio" = coef(m1_CH), confint.default(m1_CH, level = 0.95)))

#Physics
m1_PHCM <- glm(PHCM_CR ~ FG + LOW_INCOME  + FEMALE + URM +
                 scale(HS_GPA) + scale(SAT_HIGH_MATH) + scale(SAT_HIGH_VERBAL), binomial(link = "logit"),df_sub)
summary(m1_PHCM)
exp(cbind("Odds Ratio" = coef(m1_PHCM), confint.default(m1_PHCM, level = 0.95)))


sink("m1_output.csv")
print(summary(m1_BY))
print(summary(m1_CH))
print(summary(m1_PHCM))
sink()

sink("m1_output_OR.csv")
print(exp(cbind("Odds Ratio" = coef(m1_BY), confint.default(m1_BY, level = 0.95))))
print(exp(cbind("Odds Ratio" = coef(m1_CH), confint.default(m1_CH, level = 0.95))))
print(exp(cbind("Odds Ratio" = coef(m1_PHCM), confint.default(m1_PHCM, level = 0.95))))
sink()

# Model 2: Score ####

#Bio
df_sub_BYtakers <- df_sub %>%
  subset(BY_CR > 0)
    m2_BY <- lm(BY ~ scale(FG) + scale(LOW_INCOME)  + scale(FEMALE) + scale(URM) +
              scale(HS_GPA) + scale(SAT_HIGH_MATH) + scale(SAT_HIGH_VERBAL) + scale(SAT_HIGH_WRIT), df_sub_BYtakers)
summary(m2_BY)
cbind("Beta" = coef(m2_BY), confint.default(m2_BY, level = 0.95))

#Chem
df_sub_CHtakers <- df_sub %>%
  subset(CH_CR > 0)
m2_CH <- lm(scale(CH) ~ scale(FG) + scale(LOW_INCOME)  + scale(FEMALE) + scale(URM) +
              scale(HS_GPA) + scale(SAT_HIGH_MATH) + scale(SAT_HIGH_VERBAL) + scale(SAT_HIGH_WRIT), df_sub)
summary(m2_CH)   
cbind("Beta" = coef(m2_CH), confint.default(m2_CH, level = 0.95))

#Physics
df_sub_PHCMtakers <- df_sub %>%
  subset(PHCM_CR > 0)
m2_PHCM <- lm(scale(PHCM) ~ scale(FG) + scale(LOW_INCOME)  + scale(FEMALE) + scale(URM) +
                scale(HS_GPA) + scale(SAT_HIGH_MATH) + scale(SAT_HIGH_VERBAL) + scale(SAT_HIGH_WRIT), df_sub)
summary(m2_PHCM)   
cbind("Beta" = coef(m2_PHCM), confint.default(m2_PHCM, level = 0.95))

sink("m2_output.csv")
print(summary(m2_BY))
print(summary(m2_CH))
print(summary(m2_PHCM))
sink()

sink("m2_output_CI.csv")
print(cbind("Beta" = coef(m2_BY), confint.default(m2_BY, level = 0.95)))
print(cbind("Beta" = coef(m2_CH), confint.default(m2_CH, level = 0.95)))
print(cbind("Beta" = coef(m2_PHCM), confint.default(m2_PHCM, level = 0.95)))
sink()


