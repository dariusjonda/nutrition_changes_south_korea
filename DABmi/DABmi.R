# Libraries
library(dplyr)

# Functions
## Inputs Dataframe and returns same Dataframe with UPPERCASE variable names
upper = function(df) {
  names(df) = toupper(names(df))
  df
}


# Analysis
## Computes BMI categorization for the Nutrition Survey KNHANES datasets from
## 1998 and 2015 using the following four categorizations:
## Underweight, Normal Range, Overweight, Obese
upper(HN98_ALL) %>%
  group_by(ID) %>%
  summarise(BMI = (HE_WT/(HE_HT/100)^2)) %>%
  filter(!is.na(BMI)) %>%
  mutate(BMI_CAT = ifelse(BMI < 18.5, "Underweight", 
                          ifelse((BMI >= 25 & BMI < 30), "Overweight", 
                                 ifelse(BMI >= 30, "Obese", "Normal range")))) %>%
  group_by(BMI_CAT) %>%
  summarise(N = n()) %>%
  mutate(RELFREQ = N/sum(N)) 

upper(HN15_ALL) %>%
  group_by(ID) %>%
  summarise(BMI = (HE_WT/(HE_HT/100)^2)) %>%
  filter(!is.na(BMI)) %>%
  mutate(BMI_CAT = ifelse(BMI < 18.5, "Underweight", 
                          ifelse((BMI >= 25 & BMI < 30), "Overweight", 
                                 ifelse(BMI >= 30, "Obese", "Normal range")))) %>%
  group_by(BMI_CAT) %>%
  summarise(N = n()) %>%
  mutate(RELFREQ = N/sum(N))


# t-test to compare changes in body mass index from 1998 -> 2015
BMI98 = upper(HN98_ALL) %>%
  group_by(ID) %>%
  summarise(BMI = (HE_WT/(HE_HT/100)^2)) %>%
  select(BMI) %>%
  unlist(.)

BMI15 = upper(HN15_ALL) %>%
  group_by(ID) %>%
  summarise(BMI = (HE_WT/(HE_HT/100)^2)) %>%
  select(BMI) %>%
  unlist(.)

var.test(BMI98, BMI15) 
# p value < 0.01 -> reject null hypothesis: variance is not equal
t.test(BMI98, BMI15, var.equal = F)
# p value < 0.01 -> reject null hypothesis -> means are signif. different