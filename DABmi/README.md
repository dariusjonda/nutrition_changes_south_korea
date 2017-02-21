
[<img src="https://github.com/QuantLet/Styleguide-and-FAQ/blob/master/pictures/banner.png" width="888" alt="Visit QuantNet">](http://quantlet.de/)

## [<img src="https://github.com/QuantLet/Styleguide-and-FAQ/blob/master/pictures/qloqo.png" alt="Visit QuantNet">](http://quantlet.de/) **DABmi** [<img src="https://github.com/QuantLet/Styleguide-and-FAQ/blob/master/pictures/QN2.png" width="60" alt="Visit QuantNet 2.0">](http://quantlet.de/)

```yaml

Name of QuantLet : DABmi

Published in : Bachelor Thesis "Nutrition Changes in South Korea"

Description : Compares changes in Body Mass Index for the KNHANES 1998 and 2015 datasets

Keywords : 'South Korea, nutrition survey, KNHANES, BMI, nutrition, body mass index, descriptive
statistics'

Author : Darius Jonda

Submitted : 2016-02-02 by Darius Jonda

Input : 'KNHANES Nutrient Survey datasets from 1998 - 2015. Datafiles not included in this folder
due to size of around 8 GB. Data can be downloaded from https://knhanes.cdc.go.kr'

```


### R Code:
```r
# Libraries
library(dplyr)
library(tidyr)
library(foreign)

# Functions
## Inputs Dataframe and returns same Dataframe with UPPERCASE variable names
upper = function(df) {
  names(df) = toupper(names(df))
  df
}

# Read in data
## read in health examination data
files_all = list.files(pattern = "_ALL")
for (file in files_all) {
  td = as.data.frame(read.spss(paste0(file), reencode = "UTF-8"))
  td_name = substr(file, 0, 8)
  
  assign(td_name, td)
  print(td_name)
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
```
