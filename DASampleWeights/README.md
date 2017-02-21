
[<img src="https://github.com/QuantLet/Styleguide-and-FAQ/blob/master/pictures/banner.png" width="888" alt="Visit QuantNet">](http://quantlet.de/)

## [<img src="https://github.com/QuantLet/Styleguide-and-FAQ/blob/master/pictures/qloqo.png" alt="Visit QuantNet">](http://quantlet.de/) **DASampleWeights** [<img src="https://github.com/QuantLet/Styleguide-and-FAQ/blob/master/pictures/QN2.png" width="60" alt="Visit QuantNet 2.0">](http://quantlet.de/)

```yaml

Name of QuantLet : DASampleWeights

Published in : Bachelor Thesis "Nutrition Changes in South Korea"

Description : Demographic analysis of the 1998 and 2016 KNHANES datasets using sample weights.

Keywords : 'South Korea, nutrition survey, KNHANES, nutrition, descriptive statistics, sample
weights, stratified survey, survey'

Author : Darius Jonda

Submitted : 2016-02-03 by Darius Jonda

Input : 'KNHANES Nutrient Survey datasets from 1998 - 2015. Datafiles not included in this folder
due to size of around 8 GB. Data can be downloaded from https://knhanes.cdc.go.kr'

```


### R Code:
```r
# Libraries
library(foreign)
library(survey)
library(dplyr)
library(tidyr)

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
## Application of survey weights
### 1998 
#### Compute BMI, Education Categories, Agegroups
HN98_ALL$BMI         = HN98_ALL$HE_WT/(HN98_ALL$HE_HT/100)^2
HN98_ALL$EDUC_GROUP  = ifelse(HN98_ALL$educ %in% c(4:7), ">=High school diploma", "<High school diploma")

HN98_ALL             = HN98_ALL %>%
  mutate(AGEGROUP = ifelse((age > 6 & age < 13), "7-12 y", 
                    ifelse((age > 12 & age < 19), "13-18 y",
                    ifelse((age > 18 & age < 40), "19-39 y",
                    ifelse((age > 39 & age < 60), "40-59 y", 
                    ifelse(age > 59, "60+ y",
                    ifelse((age < 7 & age > 1), "2-6 y", 
                    "<2y")))))))

#### initialize survey
svy98 = svydesign(ids = ~kstrata, data = HN98_ALL, weights = ~wt_itv)

#### Age
svymean(~age, design = svy98)

#### Sex
prop.table(svytable(~sex, design = svy98))

#### Agegroup
prop.table(svytable(~AGEGROUP, design = svy98))

#### bmi
svymean(~BMI, design = svy98, na.rm = T)

#### bmi by agegroup
svyttest(BMI ~ AGEGROUP, svy98)

#### bmi by sex
svymean(~BMI, design = subset(svy98, sex == "1"), na.rm = T)
svymean(~BMI, design = subset(svy98, sex == "2"), na.rm = T)
svyttest(BMI ~ sex, svy98)

#### bmi by EDUCGROUP
prop.table(svytable(~EDUC_GROUP, design = svy98))
svymean(~BMI, design = subset(svy98, EDUC_GROUP == ">=High school diploma"), na.rm = T)
svymean(~BMI, design = subset(svy98, EDUC_GROUP == "<High school diploma"), na.rm = T)
svyttest(BMI ~ EDUC_GROUP, svy98)



### 2015 
#### Compute BMI, Education Categories, Agegroups
HN15_ALL$BMI        = HN15_ALL$HE_wt/(HN15_ALL$HE_ht/100)^2
HN15_ALL$EDUC_GROUP = ifelse(HN15_ALL$educ %in% c(4:7), ">=High school diploma", "<High school diploma")

HN15_ALL            = HN15_ALL %>%
  mutate(AGEGROUP = ifelse((age > 6 & age < 13), "7-12 y", 
                    ifelse((age > 12 & age < 19), "13-18 y",
                    ifelse((age > 18 & age < 40), "19-39 y",
                    ifelse((age > 39 & age < 60), "40-59 y", 
                    ifelse(age > 59, "60+ y",
                    ifelse((age < 7 & age > 1), "2-6 y", 
                    "<2y")))))))

#### initialize survey with survey weights
svy15 = svydesign(ids = ~kstrata, data = HN15_ALL, weights = ~wt_hs)

#### Age
svymean(~age, design = svy15)

#### Sex
prop.table(svytable(~sex, design = svy15))

#### Agegroup
prop.table(svytable(~AGEGROUP, design = svy15))

#### bmi
svymean(~BMI, design = svy15, na.rm = T)

#### educgroup
prop.table(svytable(~EDUC_GROUP, design = svy15))

#### bmi by agegroup
svyttest(BMI ~ AGEGROUP, svy15)

#### bmi by sex
svymean(~BMI, design = subset(svy15, sex == "1"), na.rm = T)
svymean(~BMI, design = subset(svy15, sex == "2"), na.rm = T)
svyttest(BMI ~ sex, svy15)

#### bmi by EDUCGROUP
svymean(~BMI, design = subset(svy15, EDUC_GROUP == ">=High school diploma"), na.rm = T)
svymean(~BMI, design = subset(svy15, EDUC_GROUP == "<High school diploma"), na.rm = T)
svyttest(BMI ~ EDUC_GROUP, svy15)
```
