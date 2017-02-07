# Libraries
library(survey)
library(dplyr)
library(tidyr)

# Functions

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