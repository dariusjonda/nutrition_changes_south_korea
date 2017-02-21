
[<img src="https://github.com/QuantLet/Styleguide-and-FAQ/blob/master/pictures/banner.png" width="888" alt="Visit QuantNet">](http://quantlet.de/)

## [<img src="https://github.com/QuantLet/Styleguide-and-FAQ/blob/master/pictures/qloqo.png" alt="Visit QuantNet">](http://quantlet.de/) **SAMacronutrientChanges** [<img src="https://github.com/QuantLet/Styleguide-and-FAQ/blob/master/pictures/QN2.png" width="60" alt="Visit QuantNet 2.0">](http://quantlet.de/)

```yaml

Name of QuantLet : SAMacronutrientChanges

Published in : Bachelor Thesis "Nutrition Changes in South Korea"

Description : 'Computes t-test to compare kcal intake by macronutrients from KNHANES nutrition
survey of 1998 and 2015'

Keywords : 'South Korea, nutrition survey, KNHANES, t-test, nutrition, test statistics, descriptive
statistics'

Author : Darius Jonda

Submitted : 2016-02-02 by Darius Jonda

Input : 'KNHANES Nutrient Survey datasets from 1998 - 2015. Datafiles not included in this folder
due to size of around 8 GB. Data can be downloaded from https://knhanes.cdc.go.kr'

```


### R Code:
```r
# Libraries
library(foreign)
library(dplyr)
library(tidyr)

# Functions
## Inputs Dataframe and returns same Dataframe with UPPERCASE variable names
upper = function(df) {
  names(df) = toupper(names(df))
  df
}

# Read in data
## read in 24 h recall examination files
files = list.files(pattern = "_24RC")
for (file in files) {
  td = as.data.frame(read.spss(paste0(file)), stringsAsFactors = F)
  td_name = substr(file, 0, 9)
  
  assign(td_name, td)
}

# Analysis
## t-tests to compare intake of total consumed kilocalories, fats, proteins,
## carbohydrates in 1998 and 2015 KNHANES nutrition survey datasets
### TOTAL ENERGY INTAKE
en98 = upper(HN98_24RC) %>%
  group_by(ID) %>%
  summarise(SUM_FAT = sum(NF_EN)) %>%
  select(SUM_FAT) %>%
  unlist(.)

en15 = upper(HN15_24RC) %>%
  group_by(ID) %>%
  summarise(SUM_FAT = sum(NF_EN)) %>%
  select(SUM_FAT) %>%
  unlist(.)

var.test(en98, en15)
t.test(en98, en15, var.equal = F)

### FAT INTAKE
fat98 = upper(HN98_24RC) %>%
  group_by(ID) %>%
  summarise(SUM_FAT = sum(NF_FAT*9)) %>%
  select(SUM_FAT) %>%
  unlist(.)

fat15 = upper(HN15_24RC) %>%
  group_by(ID) %>%
  summarise(SUM_FAT = sum(NF_FAT*9)) %>%
  select(SUM_FAT) %>%
  unlist(.)

var.test(fat98, fat15)
t.test(fat98, fat15, var.equal = F)

### PROTEIN INTAKE
PROT98 = upper(HN98_24RC) %>%
  group_by(ID) %>%
  summarise(SUM_PROT = sum(NF_PROT*4)) %>%
  select(SUM_PROT) %>%
  unlist(.)

PROT15 = upper(HN15_24RC) %>%
  group_by(ID) %>%
  summarise(SUM_PROT = sum(NF_PROT*4)) %>%
  select(SUM_PROT) %>%
  unlist(.)

var.test(PROT98, PROT15)
t.test(PROT98, PROT15, var.equal = F)


### CARBOHYDRATE INTAKE
CHO98 = upper(HN98_24RC) %>%
  group_by(ID) %>%
  summarise(SUM_CHO = sum(NF_CHO*4)) %>%
  select(SUM_CHO) %>%
  unlist(.)

CHO15 = upper(HN15_24RC) %>%
  group_by(ID) %>%
  summarise(SUM_CHO = sum(NF_CHO*4)) %>%
  select(SUM_CHO) %>%
  unlist(.)

var.test(CHO98, CHO15)
# p value = 0.2313 > 0.01 -> null hyp. not rejected: variances equal
t.test(CHO98, CHO15, var.equal = T)
# -> p value = 0.00139 < 0.01 -> null hyp rejected: means not equal
```
