
[<img src="https://github.com/QuantLet/Styleguide-and-FAQ/blob/master/pictures/banner.png" width="888" alt="Visit QuantNet">](http://quantlet.de/)

## [<img src="https://github.com/QuantLet/Styleguide-and-FAQ/blob/master/pictures/qloqo.png" alt="Visit QuantNet">](http://quantlet.de/) **SAClusterAnalysis** [<img src="https://github.com/QuantLet/Styleguide-and-FAQ/blob/master/pictures/QN2.png" width="60" alt="Visit QuantNet 2.0">](http://quantlet.de/)

```yaml

Name of QuantLet : SAClusterAnalysis

Published in : Bachelor Thesis "Nutrition Changes in South Korea"

Description : 'Computes k-means Cluster Analysis on 1998 and 2015 KNHANES Nutrition Survey dataset
aiming to derive key dietary patterns based on 23 food groups'

Keywords : 'South Korea, nutrition survey, KNHANES, food groups, nutrition, cluster analysis,
k-means, dietary patterns, elbow criterion, silhouette measure'

Author : Darius Jonda

Submitted : 2016-02-02 by Darius Jonda

Input : 'KNHANES Nutrient Survey datasets from 1998 - 2015. Datafiles not included in this folder
due to size of around 8 GB. Data can be downloaded from https://knhanes.cdc.go.kr'

```

![Picture1](PlotElbowCriterion1998.png)

![Picture2](PlotSilhouette1998.png)


### R Code:
```r
# Libraries
library(foreign)
library(dplyr)
library(tidyr)
library(factoextra)

# Functions
## Inputs KNHANES nutrition survey dataset and seperates into existing
## food groups + seperates Alcohol, Kimchi, White Rice, Coffee, Bread into
## new food groups
FoodGroupAdd = function(df = NULL, year = NULL, agefilter = NULL) {
  foodgroup_db = data.frame(
    ID = 1:18,
    NAME = c("Grains", "Potatoes and Starch", "Sugars", "Legumes", 
             "Seeds and Nuts", "Other Vegetables", "Mushrooms", 
             "Fruits", "Meats", "Eggs", "Fish", "Seeweads", 
             "Milk and Dairy Products", "Fat and Oils", 
             "Beverages", "Seasonings", "Processed Foods", "Others"))
  
  
  df_fg = upper(df) %>%
    mutate(FOOD_GROUP = as.numeric(substr(N_FCODE, 1, 2))) 
  
  if(!is.null(agefilter)) {
    df_fg = df_fg %>%
      filter(AGE > agefilter)
  }
  
  df_fg_wname = merge(df_fg, foodgroup_db, 
                       by.x = "FOOD_GROUP", 
                       by.y = "ID") %>%
    mutate(NAME = as.character(NAME))
  
  
  if (year == 98) {
    alc = as.factor(c(15031:15062))
    kimchi = paste0("0", c(6045:6057))
    whiterice = paste0("0", c(1157:1167))
    coffee = as.character(c(15012:15018))
    bread = paste0("0", c(1049:1071))
    df_fg_wname$NAME[trim(df_fg_wname$N_FCODE) %in% alc] = "Alcohol"
    df_fg_wname$NAME[trim(df_fg_wname$N_FCODE) %in% kimchi] = "Kimchi"
    df_fg_wname$NAME[trim(df_fg_wname$N_FCODE) %in% whiterice] = "White Rice"
    df_fg_wname$NAME[trim(df_fg_wname$N_FCODE) %in% coffee] = "Coffee"
    df_fg_wname$NAME[trim(df_fg_wname$N_FCODE) %in% bread] = "Bread"
  } else if (year == 14) {
    alc = as.character(c(15026:15060))
    kimchi = paste0("0", c(6057:6070))
    whiterice = paste0("0", c(1173:1182))
    coffee = as.character(c(15083:15088))
    bread = paste0("0", c(1053:1076))
    df_fg_wname$NAME[trim(df_fg_wname$N_FCODE) %in% alc] = "Alcohol"
    df_fg_wname$NAME[trim(df_fg_wname$N_FCODE) %in% kimchi] = "Kimchi"
    df_fg_wname$NAME[trim(df_fg_wname$N_FCODE) %in% whiterice] = "White Rice"
    df_fg_wname$NAME[trim(df_fg_wname$N_FCODE) %in% coffee] = "Coffee"
    df_fg_wname$NAME[trim(df_fg_wname$N_FCODE) %in% bread] = "Bread"
  }
  return(df_fg_wname)
}

## trim function removes unnecessary blank spaces
trim = function(x) gsub("^\\s+|\\s+$", "", x)

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

# Data Preparation
## Read in 1998 and 2015 datasets, filter by more than 5000 and less than 500
## kcal intake by day
### 1998 
df1 = FoodGroupAdd(HN98_24RC, 98) %>%
  group_by(ID, NAME) %>%
  summarise(DAILY_INTAKE_KCAL = sum(NF_EN)) %>%
  mutate(DAILY_INTAKE_RELATIVE = DAILY_INTAKE_KCAL/sum(DAILY_INTAKE_KCAL)) %>%
  select(ID, NAME, INTK_GRAM = DAILY_INTAKE_RELATIVE)

filternames = upper(HN98_24RC) %>%
  group_by(ID) %>%
  summarise(NF_EN = sum(NF_EN, na.rm = T)) %>%
  filter(NF_EN > 500,
         NF_EN < 5000) %>%
  select(ID) %>%
  unlist(.)

allnames = upper(HN98_24RC) %>%
  group_by(ID) %>%
  summarise(NF_EN = sum(NF_EN, na.rm = T)) %>%
  select(ID) %>%
  unlist(.)

df2 = df1 %>%
  spread(NAME, INTK_GRAM) %>%
  filter(ID %in% filternames)

dfana1998 = df2[ ,-1]
dfana1998[is.na(dfana1998)] = 0

### 2015 
df1 = FoodGroupAdd(HN15_24RC, 14) %>%
  group_by(ID, NAME) %>%
  summarise(DAILY_INTAKE_KCAL = sum(NF_EN)) %>%
  mutate(DAILY_INTAKE_RELATIVE = DAILY_INTAKE_KCAL/sum(DAILY_INTAKE_KCAL)) %>%
  select(ID, NAME, INTK_GRAM = DAILY_INTAKE_RELATIVE)

filternames = upper(HN15_24RC) %>%
  group_by(ID) %>%
  summarise(NF_EN = sum(NF_EN, na.rm = T)) %>%
  filter(NF_EN > 500,
         NF_EN < 5000) %>%
  select(ID) %>%
  unlist(.)

allnames = upper(HN15_24RC) %>%
  group_by(ID) %>%
  summarise(NF_EN = sum(NF_EN, na.rm = T)) %>%
  select(ID) %>%
  unlist(.)

length(allnames) - length(filternames)

df2 = df1 %>%
  spread(NAME, INTK_GRAM) %>%
  filter(ID %in% filternames)

dfana2015 = df2[ ,-1]
dfana2015[is.na(dfana2015)] = 0

# Analysis
## Cluster Analysis for 1998 and 2015 KNHANES datasets
## Elbow Criterion and Silhouette Measure are being computed using the 
## fviz_nbclust() function from the 'factoextra' package 

### 1998
#### Elbow / Silhouette
mydata98 = scale(dfana1998)
fviz_nbclust(mydata98, kmeans, method = "wss") 
fviz_nbclust(mydata98, kmeans, method = "silhouette")

#### k-means analysis
kmcl98 = kmeans(dfana1998, 2, 100)

### 2015
#### Elbow / Silhouette
mydata15 = scale(dfana2015)
fviz_nbclust(mydata15, kmeans, method = "wss") 
fviz_nbclust(mydata15, kmeans, method = "silhouette")

#### k-means analysis
kmcl15 = kmeans(dfana2015, 2, 100)

```
