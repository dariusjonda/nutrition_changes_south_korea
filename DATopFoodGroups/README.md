
[<img src="https://github.com/QuantLet/Styleguide-and-FAQ/blob/master/pictures/banner.png" width="888" alt="Visit QuantNet">](http://quantlet.de/)

## [<img src="https://github.com/QuantLet/Styleguide-and-FAQ/blob/master/pictures/qloqo.png" alt="Visit QuantNet">](http://quantlet.de/) **DATopFoodGroups** [<img src="https://github.com/QuantLet/Styleguide-and-FAQ/blob/master/pictures/QN2.png" width="60" alt="Visit QuantNet 2.0">](http://quantlet.de/)

```yaml

Name of QuantLet : DATopFoodGroups

Published in : Bachelor Thesis "Nutrition Changes in South Korea"

Description : Summarizes food group intake by kilocalories from the 1998 and 2015 KNHANES datasets.

Keywords : 'South Korea, nutrition survey, KNHANES, food groups, nutrition, nutrition intake,
descriptive statistics'

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
## Inputs KNHANES nutrition survey dataset and seperates into existing
## food groups + seperates Alcohol, Kimchi, White Rice, Coffee, Bread into
## new food groups
FoodGroupAdd = function(df = NULL, year = NULL, agefilter = NULL) {
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

## Summarizes the kcal intake by foodgroups for each individual first and 
## for the entire population second.
FoodGroupRank = function(df = NULL, year = NULL) {
  df1 = FoodGroupAdd(df, year) %>%
    group_by(ID, FOOD_GROUP, NAME) %>%
    summarise(DAILY_INTAKE_KCAL = sum(NF_EN))
  
  dfreturn = df1 %>%
    group_by(FOOD_GROUP, NAME) %>%
    summarise(DailYIntGram = sum(DAILY_INTAKE_KCAL)/length(unique(df1$ID))) %>%
    arrange(desc(DailYIntGram))
  
  return(dfreturn)
}

## Inputs Dataframe and returns same Dataframe with UPPERCASE variable names
upper = function(df) {
  names(df) = toupper(names(df))
  df
}

## trim function removes unnecessary blank spaces
trim = function(x) gsub("^\\s+|\\s+$", "", x)

# Read in data
## read in 24 h recall examination files
files = list.files(pattern = "_24RC")
for (file in files) {
  td = as.data.frame(read.spss(paste0(file)), stringsAsFactors = F)
  td_name = substr(file, 0, 9)
  
  assign(td_name, td)
}

# Analysis
## Creates lookup table for foodgroups
foodgroup_db = data.frame(
  ID = 1:18,
  NAME = c("Grains", "Potatoes and Starch", "Sugars", "Legumes", 
           "Seeds and Nuts", "Other Vegetables", "Mushrooms", 
           "Fruits", "Meats", "Eggs", "Fish", "Seeweads", 
           "Milk and Dairy Products", "Fat and Oils", 
           "Beverages", "Seasonings", "Processed Foods", "Others"))

## Applies FoodGroupRank function on KNHANES Nutrition Survey datasets from 
## 1998 and 2015
foodgroup98 = FoodGroupRank(df = HN98_24RC, year = 98)
foodgroup15 = FoodGroupRank(df = HN15_24RC, year = 14)
```
