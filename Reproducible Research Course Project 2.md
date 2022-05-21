---
title: "Reproducible Research Course Project 2"
author: "Choong-Hoon Hyun"
date: "5/18/2022"
output:
  html_document: default
  pdf_document: default
---

## Title: Which Types of Severe Weather Events are making significant impacts on the public health and economy?: Analysis of NOAA Storm database.

## Assignment
### The basic goal of this assignment is to explore the NOAA Storm Database and answer some basic questions about severe weather events. You must use the database to answer the questions below and show the code for your entire analysis. Your analysis can consist of tables, figures, or other summaries. You may use any R package you want to support your analysis.

## 1. Synopsis
### Severe weather events make a huge impact on the public health and economy. They can result in fatalities, injuries, property damage, and crop damage. This project will explore the given storms and weather events dataset from the U.S. National Oceanic and Atmospheric Administration (NOAA). The events in the dataset start in 1950 and finish in November 2011. We will conclude
#### - The top 10 harmful weather events associated with the population health
#### - Top top 10 weather events causing economic consequences

## 2. Data Processing
### 2.1. Loading raw dataset into R
#### We download the [storm dataset](https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2). The data comes in the form of a comma-separated-value (CSV) file compressed via the bzip2 algorithm. We read the file with the "read.csv" function. 

```r
fileUrl <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2"
f <- file.path(getwd(), "StormData.csv")
download.file(fileUrl, f, curl = "method")
StormDataset <- read.csv("StormData.csv")
```

### 2.2. Exploring Columns of the dataset
#### After loading the raw data, we explore the columns in the dataset to decide relevant columns for the assignment. 

```r
colnames(StormDataset)
```

```
##  [1] "STATE__"    "BGN_DATE"   "BGN_TIME"   "TIME_ZONE"  "COUNTY"     "COUNTYNAME" "STATE"      "EVTYPE"     "BGN_RANGE" 
## [10] "BGN_AZI"    "BGN_LOCATI" "END_DATE"   "END_TIME"   "COUNTY_END" "COUNTYENDN" "END_RANGE"  "END_AZI"    "END_LOCATI"
## [19] "LENGTH"     "WIDTH"      "F"          "MAG"        "FATALITIES" "INJURIES"   "PROPDMG"    "PROPDMGEXP" "CROPDMG"   
## [28] "CROPDMGEXP" "WFO"        "STATEOFFIC" "ZONENAMES"  "LATITUDE"   "LONGITUDE"  "LATITUDE_E" "LONGITUDE_" "REMARKS"   
## [37] "REFNUM"
```

### 2.3. Loading Dplyr package to subset relevant columns.
#### Dplyr package is loaded as we can efficiently manipulate a dataset in R.

```r
library(dplyr) 
```

```
## 
## Attaching package: 'dplyr'
```

```
## The following objects are masked from 'package:stats':
## 
##     filter, lag
```

```
## The following objects are masked from 'package:base':
## 
##     intersect, setdiff, setequal, union
```

#### Since we will investigate the storms and weather events to conclude the top 10 harmful weather events on the public health and economy, we will subset COUNTYNAME, STATE, EVTYPE, MAG, FATALITIES, INJURIES, PROPDMG, PROPDMGEXP, CROPDMG, CROPDMGEXP out of the 37 columns.

```r
Storm_DataSet <- StormDataset %>% 
  select(COUNTYNAME, STATE, EVTYPE, MAG, FATALITIES, INJURIES, PROPDMG, PROPDMGEXP, CROPDMG, CROPDMGEXP)
```

#### We only need values greater than 0 which means we need entries being made to have an impact on health and property/crop damage. We filter FATALITIES, INJURIES, PROPDMG and CROPDMG columns in Storm_DataSet.

```r
Storm_dataSet_use <- Storm_DataSet %>% filter(FATALITIES > 0 | INJURIES > 0 | PROPDMG > 0 | CROPDMG > 0)
```

#### Some exponential alphabets are in lowercase letters. Lowercase letters and uppercase ones are indicating the same numeric values. For instance, k means 1 thousand (1,000). So does K. We change the lower exponential alphabet to the upper one to remove confusion.

```r
Storm_dataSet_use$EVTYPE <- toupper(Storm_dataSet_use$EVTYPE)
Storm_dataSet_use$PROPDMGEXP <- toupper(Storm_dataSet_use$PROPDMGEXP)
Storm_dataSet_use$CROPDMGEXP <- toupper(Storm_dataSet_use$CROPDMGEXP)
```

#### We change alphanumeric exponential values to numeric ones for property damage to calculate the accurate numeric values.

```r
Storm_dataSet_use$PROPDMGEXP[which(Storm_dataSet_use$PROPDMGEXP == "")] <- 0
PROPDMG_in_number <- c("-" = 10^0, "+" = 10^0, "0" = 10^0, "1" = 10^1, "2" = 10^2, "3" = 10^3, "4" = 10^4, "5" = 10^5, 
                       "6" = 10^6, "7" = 10^7, "B" = 10^9, "H" = 10^2, "K" = 10^3, "M" = 10^6)
PROPDMG_in_number_df <- data.frame(PROPDMG_in_number)
PROPDMG_in_number_df2 <- cbind(rownames(PROPDMG_in_number_df), data.frame(PROPDMG_in_number_df, row.names = NULL))
names(PROPDMG_in_number_df2) <- c("PROPDMGEXP", "PROPDMGEXP_in_Number")
SD_updated_PROPDMGEXP <- left_join(Storm_dataSet_use, PROPDMG_in_number_df2, by = "PROPDMGEXP")
```

#### We change alphanumeric exponential values to numeric ones for crop damage to calculate the accurate numeric values.

```r
SD_updated_PROPDMGEXP$CROPDMGEXP[which(SD_updated_PROPDMGEXP$CROPDMGEXP == "")] <- 0
CROPDMG_in_number <- c("?" = 10^0, "0" = 10^0, "1" = 10^1, "B" = 10^9, "K" = 10^3, "M" = 10^6)
CROPDMG_in_number_df <- data.frame(CROPDMG_in_number)
CROPDMG_in_number_df2 <- cbind(rownames(CROPDMG_in_number_df), data.frame(CROPDMG_in_number_df, row.names = NULL))
names(CROPDMG_in_number_df2) <- c("CROPDMGEXP", "CROPDMGEXP_in_Number")
SD_updated_PROPDMGEXP_CROPDMGEXP <- left_join(SD_updated_PROPDMGEXP, CROPDMG_in_number_df2, by = "CROPDMGEXP")
```

#### We make a new column to show the total property damage in numeric value by multiplying PROPDMG and PROPDMGEXP_in_Number and to make the total crop damage in numeric value by multiplying CROPDMG and CROPDMGEXP_in_Number.

```r
SD_updated_PROPDMGEXP_CROPDMGEXP$total_PROPDMG <- SD_updated_PROPDMGEXP_CROPDMGEXP$PROPDMG * SD_updated_PROPDMGEXP_CROPDMGEXP$PROPDMGEXP_in_Number
SD_updated_PROPDMGEXP_CROPDMGEXP$total_CROPDMG <- SD_updated_PROPDMGEXP_CROPDMGEXP$CROPDMG * SD_updated_PROPDMGEXP_CROPDMGEXP$CROPDMGEXP_in_Number
```

#### We make a column for the total economic damage cost by property damage and crop damage. This will show us the total economic impact by the weather events. 

```r
SD_updated_PROPDMGEXP_CROPDMGEXP$total_economic_DMG <- SD_updated_PROPDMGEXP_CROPDMGEXP$total_PROPDMG + SD_updated_PROPDMGEXP_CROPDMGEXP$total_CROPDMG
```

#### We combine FATALITIES and INJURIES columns to show the total health impact by the weather events.  

```r
SD_updated_PROPDMGEXP_CROPDMGEXP$combined_fatalities_injuries <- SD_updated_PROPDMGEXP_CROPDMGEXP$FATALITIES + SD_updated_PROPDMGEXP_CROPDMGEXP$INJURIES
```

#### We clean the dataset (SD_updated_PROPDMGEXP_CROPDMGEXP) by removing leading and trailing spaces in EVTYPE column to get the accurate results. We use the "trimws" function. 

```r
SD_updated_PROPDMGEXP_CROPDMGEXP$EVTYPE <- trimws(SD_updated_PROPDMGEXP_CROPDMGEXP$EVTYPE, which = c("both")) 
```

## 3. Results
###  Q1. Across the United States, which types of events (as indicated in the EVTYPE variable) are most harmful with respect to population health?

#### We subset columns in terms of the population health from the processed dataset (SD_updated_PROPDMGEXP_CROPDMGEXP).

```r
dataset_harmful_population_health <- SD_updated_PROPDMGEXP_CROPDMGEXP %>% select(EVTYPE, FATALITIES, INJURIES, combined_fatalities_injuries)
```

#### We make the dataset grouped by the EVTYPE and subset the top 10 harmful event types associated with the population health. 

```r
dataset_harmful_by_EVTYPE <- dataset_harmful_population_health  %>% group_by(EVTYPE) %>% 
  summarise(total_fatalities = sum(FATALITIES), total_injuries = sum(INJURIES), total = sum(combined_fatalities_injuries)) %>%
  arrange(desc(total))
dataset_harmful_by_EVTYPE_top_10 <- dataset_harmful_by_EVTYPE[1:10,]
```

#### Installing the "reshape2" package to show three factors (total_facilities, total_injuries, and total) in one plot.

```r
library(reshape2)
```

#### The "melt" function makes "total_fatalities", "total_injuries", and "total" columns as a variable to show relevant values in one plot.

```r
dataset_harmful_reshape <- melt(dataset_harmful_by_EVTYPE_top_10, id.vars = "EVTYPE")
```

#### Installing ggplot2 package

```r
library(ggplot2)
```

#### We make a plot to show the 10 harmful events with respect to population health.

```r
ggplot(dataset_harmful_reshape, aes(reorder(EVTYPE, -value), value, fill = variable)) + geom_bar(stat = "identity", position = "dodge") +
  labs(x = "Event Type", y = "Harmful Event Counts", title = "Top 10 Harmful Events With Respect To Population Health") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))
```

![plot of chunk Make a plot to show the 10 harmful events with respect to population health](<figure/Make a plot to show the 10 harmful events with respect to population health-1.png>)

#### Findings:
* **Tornadoes** are **the most harmful event** for both fatalities and injuries. The ranking is followed by Excessive Heat, TSTM Wind, Flood, Lightning, Heat, Flash Flood, Ice Storm, Thunderstorm Wind, and Winter Storm. 
* **Tornadoes** inflict by far more **injuries** than fatalities. **Excessive Heat, TSTM Wind, Flood, and Lightning** are also a **contributor** to **injuries**. 


###  Q2. Across the United States, which types of events have the greatest economic consequences?

#### We subset columns in terms of the economic consequences from the processed dataset (SD_updated_PROPDMGEXP_CROPDMGEXP).

```r
dataset_greatest_economic_consequences <- SD_updated_PROPDMGEXP_CROPDMGEXP %>% select(EVTYPE, total_PROPDMG, total_CROPDMG, total_economic_DMG)
```

#### We make the dataset grouped by the EVTYPE and subset the top 10 harmful event types in terms of the economic consequences.

```r
economic_consequences_by_EVTYPE <- dataset_greatest_economic_consequences %>% group_by(EVTYPE) %>%
  summarise(total_property_damage = sum(total_PROPDMG), total_crop_damage = sum(total_CROPDMG), total = sum(total_economic_DMG)) %>%
  arrange(desc(total))
economic_consequences_by_EVTYPE_top_10 <- economic_consequences_by_EVTYPE[1:10,]
```

#### We use the "melt" function in the reshape2 package to show three factors (total_property_damage, total_crop_damage, and total) in one plot by making the three factors as variables. 

```r
economic_consequences_reshape <- melt(economic_consequences_by_EVTYPE_top_10, id.vars = "EVTYPE")
```

#### We make a plot to show the top 10 weather events causing economic consequences.

```r
ggplot(economic_consequences_reshape, aes(reorder(EVTYPE, -value), value/10^6, fill = variable)) + geom_bar(stat = "identity", position = "dodge") +
  labs(x = "Event Type", y = "Cost (Million Dollar)", title = "Top 10 Weather Events Causing Economic Consequences") + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))
```

![plot of chunk Make a plot for the top 10 economic consequences](<figure/Make a plot for the top 10 economic consequences-1.png>)

#### Findings:
* With regard to the **combined damage costs** of property damage and crop damage, **flood** causes **the most significant economic consequences**. The ranking is followed by Hurricane/Typhoon, Tornado, Storm Surge, Hail, Flash Flood, Drought, Hurricane, River Flood, and Ice Storm.
* In terms of the **property damage**, **flood** is **the most contributor** to the damage. It is followed by Hurricane/Typhoon. Tornado is ranked at the third, and Storm Surge is the fourth contributor. Flash Flood, Hail, Hurricane, River Flood, Ice Storm, and Drought are followed.
* For **crop damage**, **drought** is **the most significant cause** of economic consequences. It is followed by Flood, River Flood, Ice Storm, Hail, Hurricane, Hurricane/Typhoon, Flash Flood, Tornado, and Storm Surge. 
* While **flood** is **the most significant contributor** to the **property damage**, **drought** is ranked at the **top contributor** to the **crop damage**. 
