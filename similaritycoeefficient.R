# Similarity Coefficient

## Import Library
list.of.packages <- c("MASS", "philentropy","dplyr","tidyverse", "tablerDash","ggpol","XML")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

library(MASS)
library(philentropy)
library(dplyr)
library(tidyverse)
library(ggpol)

## Importing the dataset
social.data = read.csv('dataset/maindataset/Social_Network_Ads.csv')


colnames(social.data)
colnames(social.data) <- tolower(make.names(colnames(social.data)))
colnames(social.data)

## Summary of the data
summary(social.data)

## Explanatory data analysis

### Gender
gender.plt <- social.data %>% 
    group_by(gender) %>% 
    summarise(counts = n()) %>% 
    ggplot(aes(gender, counts)) +
    geom_bar(stat = "identity", width=0.5, fill="steelblue") +
    geom_text(aes(label=counts), vjust=1.6, color="white", size=4.0, fontface = "bold")+
    ggtitle("Count of People by Gender") + 
    xlab("Gender") +
    ylab("Counts") +
    theme_minimal() +
    theme(plot.title = element_text(hjust = 0.5))
    
print(gender.plt)

### Age
groupAge <- function(age){
    if(age <= 15){
        return(as.factor("0-15"))  
    }else if( (age >= 16) & (age <= 30) ){
        return(as.factor("16-30"))
    }else if( (age >= 31) & (age <= 45) ){
        return(as.factor("31-45"))
    }else if( (age >= 46) & (age <= 60) ){
        return(as.factor("46-60"))
    }else if( (age >= 61) & (age <= 75) ){
        return(as.factor("61-75"))
    }
}
social.data$age.band <- sapply(social.data$age, groupAge)


social.data.age <- social.data %>% 
    select(age.band, gender)

age.plt <- ggplot(data=social.data.age, aes(x=as.factor(age.band),fill=gender)) + 
    geom_bar(data=subset(social.data.age, gender =="Female")) + 
    geom_bar(data=subset(social.data.age, gender =="Male"), aes(y=..count..*(-1))) + 
    scale_y_continuous(breaks=seq(-150,150,10),labels=abs(seq(-150,150,10))) + 
    xlab("Age Band") +
    ylab("Count") +
    coord_flip() +
    ggtitle("Age Distribution") +
    theme_minimal() +
    theme(plot.title = element_text(hjust = 0.5))

print(age.plt)

## Converting into binary values

social.data <- social.data %>%
    select(gender, age, estimatedsalary,purchased) %>% 
    mutate(gender = ifelse(social.data$gender == "Female", 0, 1) ) %>%
    mutate(age = ifelse(social.data$age <= 37, 1, 0) ) %>%
    mutate(gender = ifelse(social.data$estimatedsalary <= 70000, 1, 0) )
    

social.data$gender <- ifelse(social.data$gender == "Female", 0, 1)
social.data$age <- ifelse(social.data$age <= 37, 1, 0)
social.data$estimatedsalary <- ifelse(social.data$estimatedsalary <= 70000, 1, 0)

## Structure of data
str(social.data)

social.data <- social.data[sample(nrow(social.data), 10), 2:length(social.data)]

## Check the data
head(social.data)

## Simple Matching
simple.match.data <- 1- distance(social.data,method = 'gower')
simple.match.data

## Jaccard 
jaccard.data <- distance(social.data,method = 'jaccard')
jaccard.data

## Dice
dice.data <- distance(social.data,method = 'dice')
dice.data