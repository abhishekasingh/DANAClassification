# Similarity Coefficient

## Import Library
library(MASS)
library(philentropy)

## Importing the dataset
social.data = read.csv('dataset/Social_Network_Ads.csv')


colnames(social.data)
colnames(social.data) <- tolower(make.names(colnames(social.data)))
colnames(social.data)

## Summary of the data
summary(social.data)
social.data <- social.data[1:5,2:length(social.data)]

## Converting into binary values
social.data$gender <- ifelse(social.data$gender == "Female", 0, 1)
social.data$age <- ifelse(social.data$age <= 37, 1, 0)
social.data$estimatedsalary <- ifelse(social.data$estimatedsalary <= 70000, 1, 0)

## Structure of data
str(social.data)

## Check the data
head(social.data)

## Simple Matching
sm <- 1- distance(social.data,method = 'gower')
sm
#k.res <- kmeans(sm,2)
#print(k.res)

## Jaccard 
jv <- distance(social.data,method = 'jaccard')
jv

## Dice
dv <- distance(social.data,method = 'dice')
dv