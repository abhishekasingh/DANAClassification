# Similarity Coefficient

## Import Library
library(MASS)
library(philentropy)
library(dplyr)
library(tidyverse)

## Importing the dataset
social.data = read.csv('dataset/Social_Network_Ads.csv')

## Rename the columns
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
social.data$gender <- ifelse(social.data$gender == "Female", 0, 1)
social.data$age <- ifelse(social.data$age <= 37, 1, 0)
social.data$estimatedsalary <- ifelse(social.data$estimatedsalary <= 70000, 1, 0)

## Structure of data
str(social.data)

## Sample 10 oberservation from the dataset
##social.data <- social.data[sample(nrow(social.data), 10), 2:length(social.data)]

## Check the data
head(social.data)

## Simple Matching
simple.matching <- 1- distance(social.data,method = 'gower')
round(as.matrix(simple.matching)[1:5, 1:5], 1)

fviz_nbclust(simple.matching, kmeans, method = "wss") +
    geom_vline(xintercept = 3, linetype = 2)

sm.knn.res <- kmeans(simple.matching, 3, nstart = 50)
sm.knn.res

fviz_cluster(sm.knn.res, data = simple.matching,
             palette = c("#2E9FDF", "#00AFBB", "#E7B800", "#FC4E07"), 
             ellipse.type = "euclid", # Concentration ellipse
             star.plot = TRUE, # Add segments from centroids to items
             repel = TRUE, # Avoid label overplotting (slow)
             ggtheme = theme_minimal()
             )

## Jaccard 
jaccard.res <- distance(social.data,method = 'jaccard')
jaccard.res

fviz_nbclust(jaccard.res, kmeans, method = "wss") +
    geom_vline(xintercept = 3, linetype = 2)

jaccard.knn.res <- kmeans(jaccard.res, 3, nstart = 50)
jaccard.knn.res

fviz_cluster(jaccard.knn.res, data = jaccard.res,
             palette = c("#2E9FDF", "#00AFBB", "#E7B800", "#FC4E07"), 
             ellipse.type = "euclid", # Concentration ellipse
             star.plot = TRUE, # Add segments from centroids to items
             repel = TRUE, # Avoid label overplotting (slow)
             ggtheme = theme_minimal()
)

## Dice
dice.res <- distance(social.data,method = 'dice')
dice.res

fviz_nbclust(dice.res, kmeans, method = "wss") +
    geom_vline(xintercept = 3, linetype = 2)

dice.knn.res <- kmeans(dice.res, 3, nstart = 50)
dice.knn.res

fviz_cluster(dice.knn.res, data = dice.res,
             palette = c("#2E9FDF", "#00AFBB", "#E7B800", "#FC4E07"), 
             ellipse.type = "euclid", # Concentration ellipse
             star.plot = TRUE, # Add segments from centroids to items
             repel = TRUE, # Avoid label overplotting (slow)
             ggtheme = theme_minimal()
)