# Installing libraries
library(tidyverse) # Including import and ggplot and dplyr
library(RColorBrewer) # Use it to change the colors "visualization"
library(ggplot2) # Using it with the visualization
library(dplyr) # Using it with the visualization
library(plyr) # Makes it simple to split data apart
library(forecast) # Using it with the visualization
library(bestNormalize)
library(visdat)
library(recipes)
library(caret)
library(rsample)
library(vip)
library(glmnet)
library(stringr)
library(rio) # R I/O # Use it to import the dataset

#######################################################################################

# Data set from: https://www.kaggle.com/stackoverflow/so-survey-2017

#######################################################################################

# Defined the data sets
surveyresult <- import("survey_results_public.csv")
schema <- import("survey_results_schema.csv")

#######################################################################################

# Exploring the data
glimpse(surveyresult)
summary(surveyresult)
class(surveyresult)
colnames(surveyresult) #154 Columns
rownames(surveyresult) # 51392 Rows
(nrow(data))
head(surveyresult)
typeof(surveyresult)

#######################################################################################

# Finding out what the must request language in the market, depending on the full-time employers results.

#######################################################################################

# Count each Employment status
surveyresult[, c("EmploymentStatus")]
count(surveyresult, 'EmploymentStatus')

#######################################################################################

# Filter the employment status with "Employed full-time", and make it as a dataframe to make it easier to work with.
EmploymentStatus <- surveyresult[, c("EmploymentStatus")]
EFT <- surveyresult %>% filter(EmploymentStatus == "Employed full-time")
count(EFT) # 36148 Rows

#######################################################################################

# Counting the languages depend on the EFT dataframe
# Extract the languages column
HaveWorkedLanguage <- surveyresult[, c("HaveWorkedLanguage")]

# Split the languages to different cells, because the cells are includes multiple languages in each.
lang <- data.frame(do.call('rbind',strsplit(as.character(EFT$HaveWorkedLanguage),';',fixed = TRUE)))
colnames(lang)

# To complete counting the frequency we should clean our data frame to collect the results for each language
# Chick the NA in the lang data frame
sum(is.na(lang))
# Clean the "lang" data frame by removing the NA
lang <- lang[!is.na(lang)]

# Count the frequency for the words
lang1 <- data.frame(table(unlist(strsplit(tolower(lang), " "))))
colnames(lang1)

# There is one empty cell, number without a language name
lang2 <- lang1[-c(1),] 

#######################################################################################

# ggplot2
# Using scales::comma because we have small numbers
ggplot(data= lang2, aes(x = Var1,
           y = Freq)) +
  geom_col(fill = "#9d8fdb") +
  scale_y_continuous(labels = scales::comma) +
  coord_flip() +
  labs(x = "Programming Lnguages",
       y = "Freq",
       title = "Languages Frequency") +
  theme_minimal()


# Using the 'reorder()' to organaise the plot more
ggplot(data= lang2, aes(x = reorder(Var1, -Freq) ,
                        y = Freq)) +
  geom_col(fill = "#9d8fdb") +
  scale_y_continuous(labels = scales::comma) +
  coord_flip() +
  labs(x = "Programming Lnguages",
       y = "Freq",
       title = "Languages Frequency") +
  theme_minimal()

#######################################################################################

# What the fresh graduates supposed to expect?

#######################################################################################

# Deep analyzing the must popular languages depending on the Formal Education.
EFT[, c("FormalEducation")]
count(EFT, 'FormalEducation')
FE <- EFT[, c("FormalEducation")]
FEfreq <- count(FE)

sum(is.na(FEfreq))

ggplot(FEfreq, aes(x= x, y=freq)) +
  geom_bar(stat="identity", fill="#f68060", alpha=.6, width=.4) +
  coord_flip() +
  xlab("") +
  theme_bw()

#######################################################################################

# Deep analyzing the must popular languages depending on the Professional.
surveyresult$Professional
EFT[, c("Professional")]
count(EFT, 'Professional')
Professionality <- EFT[, c("Professional")]
Professionalitydf <- count(Professionality)

ggplot(Professionalitydf, aes(x= x, y=freq)) +
  geom_bar(stat="identity", fill="#f68060", alpha=.6, width=.4) +
  coord_flip() +
  xlab("") +
  theme_bw()














