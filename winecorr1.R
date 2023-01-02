getwd()
setwd("Your File location")

install.packages("readr")
install.packages("dplyr")
install.packages("tidyr")
install.packages("ggplot2")
install.packages('corrplot')
install.packages("readxl")
install.packages("psych")
install.packages("GGally")
install.packages("gmodels")
install.packages("corrplot")
install.packages("Hmisc")

library(readr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(corrplot)
library(readxl)
library(psych)
library(GGally)
library(gmodels)
library(corrplot)
library(Hmisc)

### ASSOCIATION BETWEEN VARIABLES ANALYSIS ###
### ASSOCIATION BETWEEN VARIABLES ANALYSIS ###
### ASSOCIATION BETWEEN VARIABLES ANALYSIS ###

#Read data and fill in NA
winenew <- read_csv("wine_new.csv", na = c("", "NA", "UNKNOWN"))
glimpse(winenew)
summary(winenew)

#Convert all flavor variables to 1 and 0
winenew$choc2 <- ifelse(winenew$choc == TRUE, 1, 0)
winenew$flower2 <- ifelse(winenew$flower == TRUE, 1, 0)
winenew$berry2 <- ifelse(winenew$berry == TRUE, 1, 0)
winenew$citrus2 <- ifelse(winenew$citrus == TRUE, 1, 0)
winenew$coffee2 <- ifelse(winenew$coffee == TRUE, 1, 0)
winenew$herb2 <- ifelse(winenew$herb == TRUE, 1, 0)
winenew$melon2 <- ifelse(winenew$melon == TRUE, 1, 0)
winenew$mint2 <- ifelse(winenew$mint == TRUE, 1, 0)
winenew$nut2 <- ifelse(winenew$nut == TRUE, 1, 0)
winenew$spice2 <- ifelse(winenew$spice == TRUE, 1, 0)
winenew$stonefruit2 <- ifelse(winenew$stonefruit == TRUE, 1, 0)
winenew$tobacco2 <- ifelse(winenew$tobacco == TRUE, 1, 0)


#Continuous - Continuous

#Corr test function from the psych package computes Pearson's correlation between numerical variables
#Also computes confidence intervals and p-values from testing each pairwise association
winenew %>% 
  select(price, points) %>% 
  psych::corr.test()

#Scatterplot matrix using ggplot
#visualize bivariate associations
winenew %>%
  select(price, points) %>%
  GGally::ggpairs()


#Categorical - Categorical 
#Use chi-square test used to test for associations between categorical variables

#contingency tables showing frequencies and proportions for the different categories
#Contingency tables are useful for summarizing the number of subjects in each category for all possible 
#pairwise combinations of categories for the 2 variables
winenew %>% with(table(winenew$berry2))
winenew %>% with(table(winenew$citrus2))


#CHISQ TEST OPTION 1
#frequency table and chisquare test
freqtable <- winenew %>%
  with(table(winenew$berry2, winenew$citrus2))

freqtable

chisq.test(freqtable)

#mosaicplot - visualize proprotions between variables computed in the contingency table
mosaicplot(berry2 ~ citrus2, data = winenew,
           color = c("light blue", "light grey"))

#CHISQ TEST OPTION 2
#CrossTable, run chisq test

winenew %>%
  with(gmodels::CrossTable(winenew$berry2, winenew$citrus2, chisq = TRUE,
                           prop.c = FALSE,
                           prop.t = FALSE,
                           prop.chisq = FALSE,
                           expected = TRUE))

#Correlations between all flavor variables
#OPTION 1: using cor function
#Spearman's method assesses how well relationship between 2 variables can be described using a monotonic function
#Spearman correlation between 2 variables is equal to the Pearson correlation between the rank values of the 2 variables
#Pearson's correlation assesses linear relationship, Spearman's correlation assesses monotonic relationships (whether linear or not)
flavor <- winenew[,25:36]
flavor.cor <- cor(flavor, method = c("spearman"))
flavor.cor

#Correlation matrix plot
#Correlogram of default correlation matrix plot is generated. 
#Positive correlations are displayed in a blue scale while negative correlations are displayed in a red scale
corrplot(flavor.cor)

#Creating a heatmap object using correlation coefficients as input to the heatmap
palette = colorRampPalette(c("purple", "white", "green")) (20)
heatmap(x = flavor.cor, col = palette, symm = TRUE)

#OPTION 2: Use rcorr from Hmisc to obtain correlation matrix (table of correlation coefficients)
#and another table of p-values
flavor.rcorr = rcorr(as.matrix(flavor))
flavor.rcorr

flavor.coeff <- flavor.rcorr$r
flavor.coeff
flavor.p <- flavor.rcorr$P
flavor.p

