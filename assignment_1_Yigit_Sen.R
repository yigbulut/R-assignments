
###Yigit Sen
### Please consider this assignment a type of translation task. Translate each of the 
### statements below from English to R. Each instruction should correspond to 1-3 lines 
### of code (usually one line).
### Please note that there may be multiple, equally valid solutions to each instruction.
###
### After you finish the assignment, please click on "Session"->"Restart R" in your R Studio
### and run the code again, to make sure it executes properly in *exactly* the order in which
### you have written it. (The most common error is that people don't load packages later than
### they should be loaded, or leave install.packages() calls in this R code. )
###
### Each statement is worth 1 point.
###

# load the packages languageR, tidyverse, and ggplot2 (install them if you don't have them already)
# (packages are installed using install.packages())
install.packages("languageR")
install.packages("tidyverse")
install.packages("ggplot2")

# create a vector named x by assigning it all numbers between 1 and 25, use seq()
x <- seq(1, 25, by = 1)
print(x)

# compute the average of this vector
xmean = mean(x)
print(xm)

# determine how many elements the vector has
xsize = length(x) 
print(xsize)

# convert this integer vector to a character vector
x <- as.character(x) 
print(as.character(x))

# load the 'warlpiri' data.frame from the package 'languageR'
install.packages("languageR")
library(languageR)
data("warlpiri")
warlpiri

# display the first 3 lines of the data.frame
head(warlpiri, 3)

# look up the meaning of the various columns in the data.frame
str(warlpiri)

  # find out how many rows are in the data.frame
nrow(warlpiri)

# find out which values can occur in the column which indicates the presence of ergative case marking
unique(warlpiri$CaseMarking)

# determine the overall frequency of ergative case marking in the data set
library(dplyr)
warlpiri %>%
  dplyr::summarize(perc_erg = mean(CaseMarking == "ergative"), N = n_ergative)

## In *separate commands*, determine the frequency of ergative case
## marking as a function of the variables listed
## below, and please save the results in the specified data frames.
## Inspect the data frames with View().
## ... (i) word order (save in data frame called 'proportions_by_WO')
View(proportions_by_WO <- warlpiri %>%
  group_by(WordOrder) %>% 
  dplyr::summarize(perc_erg = table(CaseMarking == "ergative")))

# ... (ii) overtness of object  (save in data frame called 'proportions_by_ovObject'
View(proportions_by_ovObject <- warlpiri %>%
  group_by(OvertnessOfObject) %>% 
  dplyr::summarize (perc_erg = table (CaseMarking == "ergative")))

# ... (iii) animacy of the object (save in data frame called 'proportions_by_animObj')
View(proportions_by_animObj <- warlpiri %>%
  group_by(AnimacyOfObject) %>% 
  dplyr::summarize(perc_erg = table(CaseMarking == "ergative")))

proportions_by_animObj %>%
  ggplot(aes(x = AnimacyOfObject, y = perc_ergative)) +
  geom_point() +
  xlab("Animacy of the object") +
  ylab("Percentage of ergative")

