# install any packages you may need to install
install.packages("languageR")
install.packages("tidyverse")
install.packages("magrittr")
install.packages("dplyr")
install.packages("gapminder")
install.packages("ggplot2")

# load the following packages: languageR, tidyverse, magrittr
library(tidyverse)
library(magrittr)
library(languageR)
library(dplyr)
library(gapminder)
library(ggplot2)

# load the dataset called danish, using data()
data(danish)
danish

# look up the help page on the dataset danish using help(), and *look up* the meanings of the different columns
help(danish)
str(danish)

# convert log(RT) to raw RTs
danish %<>% mutate( RT = exp(LogRT) )
print(danish$RT)

# create a new variable called 'frequency', which has the value 'high freq' when the log-frequency
# is above the median, and 'low frequency' when the log-frequency is below the median
danish %<>% mutate( frequency = ifelse( LogWordFreq > median(LogWordFreq), 'high freq', 'low freq') )

# convert frequency to a factor
danish %<>% mutate( frequency = factor(frequency, levels = c('high freq', 'low freq')) )

# compute the average RT, and the interquartile range (IQR) of RT by participant gender
# use the lecture notes to look up how to to compute the interquartile rang
danish %>%
  group_by(Sex) %>%
  dplyr::summarize(avg_RT_by_gender = mean(RT))

  danish %>%
    group_by(Sex) %>%
    dplyr::summarize(IQR_RT_by_gender = IQR(RT))

# plot the average RT by gender, with gender on the x-axis; use geom_point() 
  danish %>%
    group_by(Sex) %>%
    dplyr::summarize(avg_RT_by_gender = mean(RT))
  ggplot(avg_RT_by_gender, aes(Sex, avg_RT_by_gender)) + geom_point()
  
# create a boxplot of RTs by gender
  ggplot(data = danish, aes(x = Sex, y = RT)) +
    geom_boxplot(
      notch = T,
      fill = c("blue", "red"),
      outlier.color = NULL
    ) +
    theme_classic() +
    theme(legend.position = "none")
  
# plot a histogram of RTs by gender
  ggplot(danish, aes(RT)) + geom_histogram() + facet_wrap(danish$Sex, scales = "free")
  
# answer below in plain text: Is there an appreciable effect of gender?
# The mean of RT is 890. Average RT by gender is more in males with 902. Therefore gender causes a slight change towards males.
  

# compute the average RT, and the interquartile range of RT by frequency (i.e., the newly created column called 'frequency')
  danish %>%
    group_by(frequency) %>%
    dplyr::summarize(avg_RT_by_frequency = mean(RT))
  
  danish %>%
    group_by(frequency) %>%
    dplyr::summarize(IQR_RT_by_frequency = IQR(RT))

# plot the average RT by frequency, with frequency on the x-axis; use geom_point() 
   danish %>%
     group_by(frequency) %>%
     dplyr::summarize(avg_RT_by_frequency = mean(RT))
   ggplot(avg_RT_by_frequency, aes(frequency, avg_RT_by_frequency)) + geom_point()
   

# create a boxplot of RTs by frequency
   ggplot(data = danish, aes(x = frequency, y = RT)) +
     geom_boxplot(
       notch = T,
       fill = c("blue", "red"),
       outlier.color = NULL
     ) +
     theme_classic() +
     theme(legend.position = "none")

# plot a histogram of RTs by frequency
   ggplot(danish, aes(RT)) + geom_histogram() + facet_wrap(danish$frequency, scales = "free")
   
# answer below in plain text: Is there an appreciable effect of frequency?
# The mean of RT is 890. Average RT by frequency is more in males with 906. Therefore frequency causes a slight change towards males, however the effect of frequency is more than the effect of gender.
