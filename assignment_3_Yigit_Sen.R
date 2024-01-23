###############################################################################################
### Please consider this assignment a type of translation task. Translate each of the 
### statements below from English to R. Each instruction should correspond to 1-3 lines 
### of code (usually one line).
### Please note that there may be multiple, equally valid solutions to each instruction.
###
### *******************************************************************************************
### After you finish the assignment, please click on "Session"->"Restart R" in your R Studio  *
### and run the code again, to make sure it executes properly in *exactly* the order in which *
### you have written it. (The most common error is that people don't load packages later than *
### they should be loaded, or leave install.packages() calls in this R code. )                *
### *******************************************************************************************
###
### Each instruction is worth 1 point.
###

###
### The question I want you to answer in this assignment is whether languages with genitive-noun word order (rather than noun-genitive WO) 
### tend to have postpositions (rather than prepositions)
###

# 1. load the packages dplyr, magrittr, ggplot2, janitor, and scales (install packages that are missing, but don't include the installation code into this R script)
install.packages("dplyr")
install.packages("magrittr")
install.packages("ggplot2")
install.packages("janitor")
install.packages("scales")
library(dplyr)
library(magrittr)
library(ggplot2)
library(janitor)
library(scales)

# Read in the WALS data set. In case you are not familiar with it, please it up under http://wals.info
wals <- readr::read_csv("http://plogacev.github.io/lecture_notes/data/wals.csv")

# 2. print the first few rows of the wals data frame
head(wals)

# 3. create a data frame called wals_mini, which contains the first 10 columns of wals (but all of its rows). Use select().
wals_mini <- 
  wals %>%
  select(c(1:10))

# 4. call View(wals_mini), and given the information in it, as well as on the WALS website (http://wals.info), determine, 
# *what is the unit of observation in this data set?* (i.e., what does one row correspond to). Write down your answer as a comment (that is, preceded by a #-mark). Make it as short as possible. 
View(wals_mini)
# This is a database that contains the names of a big number of the world's languages with information on their location, language family and the countries in which they are spoken, so the unit of observation is a language.

# 5. In the full wals data frame, print the names of the 11-th, 12-th, and 13-th column, using colnames() [in a single R command, please!]
print(colnames(wals[11:13]))

# 6. Use the function 'make_clean_names' from the package 'janitor' to bring the column names into a more r-friendly format (e.g., underscores instead of spaces in column names)
#    Use colnames() and the assignment pipe in doing so.
#    (To find out how make_clean_names() works, look at the help page for janitor::make_clean_names - and scroll down to the examples of use. Try the examples by copy-and-pasting them into R.)
colnames(wals) <- colnames(wals) %>% make_clean_names()

# print the column names using colnames() again
# (please take note of how the column names have changed) 
print(colnames(wals))

# 7. Using the list of WALS features (https://wals.info/feature), find the IDs for the features 
#    (i) 'Order of Adposition and Noun Phrase', 
#   (ii) 'Order of Genitive and Noun',
#    and use head() to print the first few values in the columns corresponding to these features in the wals data frame.
#    (This corresponds to two columns.)
head(wals$"x85a_order_of_adposition_and_noun_phrase")
head(wals$"x86a_order_of_genitive_and_noun")

# 8. In the wals data frame, rename the features from 7. to 'order_adposition' and 'order_genitive'.
names(wals)[names(wals) == 'x85a_order_of_adposition_and_noun_phrase'] <- 'order_adposition'
names(wals)[names(wals) == 'x86a_order_of_genitive_and_noun'] <- 'order_genitive'
  
# 9. Create a data frame called wals_relevant with only the columns 'name' (language name), 'order_adposition', and 'order_genitive'.
wals_relevant <- data.frame(wals %>% dplyr::select('name', 'order_adposition', 'order_genitive'))
print(wals_relevant)
  
# 10. Convert the columns 'order_adposition' and 'order_genitive' into factors using as.factor(), and the assignment pipe
wals_relevant$order_adposition %>% as.factor()
wals_relevant$order_genitive %>% as.factor()

# 11. Use ggplot and geom_histogram(stat="count") to plot the frequencies of the different values occurring in the 'order_adposition' column.
#    Save the plot in 'p1'.
p1 <- ggplot(wals_relevant, aes(x = order_adposition)) + geom_histogram(stat="count")
p1

# If everything went according to plan, this code should rotate the x axis labels in p1 by 45 degrees to make it more readable, and then display the plot. 
p1 <- p1 + theme(axis.text.x = element_text(angle = 45, hjust=1))
p1

# 12. Use ggplot and geom_histogram(stat="count") to plot the frequencies of the different values occurring in the 'order_genitive' column.
#    Save the plot in 'p2'.
p2 <- ggplot(wals_relevant, aes(x = order_genitive)) + geom_histogram(stat="count")
p2

# If everything went according to plan, this code should rotate the x axis labels in p2 by 45 degrees to make it more readable, and then display the plot. 
p2 <- p2 + theme(axis.text.x = element_text(angle = 45, hjust=1))
p2

# 13. As you see in the above plots, both columns contain plenty of NAs (meaning the value for that language is unknown; NAs mark 'missing' or 'unknown' values.), 
#     as well as languages that can't be clearly located in our simplified  noun-genitive/genitive-noun and preposition/postposition dichotomies. 
#     To simplify our problem for present purposes, create a new data frame from wals_relevant, called *wals_relevant_clean*, 
#     by removing all rows which have one of the following characteristics:
#     (i) value in 'order_adposition' is NA
#     (ii) value in 'order_genitive' is NA
#     (iii) anything other than '1 Postpositions' or '2 Prepositions' in 'order_adposition'
#     (iv) anything other than '1 Genitive-Noun' or '2 Noun-Genitive' in 'order_genitive'
#    Please do this task carefully and in 4 steps. At first, your data frame should have 2679 rows, after the first step 1183 rows, 
#    after the second step 1006 rows, after the third step 917 rows, and after the fourth step 860 rows.
#    You will need to use filter() or subset(), as well as is.na(), as well as the R operators '!' and '%in%' (and possibly look them up in the Base R cheat sheet).
#    Please post questions in the forum if anything is unclear.
wals_relevant_clean <- wals_relevant
wals_relevant_clean <- wals_relevant_clean[!is.na(wals_relevant_clean$order_adposition),]
wals_relevant_clean <- subset(wals_relevant_clean, order_genitive!="NA")
wals_relevant_clean <- subset(wals_relevant_clean, order_adposition!="3 Inpositions" & order_adposition!="4 No dominant order" & order_adposition!="5 No adpositions")
wals_relevant_clean <- subset(wals_relevant_clean, !(order_genitive %in% c("3 No dominant order")))

# 14. Use wals_relevant_clean to compute the number of observations for each combination of order_adposition and order_genitive.
#     Use group_by() and summarize(). Store the result in the data frame 'order_counts'. Call the count column N.
order_counts <- (wals_relevant_clean %>%
  group_by(order_genitive, order_adposition) %>%
  summarize(N = n()))

order_counts 

# 15. Visualize order_counts with a bar diagram using geom_bar(stat="identity"), putting order_genitive on the x-axis, N on the y-axis, and using order_adposition for the fill argument.
ggplot(order_counts, aes(x = order_genitive, y = N)) + geom_bar(stat = "identity", aes(fill = order_adposition))

# 16. Visualize order_counts again, but this time with geom_bar(stat="identity", position = "dodge")
ggplot(order_counts, aes(x = order_genitive, y = N)) + geom_bar(stat = "identity", position = "dodge", aes(fill = order_adposition))

# 17. Please use group_by()/mutate() to compute the proportion of languages with prepositions and postpositions for genitive-noun languages and for noun-genitive languages.
#     Store the proportions in a column called 'prop'.
#     Please be careful, print the output data frame, and double-check that the proportions sum up to 1 for genitive-noun languages, as well as for noun-genitive languages.
order_counts <- (wals_relevant_clean %>%
                   group_by(order_genitive, order_adposition) %>%
                   summarise(n = n()) %>%
                   mutate(prop = n / sum(n)))
order_counts 

# 18. Visualize the proportions you just computed with a bar diagram using geom_bar(stat="identity", position = "dodge"), putting order_genitive on the x-axis, prop on the y-axis, 
#     and using order_adposition for the fill argument. Save the plot to p3.
p3 <- ggplot(order_counts, aes(x = order_genitive, y = prop)) + geom_bar(stat = "identity", position = "dodge", aes(fill = order_adposition))
p3

# If everything went according to plan, this should change the y axis on your plot p3 to percentages 
p3 + scale_y_continuous(labels=scales::percent)
