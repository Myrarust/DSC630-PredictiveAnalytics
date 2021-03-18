#Assignment 1.2: R Refresher 

#Part 1: Import, Plot, Summarize, and Save Data
# Set working directory
setwd("C:\\Users\\myraw\\RStudio\\DSC630")

# Read in the CSV file 
df <- read.table("CombinedPay.csv", header = TRUE, sep=",")
df

# Create a variable that is the average weekly pay for each year 
library(dplyr)
df <- df %>% mutate(MeanWeeklyPay=(Qtr1+Qtr2+Qtr3+Qtr4)/4)
df

# Save data locally as csv file
write.csv(df,"C:\\Users\\myraw\\RStudio\\DSC630\\CombinedPayNew.csv", row.names = TRUE)

# Generate Summary statistics for two variables
summary(df$Qtr1)
summary(df$Qtr4)

# Create subsets based on gender variable
male <- df[df$Gender == "Male", ]  
female <- df[df$Gender == "Female", ]  

#Investigate a couple variables using histograms
hist(female$MeanWeeklyPay, 
     main="Histogram of Average Weekly Pay (Female)",
     xlab="Weekly Pay in Dollars",
     col="darkred")

hist(male$MeanWeeklyPay, 
     main="Histogram of Average Weekly Pay (Female)",
     xlab="Weekly Pay in Dollars",
     col="steelblue") 

# Calculate the correlation between men and womens Average Weekly Pay
res <- cor.test(male$MeanWeeklyPay, female$MeanWeeklyPay)
res

# Change Gender variable to numeric to calculate correlation by gender
df1 <- df
df1$Gender <- as.numeric(df1$Gender)
df1

# Calculate the correlation between Gender and Average Weekly Pay
res2 <- cor.test(df1$MeanWeeklyPay, df1$Gender)
res2


# Part 2: Explore Some Bivariate Relations
library(ggplot2)

# Bivariate Histogram
ggplot(NULL, aes(x=MeanWeeklyPay, fill=Gender), legendPosition="top",
       alpha=0.5 ) + 
    geom_histogram(data = male, color="steelblue") +
    geom_histogram(data = female, color = "darkred")

# Bivariate scatterplot
ggplot(df, aes(x=ï..Year, y=MeanWeeklyPay)) +
    geom_point(aes(color = Gender, shape = Gender), size = 2, alpha = 0.6) 

# Bivariate boxplot
boxplot(df$MeanWeeklyPay ~ df$Gender)


# Part 3: Organize a Data Report

# Display the structure of the data
str(df)

# Look at summary statistics for all the data split by Gender
by(df, df$Gender, summary)

# Report data results
# I chose a data set that contained mean weekly pay per quarter for each year 
# from 2010 to 2020 and also Gender.

# From the histograms, we can see that the range for women is from $650-$900
# with $700-$750 being the mode. The range for men being $800-$1100 and the 
# mode being $850-$900.

# The relationship between men and women's mean weekly pay has r=0.9971124 and a 
# p-value=2.178e-11 indicating they are significantly correlated.
# This can be seen in the bivariate scatterplot which shows that both pays increase
# at nearly the same rate over time.

# The relationship between mean weekly pay to gender r=0.7634741 and 
# p-value=3.568e-05 also indicating a significant relationship.

# These results are nothing unexpected, we know that men on average get paid more 
# than women, but probably the most interesting visualization was the boxplot of 
# the two mean weekly pays. The ceiling for women lies right around the mean for 
# men and you can visually see how different the average pay is.