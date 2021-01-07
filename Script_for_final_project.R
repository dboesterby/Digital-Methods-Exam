# At first I will import the dataset from my computer 
##NB. You need to upload the dataset from the position it has on your computer##
###The dataset you need to upload is in my github repository###

library(readxl)
Life_Expetancy <- read_excel("C:/Users/Daniel/Desktop/7. semester/Teoriorienteret Historie/Digitale metoder/Life_Expetancy.xlsx")
View(Life_Expetancy)

# Installing and loading necessary R-packages for the project

install.packages("tidyverse")
library(tidyverse)

install.packages("ggplot2")
library(ggplot2)


# Running initial codes just to make sure that my dataset is imported correctly

head(Life_Expetancy, 20) 

summary(Life_Expetancy)  


# Locating observations where life expectancy is 0 and excluding them

which(Life_Expetancy[,4]==0) 

Life_Expetancy1 <- Life_Expetancy[-c(1073, 1154, 1875, 1940, 2037, 2102, 2135, 2296, 2345, 2746),]


# Checking that the new dataset is stripped for life expectancy values == 0

summary(Life_Expetancy1) 


# Grouping using aggregate. Grouping countries by status and their mean each year

aggregate(Life_Expetancy1[,4], list(Life_Expetancy1$Status), mean)

aggregate(Life_Expetancy1[,4], list(Life_Expetancy1$Year), mean)

Life_Expetancy1$Status_Year <- apply( Life_Expetancy1[ , c( 'Status' , 'Year' ) ] , 1 , paste , collapse = "_" )

aggregate(Life_Expetancy1[, 4], list(Life_Expetancy1$Status_Year), mean)


# Creating new data-environment with the average life expectancy each year by country status
average <- aggregate(Life_Expetancy1[, 4], list(Life_Expetancy1$Status_Year), mean)


# Making status in character in order to beautify the finalized ggplot

countrystatus <- c("Developed","Developed","Developed","Developed","Developed","Developed","Developed","Developed","Developed","Developed","Developed","Developed","Developed","Developed","Developed","Developed","Developing","Developing","Developing","Developing","Developing","Developing","Developing","Developing","Developing","Developing","Developing","Developing","Developing","Developing","Developing","Developing")

average$status <- countrystatus


#Making a year-variabel in data-frame 'average' to use when creating ggplot

Year <- c(2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012,2013,2014,2015)
average$year <- Year

# Duplicating the column variable "Life expectancy" because the name causes a problem in ggplot
average$lifeexp <- average$`Life expectancy`


# Making the plot/figure with ggplot - ggplot2-package necessary 
ggplot(average, aes(x=year, y=lifeexp, color=status)) +
  labs(title = "Average life expectancy from 2000 to 2015",
       subtitle = "Devided in developed and developing countries")+
  xlab("Year")+
  ylab("Life expectancy")+
  geom_point(size=2, alpha=0.8)+
  geom_line()
 
     


