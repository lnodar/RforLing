#Practice with line graphs and scatterplots
#The bar graph code from last week is at the bottom of this script!

getwd()
setwd("C:/Users/Leah/Desktop/RforLing")
library(tidyverse)

###Line graph of police shootings by year (data from WashPo)
#1. Import dataset
WaPo <- read_csv("fatal-police-shootings-data.txt")

#2. Organization: look at the data. How are dates given? 

#We want our chart to be by year. For that we'll use "lubridate." Install the package if you need to.
library(lubridate)
year <- year(ymd(WaPo$date))
WaPo <- mutate(WaPo, year)

#Use piping (%>%) to group the data by year, then get the count for each year. Save this as a dataset WaPo2. When you get the count, remember to name it something, so it won't have parentheses in the variable name. 
WaPo2 <- WaPo %>% group_by(year) %>% summarise(shootings=n())
WaPo2

#3. Make this dataset ggplot-readable. That means preparing ggplot for what the data is and what we want the x and y variables to be. Remember this WON'T make a graph; it's just getting ready. Name this WaPo2plot.
WaPo2plot <- ggplot(data=WaPo2, aes(x=year,y=shootings))

#4. Use the data in WaPo2plot to make a graph. Show ggplot that what we want is a line graph by using geom_line(). 
WaPo2plot+
  geom_line()

#5. Well, that's not too useful. I downloaded this data in May 2020, so the 2020 numbers are obviously way smaller, and this makes it hard to see the differences in 2015-19. Go back to WaPo2, make a WaPo3 by dropping 2020, and try again. 
WaPo3 <- WaPo2 %>% filter(year<2020)
WaPo3plot <- ggplot(data=WaPo3, aes(x=year,y=shootings))
WaPo3plot+
  geom_line()

#6. Customize title, labels, theme, colors, etc., then save as a png.




###Scatterplot with line of best fit
#Let's go back to lexdec for this one. Goal: two scatterplots, reaction times by frequency and reaction times by word length.
#1. Get the data. Don't really need to do any special organizing on this one. 
library(languageR)
?lexdec

#2. Make two ggplot-readable datasets. I called them ldplot and ldplot2. 

ldplot <- ggplot(data=lexdec, aes(x=RT, y=Frequency))
ldplot2 <- ggplot(data=lexdec, aes(x=RT, y=Length))

#3. Make these into two scatterplots. Try geom_point and geom_jitter. What's the difference?

ldplot+
  geom_jitter()

ldplot2+
  geom_jitter()

#4. Add a line of best fit to each one. There are several ways to do this, but the simplest for just looking at data like this is by adding the ADDITIONAL layer geom_smooth(method=lm). What are the tendencies for each graph? Is that what you'd expect?

ldplot+
  geom_jitter()+
  geom_smooth(method=lm)

ldplot2+
  geom_jitter()+
  geom_smooth(method=lm)

#5. Customize and save. 




###EXAMPLE FROM LAST TIME: BAR GRAPH

budgplot <- ggplot(data=budget,aes(x=`Department/Cost Center`,y=`Millions of Dollars`))

budgplot+
  geom_col(fill="#333366")+
  coord_flip()+
  theme_grey()+
  ggtitle("Ten Largest Expenditures in Mobile, AL 2020 City Budget", subtitle="Accounting for $162 million of the total $259 million budget")+
  geom_text((aes(label=`Millions of Dollars`)), hjust=1.25,color="white")+
  labs(caption="Data Source: City of Mobile, AL Annual Budget Fiscal Year 2020 \n https://www.cityofmobile.org/uploads/file_library/2020-proposed-budget-final-082019.pdf")

ggsave("MobileAlBudget2020.png")

