#As always, start by setting up the working directory 
getwd()
setwd("C:Users/John/Downloads")

#Next, import any packages you need. 
#Since we'll be messing with a new dataset, we'll want "tidyverse." (You pretty much always want tidyverse.)
#We'll get the dataset from another package, languageR
#Remember that if you don't already have these you'll need to "install packages" first 
library(tidyverse)
library(languageR)

#The languageR package includes many datasets
#Take a look:
data(package="languageR")

#We'll use the "english" dataset for now. 
#Start by just looking at the first few rows, and at the structure
head(english)
tail(english)
str(english)

#Good grief that's a lot of columns
#What do they all mean?
#Let's look at the help file
?english

#So this dataset has a bunch of English words, information about those words, and how long the reaction time was for subjects to decide if it was a word or not. Many of the words are really obscure. 
#Let's practice by asking two questions about this dataset
#First, is reaction time correlated with subjective ratings of familiarity for a word?
#And second, do young and old speakers have a statistically significant difference in average reaction time?

#Both of these involve taking just the columns we need from all of these options 
#For the first question, we want "Familiarity" and "RTlexdec" 
#But wait!
#Every word is in here twice -- once with the "old" speakers and once with the "young" speakers
#Let's put them back together before we go on
#So we'll need the two columns of interest, plus "Word" and "AgeSubject"
#You pull particular columns from a dataframe using the "select()" function
#And we're gonna use piping, which comes from tidyverse
#That's stuff using this symbol: %>%
#With that, we can put the data first, then do something to it
english %>% select(RTlexdec,Familiarity,Word,AgeSubject)

#That's the same as this:
select(english, c(RTlexdec,Familiarity,Word,AgeSubject))
#But the first one is easier to read, especially if we keep doing more stuff to that same data

#Let's save the smaller dataset as a new variable
#You could save it as "english" again, but then you would *lose* anything you didn't keep
#So it's better to save it as a new variable, so "english" still has all the original information and we can go back and pull other stuff out later
q1data <- english %>% select(RTlexdec,Familiarity,Word,AgeSubject)

#Ok. Let's look at the structure of this smaller dataframe
str(q1data)

#Hmm. "Word" is saved as a factor with 2197 levels (that means there are 2197 different words). But there are 4568 total rows. 
4568/2197
4568/2
#So some words are in here three or four times??
#Let's check on that. First we'll group the data by the "Word" column, then we'll get the count of how many times each word appears
#This pattern is a fundamental one for data org and exploration: 
# data %>% group_by(something) %>% summarize(number_you_want)
#There are lots of options in summarize() -- count, mean, range, etc.
?summarize()
#For now we want the count, which we get with n()
q1data %>% group_by(Word) %>% summarize(count=n())
#What we specifically want to know, though, is whether any of these counts are above 2
#So let's add one more step: filter()
q1data %>% group_by(Word) %>% summarize(count=n()) %>% filter(count>2)
#Looks like there are some that show up 4 times. That's weird. 
#Are they just duplicates?
q1data[q1data$Word=="arm",]
q1data[q1data$Word=="bitch",]
q1data[q1data$Word=="bust",]
#Looks like yes. So I can just include all 4 in the averages, and it shouldn't change anything (because (2+4)/2 = (2+4+2+4)/4). I'd be more careful about it if this was a final analysis, but for now, I'm just taking a first look at the data and I  want to move forward.

#To get the reaction time averages, I group by word again, and this time take a mean() in the summarize() function
q1data %>% group_by(Word) %>% summarize(avgRT=mean(RTlexdec))
#Once again, this just shows these summary numbers in the console 
#To continue messing with this, I'll need to save it as a variable too
#This time I'm removing data again by averaging, so I'll make another new dataframe variable. Don't forget to keep "Familiarity" in there too!
#Familiarity should be the same for each word, no matter whether it's under "old" or "young," so I'll just take the first time it comes up
q1d2 <- q1data %>% group_by(Word) %>% summarize(avgRT=mean(RTlexdec), familiarity=mean(Familiarity))
head(q1d2)

#Ok great!
#So the question: are these two correlated?
cor(q1d2$avgRT, y=q1d2$familiarity)
#Looks like yes, and in the direction we'd expect (higher familiarity correlates to lower reaction time)
#So this might be something worth investigating with linear regression later
#If we did that, we'd first want to make sure that both avgRT and familiarity are normally distributed
#(I'd bet money avgRT is, but familiarity? Could be exponential)
#And of course we'd want to go back and check that averaging in those surprise words with 4 answers 
#Anyway for now we're just organizing data for a stats first pass, so let's move on to question 2

#Do young and old speakers have a statistically significant difference in average reaction time?
#Now we DO want separate info for young and old speakers, so we can't use the q1data variable we made before
#We need to go back to the original and get the data we need for this new question
#What were the original columns again?
str(english)
#Ok, so we just want "RTlexdec" and "AgeSubject"
q2data <- english %>% select(RTlexdec, AgeSubject)
head(q2data)
tail(q2data)
q2data %>% group_by(AgeSubject) %>% summarize(mean(RTlexdec))

#Aaaand a t-test:
t.test(RTlexdec ~ AgeSubject, data=q2data)
q2data %>% t.test(RTlexdec ~ AgeSubject)
#Also a yes to question 2, then. 

#Ok, practice time! Take 2 minutes to think of another fairly simple question we could ask about this data. Remember, you can look at the information on what all the variables are in help:
?english

#Is there a difference between noun and verb in terms of reaction time?
q3data <- english %>% select(RTlexdec, Word, WordCategory)
q3d2 <- q3data %>% group_by(Word) %>% summarize(avgRT=mean(RTlexdec)) 
head(q3d2)

q3d3 <- left_join(q3d2, q3data, by="Word")
t.test(avgRT ~ WordCategory, data=q3d3)
