getwd()
setwd("C:/Users/Leah/Desktop/RforLing")
library(tidyverse)
library(languageR)

### THE BASICS: MEAN AND VARIANCE
english %>% summarize(mean(RTlexdec))
english %>% summarize(sd(RTlexdec))
#other useful summarize() statistics: var, min, max, median, range, quantile
#can get those one at a time, as above
#or "summary()" gives several:
summary(english$RTlexdec)
sapply(english, mean) #where do the NA's appear?

###BASICS: DISTRIBUTIONS
#A "distribution" is the pattern of what results you get following certain probability rules
#For example, say your experiment is "flip a fair coin 100 times and count the number of heads." The first time you do that, you get 47 heads. The second time, 60 heads. The third time, 54 heads. The fourth time, 32 heads. And so on. 
#If you repeat this experiment many many times, most of the results are going to be near 50 heads. It has a binomial distribution -- this kind of experiment follows the binomial rules. But we usually approximate this with a normal distribution. That's the hill-shaped one. We expect most of the results to be near 50, some of the results to be a little further away from 50, and very few results to be far away from 50. 
rbinom(n=1, size=100, prob=0.5) #one experiment of 100 flips
rbinom(n=1000, size=100, prob=0.5) #one thousand experiments of 100 flips each
hist(rbinom(n=1000, size=100, prob=0.5))

#Say your second experiment is "roll a fair six-sided die 60 times and tally what numbers come up." You'd expect each number to come up around (but probably not exactly) 10 times. 
sample(x=1:6, size=60, replace=TRUE) #one experiment with 60 rolls -- indiv rolls
table(sample(x=1:6, size=60, replace=TRUE)) #one experiment with 60 rolls -- table
plot(table(sample(x=1:6, size=60, replace=TRUE)), type="h")
#Trying to make R save 1,000 die-rolling experiments of 60 throws each is surprisingly annoying
#We'll do not-quite-the-same-thing and just roll 60,000 times
plot(table(sample(x=1:6, size=60000, replace=TRUE)), type="h")

#So this is not at all like normal distribution -- it's six numbers that are all equally likely, so it's a flat line, not a bell shape. We can use a uniform distribution to model this (a flat-line model). But if we don't think about that and use tests that are for normal distributions, our statistics won't make any sense.

#Here's the really important (and tricky) part: it's not the situation, the rolling of dice or flipping of coins, that matters. It's the question you're asking ABOUT that situation. 
#With the die roll, I can change my question to "how often do I roll a 1"? And THAT is a binomial question again 
hist(rbinom(n=1000, size=60, prob=(1/6)))
#We get a 1 around 10 times in every 60 rolls. Sometimes we get more than 10, sometimes less, but it's around 10. 

#Probability people are always talking about flipping coins and rolling dice and throwing darts and stuff like that. The reason is that these are very very controlled situation. You might flip a coin and have it land in a crack in the table and stick up exactly on its edge. But this is so phenomenally unlikely that for all practical purposes heads and tails are 50/50 chances. We can use abstract, perfectly fair coins and dice to get an idea of what the  probability distributions look like under abstract, perfectly fair conditions.
#The point of stats is using this abstract information to understand messy, complicated real-world data, instead of imaginary perfect coins. Statistical questions involve figuring out which of these theoretical distributions is the right one for your question (so if it was a pure probability problem, what distribution would we expect) and then comparing your actual data to that distribution. 

### TEST 1: T-TEST

#A t-test compares data to "Student's t" distibution (not just for students, the guy who invented it was named Student). This is like a normal distribution but has fatter tails, which means it's more likely than in a normal distribution to sometimes get results far away from the mean.
hist(rnorm(100, mean=0, sd=1))
hist(rt(n=100, df=5))
#We use the Student's t-distribution when we think the data is probably normal, but we're sampling from something complicated so we can't be 100% sure. 
#The main way we use this is for the t-test. If we have two normally distributed variables and they have different means, the t-test tells us whether it's likely that the difference in their means is just random chance. 
#The t-test has three assumptions:
# 1. The data is normally distributed
# 2. The two groups have the same variances
# 3. The different results are independent. 

#Let's stay with coin flips for an example. Say we have a new coin, Coin X, and we don't know if it's fair or not. We do one hundred experiments where we flip it 100 times and we get this results for the number of heads: 
CoinX <- c(41, 37, 38, 37, 32, 25, 28, 38, 40, 28, 32, 41, 30, 37, 33, 38, 36, 33, 28, 33, 39, 35, 30, 38, 36, 41, 30, 33, 27, 31, 34, 43, 30, 37, 48, 32, 39, 38, 38, 36, 35, 41, 32, 42, 35, 38, 35, 34, 29, 38, 30, 36, 35, 45, 32, 36, 34, 35, 36, 44, 42, 33, 37, 34, 35, 30, 26, 32, 33, 37, 31, 48, 38, 36, 38, 42, 34, 34, 42, 31, 34, 35, 37, 34, 31, 32, 29, 39, 37, 36, 33, 36, 34, 32, 35, 36, 45, 42, 39, 34)
#What's the mean here?
mean(CoinX)
#Is Coin X likely to be a fair coin? Well, a fair coin would have a mean around 0.5, so in a hundred flips there would be about 50 heads. It's  possible for us to get the mean we got here with a fair coin. And that's our null hypothesis:  Coin X is *really* a fair coin (mean=0.5), and the results we got (mean=0.35) are from chance. But is that very likely? 
#We're gonna t-test to find out. But first, let's check our three assumptions. 
# 1. Is the data normally distributed?
qqnorm(CoinX)
#Looks ok. We're using discrete data and not enough of it to get a real "line," it's more a "stairstep," but that's not the end of the world. Wobbly at the edges, again that's alright. And we know our "fair coin" comparison will be normal because we're gonna generate that ourselves.
qqnorm(rbinom(n=100,size=100,prob=0.5))
# 2. Are the variances the same?
var.test(CoinX, rbinom(n=100,size=100,prob=0.5))
#The variance differences are NOT statistically significant, which is what we want.
# 3. Are the tests independent?
#Well, sure. We already know that these are coin flips. Getting "heads" on flip #4 doesn't affect whether we get "heads" on flip #5, or #12, or #47. No problem there.
#So t-test is go. Are these two means different from each other in a statistically significant way?
t.test(CoinX, rbinom(n=100,size=100,prob=0.5))
#The p-value is extremely small. So there's only a very very small chance that we would get in a hundred experiments a mean of 35 heads if the coin that was really fair. Coin X is very unlikely to be a fair coin. We can reject the null hypothesis that Coin X has the same mean as a fair coin. 

#Now let's try it with linguistics data. Going back to the "english" dataset, there are are two groups of subjects: young and old. Do their reaction times have different means?
mean(english$RTlexdec[english$AgeSubject=="young"])
mean(english$RTlexdec[english$AgeSubject=="old"])
#They're a little different. Is that difference more likely to mean something real, or are the two groups probably really the same and the difference is due to chance? 
#A t-test will help us, but first we need to make sure that the three basic assumptions holds. 
#1. Are the reaction times of the "young" group and the reaction times of the "old" group  approximately normally distributed?
hist(english$RTlexdec[english$AgeSubject=="young"])
hist(english$RTlexdec[english$AgeSubject=="old"])
#Hmm, they're a little bit skewed. That is, the mean isn't right in the middle, it's to the left. Let's look at a Q-Q plot to get a better idea of how far this is from a normal distribution. 
qqnorm(english$RTlexdec[english$AgeSubject=="young"])
qqnorm(english$RTlexdec[english$AgeSubject=="old"])
#A little curvy, but not terrible. The t-test can still handle this okay, as long as we're working with enough data. Are we?
table(english$AgeSubject)
#Oh, yeah, that's plenty. If we only had like 30 observations, the skew would be a big problem, but with 2,000+ observations for each group, we're okay. 
#That means we can go to assumption #2: that the variances in the two groups are roughly the same. This is important because if like the young group is almost all between 6.41 and 6.45, to get a mean of 6.43, but the old group is anywhere from 6.0 to 7.32 to get the mean of 6.66, then there's something more weird going on and the t-test isn't going to notice it. R mostly handles this itself, but let's take a look. 
var(english$RTlexdec[english$AgeSubject=="young"])
var(english$RTlexdec[english$AgeSubject=="old"])
#Looks pretty close. If the two groups were both very normal, we could test variances with the same "var.test()" function we used above. These aren't exactly normal, though, so let's use Levene's test. We need to get that from the "car" package. (Both variance tests are actually types of F-test, which we'll talk about in its own section later.)
install.packages("car")
library(car)
leveneTest(RTlexdec ~ AgeSubject, data=english)
#Well damn. The differences in variations are statistically significant. But like I said, R can handle this anyway. The default for a t-test in R is to assume that the variances are NOT equal. If they WERE equal, we could run a stricter t-test, but since they're not, we'll use the default.   
#And now assumption #3: that the tests are independent. 
#That's also a little tricky here. See, we don't know how many subjects there are. The dataset doesn't say. Maybe each word was given to one and only one person, so there were 4568 subjects. Maybe there were only two subjects, a young one and an old one, and they each did 2284 words.
#My guess is that it's somewhere in the middle, like 45 subjects doing about 100 words each. But we don't KNOW that. And that puts a HUGE caveat on our analysis. We can run the t-test, but without knowing how many subjects there are we can't be very sure about it. For example, if there are only 6 subjects, 3 young and 3 old, and one of the old people happens to be super slow, then the young/old difference might not actually be a young/old difference. It could be that everyone's reaction time is the same except old Barnabus, and he's skewing the "old" data.
#This is a VERY COMMON problem in linguistics -- scholars reporting data about individual WORDS or FEATURES without being clear about how many PEOPLE there are. You can have 5,000 examples of something, but if they're all from the same person, they're not independent and you can't say much about this word or feature generally. 
#We're gonna test anyway. But we would need to put a HUGE caveat on this information. In fact, if I was doing this for real, I'd try to contact the people who did the experiment and find out how many subjects there were. And if I couldn't find that, I'd just throw this away. 
t.test(english$RTlexdec[english$AgeSubject=="young"], english$RTlexdec[english$AgeSubject=="old"])
#WITH OUR BIG CAVEAT IN MIND, based on this we can reject the null hypothesis: probably the difference in mean reaction time between these two groups is not due to chance. Probably young and old people in the whole population have different means here. 
#Now, this is only STATISTICAL significance: it's probably not chance. That's not the same as SUBSTANTIVE significance. Substantive significance means noticing that the difference between these two means is only 0.22, and knowing enough about brains and language and response time to say whether that's enough of a difference for us to care about. 

#Let's practice. Compare the reaction times when a word is a verb to when it's a noun. But first, make sure that it's okay to use a t-test. Discuss all three assumptions that a t-test makes and whether they hold up for the noun/verb groups. 













### TEST 2: CHI-SQUARE
#For comparing categorical variables
#First, take a look at them:
#Two categorical variables
table1 <- table(english$CV, english$Voice)
prop.table(table1)
#Three categorical variables
table2 <- table(english$CV, english$Voice, english$WordCategory)
ftable(table2)
#If you have categorical groups (like "noun" and "verb") and the 
#

#each one: what it's for (what kind of questions it answers); what assumptions it makes and how to tell if those assumptions hold; how to code; how to interpret result; how to present graph or chart



### F-TEST ###
## Purpose
## Assumptions
## Code
## Interpreting Results
## Presenting Results

### CHI-SQUARE ###
## Purpose
## Assumptions
## Code
## Interpreting Results
## Presenting Results

### ANOVA ###
## Purpose
## Assumptions
## Code
## Interpreting Results
## Presenting Results


