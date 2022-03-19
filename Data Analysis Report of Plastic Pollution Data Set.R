---
  title: "Data Analysis Report of Plastic Pollution Data Set"
author: "Amrinder Sehmbi"
date: "9/04/2021"
output: pdf_document
---
  knitr::opts_chunk$set(echo = TRUE)
plastics <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-01-26/plastics.csv')
library(tidyverse)
library(ggplot2)
library(mosaic)
library(knitr)

attach(plastics)
# Table of proportion for years
year <- table(plastics$year)
prop.table(year)
# Barplot of proportion of years
barplot(prop.table(year), xlab = "year", ylab = "proportion")

attach(plastics)
sub <- subset(plastics, country == c("Argentina", "India", "China", "Brazil", "Mexico") & grand_total > 10 & grand_total < 500, select = c(grand_total, country))
# Table of proportion of the countries
countries <- table(sub$country)
prop.table(countries)
# Barplot of proportion of the countries
barplot(prop.table(countries), xlab = "country", ylab = "proportion")

attach(plastics)
summary(grand_total[grand_total > 500])
boxplot(grand_total[grand_total > 500], horizontal = TRUE)

attach(plastics)
summary(num_events)
hist(num_events)

attach(plastics)
summary(volunteers)
hist(volunteers)

cat('\\pagebreak')

total_2019 <- subset(plastics, select = c(grand_total, year))
total_2019 <- subset(total_2019, grand_total > 500 & year <= 2019)
attach(total_2019)
grand_total_2019 <- grand_total
total_2020 <- subset(plastics, select = c(grand_total, year))
total_2020 <- subset(total_2020, grand_total > 500 & year >= 2020)
attach(total_2020)
grand_total_2020 <- grand_total
#Summary of 2019 total plastic count.
favstats(grand_total_2019)
#Summary of 2020 total plastic count.
favstats(grand_total_2020)
par(mfrow = c(1,2))
#Histogram of 2019 total plastic count.
boxplot(grand_total_2019, main = "Plastic Count 2019")
#Histogram of 2020 total plastic count.
boxplot(grand_total_2020, main = "Plastic Count 2020")

sub <- subset(plastics, select = c(grand_total, country))
sub <- subset(sub,country == c("Argentina", "India", "China", "Brazil", "Mexico") & grand_total > 10 & grand_total < 500)

sub1 <- subset(sub, country == "Argentina")
attach(sub1)
# Summary Statistics for Argentina Total Plastic Count 
summary(grand_total)
sub2 <- subset(sub, country == "India")
attach(sub2)
# Summary Statistics for India Total Plastic Count 
summary(grand_total)
sub3 <- subset(sub, country == "China")
attach(sub3)
# Summary Statistics for China Total Plastic Count 
summary(grand_total)
sub4 <- subset(sub, country == "Brazil")
attach(sub4)
# Summary Statistics for Brazil Total Plastic Count 
summary(grand_total)
sub5 <- subset(sub, country == "Mexico")
attach(sub5)
# Summary Statistics for Mexico Total Plastic Count 
summary(grand_total)

# Box Plot of Total Plastic Count for Contries Above.
ggplot(sub, aes(x = country, y = grand_total)) + geom_boxplot()

tuesd <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-01-26/plastics.csv')
attach(tuesd)
summary(hdpe)
plot(hdpe,grand_total)

tuesd <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-01-26/plastics.csv')
attach(tuesd)
summary(pvc)
plot(pvc,grand_total)

tuesd <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-01-26/plastics.csv')
attach(tuesd)
summary(ps)
plot(ps,grand_total)

tuesd <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-01-26/plastics.csv')
attach(tuesd)
summary(grand_total)

cat('\\pagebreak')

plastics <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-01-26/plastics.csv')
attach (plastics)
grand_totals = subset(plastics, parent_company == "Grand Total")
summary(grand_totals$grand_total)
par(mfrow=c(1,2))
boxplot(grand_totals$grand_total, main="Grand Totals")
boxplot(grand_totals$grand_total, ylim=c(1,10000), main="Grand Totals Without Outliers")
par(mfrow=c(1,2))
hist(grand_totals$grand_total, main="Grand Total")
hist(grand_totals$grand_total, main="Grand Total Without Outliers")
grand_totals = subset(plastics, parent_company == "Grand Total")
x = mean(grand_totals$grand_total)
n = length(grand_totals$grand_total)
p = x/n
p
SE_p=sqrt(-1*(p*(1-p))/n)
SE_p
conf.level=0.95
alpha=1-conf.level
alpha
z.score=qnorm(alpha/2,mean=0,sd=1,lower.tail = FALSE)
z.score
Margin.Error=z.score*SE_p
Margin.Error
lower.bound=p-Margin.Error
lower.bound
upper.bound=p+Margin.Error
upper.bound
Ninetyfive.confidence.interval=data.frame(lower.bound,upper.bound)
Ninetyfive.confidence.interval

tuesd <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-01-26/plastics.csv')
attach(tuesd)
x=15.29
n=13307
p=x/n
p
SE_p=sqrt((p*(1-p))/n)
SE_p
conf.level=0.95
alpha=1-conf.level
alpha
z.score=qnorm(alpha/2,mean=0,sd=1,lower.tail = FALSE)
z.score
Margin.Error=z.score*SE_p
Margin.Error
lower.bound=p-Margin.Error
lower.bound
upper.bound=p+Margin.Error
upper.bound
Ninetyfive.confidence.interval=data.frame(lower.bound,upper.bound)
Ninetyfive.confidence.interval

favstats(ps)
hist(ps)
plot(ps)

favstats(hdpe)
hist(hdpe)
plot(hdpe)
favstats(ps ~ hdpe)

#alpha = 0.05
#alpha/2 = 0.025
#1 - alpha/2 = 1 - 0.025 = 0.975

n1 = 11408
n2 = 11734

lower.bound = qf(0.025, df1 = n1 -1, df2 = n2 - 1)
higher.bound = qf(0.975, df1 = n1 -1, df2 = n2 - 1)
Interval.Estimate = data.frame(lower.bound, upper.bound)
Interval.Estimate

attach(plastics)
favstats(num_events)
plot(num_events)
hist(num_events)
boxplot(num_events)

q1 = 4.00
q3 = 32.00
iqr = q3 - q1
x1 = num_events[q1 - (1.5)*(iqr) < num_events]
num.events = x1[x1 < q3 + (1.5)*(iqr) ]
favstats(num.events)
hist(num.events)
boxplot(num.events)

s = 16.38008	
n = 11658
x.bar = 17.23572
df = n - 1
t.score = qt(0.975, df) 
t.score
SE = s/(sqrt(n))
SE
ME = t.score*SE
ME
lower.bound = x.bar - ME
lower.bound
upper.bound = x.bar + ME
upper.bound
Interval.Estimate = data.frame(lower.bound, upper.bound)
Interval.Estimate



total_2019 <- subset(plastics, select = c(grand_total, year))
total_2019 <- subset(total_2019, grand_total > 500 & year <= 2019)
attach(total_2019)
grand_total_2019 <- grand_total
total_2020 <- subset(plastics, select = c(grand_total, year))
total_2020 <- subset(total_2020, grand_total > 500 & year >= 2020)
attach(total_2020)
grand_total_2020 <- grand_total

par(mfrow = c(1,2))
#Histogram of 2019 total plastic count.
hist(grand_total_2019, main = "Plastic Count 2019")
#Histogram of 2020 total plastic count.
hist(grand_total_2020, main = "Plastic Count 2020")

t.test(grand_total_2019, grand_total_2020, var.equal = FALSE)

knitr::opts_chunk$set(echo = TRUE)
plastics <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-01-26/plastics.csv')
library(tidyverse)
library(ggplot2)
library(mosaic)
library(knitr)

total_2019 <- subset(plastics, select = c(grand_total, year))
total_2019 <- subset(total_2019, grand_total > 500 & year <= 2019)
attach(total_2019)
grand_total_2019 <- grand_total
total_2020 <- subset(plastics, select = c(grand_total, year))
total_2020 <- subset(total_2020, grand_total > 500 & year >= 2020)
attach(total_2020)
grand_total_2020 <- grand_total

#Summary statistics for total plastic count in 2019
favstats(grand_total_2019)
#Summary statistics for total plastic count in 2020
favstats(grand_total_2020)

par(mfrow = c(1,2))
#Histogram of 2019 total plastic count.
hist(grand_total_2019, main = "Plastic Count 2019")
#Histogram of 2020 total plastic count.
hist(grand_total_2020, main = "Plastic Count 2020")


t.test(grand_total_2019, grand_total_2020, var.equal = FALSE)
