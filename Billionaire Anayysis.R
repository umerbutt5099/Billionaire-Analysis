library(tidyverse)
library(naniar)
library(visdat)
library(ggplot2)
library(stringr)
library(dplyr)
library(formattable)
Billionaires <- read.csv("Billionaire.csv", header = TRUE, na.strings = c("NA","N/A",""))
head(Billionaires)
glimpse(Billionaires)
sum(duplicated(Billionaires))
Billionaires %>%
count(Name) %>%
filter (n > 1)
sum(is.na(Billionaires))
miss_var_summary(Billionaires)
Billionaires %>%
arrange(NetWorth) %>%
vis_miss()
Billionaires_cleaned <- na.omit(Billionaires) %>%
distinct(Name, .keep_all = TRUE)
sum(is.na(Billionaires_cleaned))
Billionaires_cleaned %>%
count(Name) %>%
filter (n > 1)
print(Billionaires$NetWorth)
head(Billionaires_cleaned)
regexp <- "[[:digit:]]+"
Billionaires_cleaned$NumNetworth<-str_extract(Billionaires_cleaned$NetWorth, regexp)
Billionaires_cleaned$NumNetworth<-as.numeric(Billionaires_cleaned$NumNetworth)
Billionaires_cleaned<-Billionaires_cleaned[order(Billionaires_cleaned$NumNetworth, decreasing = TRUE),]
topten<-head(Billionaires_cleaned,10)
topten
ggplot(topten,aes(x= reorder(Name, desc(NumNetworth))))+geom_bar(aes(fill= NetWorth))+labs(title = "Top 10 Billionaires in World")+theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))+labs(x="Names",y="Count")

# Top Five Industry s with most Billionaires

Industry<-as.data.frame(table(Billionaires_cleaned['Industry']))
colnames(Industry)<-c("Industry","Occurences")
Industry$Fraction <- Industry$Occurences / sum(Industry$Occurences)
Industry
Industry<-Industry[order(Industry$Occurences, decreasing = TRUE),]
Industry
TopFiveIndustry<-head(Industry,5)
TopFiveIndustry$Fraction <- TopFiveIndustry$Occurences / sum(TopFiveIndustry$Occurences)
TopFiveIndustry
TopFiveIndustry$max <- cumsum(TopFiveIndustry$Fraction)
TopFiveIndustry$min <- c(0, head(TopFiveIndustry$max, n=-1))
TopFiveIndustry
TopFiveIndustry$labelPosition <- (TopFiveIndustry$max + TopFiveIndustry$min) / 2
TopFiveIndustry$label <- paste0(TopFiveIndustry$Industry, "\n value: ", formattable(TopFiveIndustry$Fraction*100,digits = 2, format = "f"))
TopFiveIndustry
ggplot(TopFiveIndustry, aes(ymax=max, ymin=min, xmax=4, xmin=3, fill=Industry)) +
geom_rect() +
geom_label( x=3.5, aes(y=labelPosition, label=label), size=6) +
scale_fill_brewer(palette=8) +
coord_polar(theta="y") +
xlim(c(2, 4)) +
theme_void() +
theme(legend.position = "none")

# Top Five Countries with most Billionaires

Country<-as.data.frame(table(Billionaires_cleaned['Country']))
colnames(Country)<-c("Country","Occurences")
Country$Fraction <- Country$Occurences / sum(Country$Occurences)
Country
Country<-Country[order(Country$Occurences, decreasing = TRUE),]
Country
TopFiveCountry<-head(Country,5)
TopFiveCountry$Fraction <- TopFiveCountry$Occurences / sum(TopFiveCountry$Occurences)
TopFiveCountry
TopFiveCountry$max <- cumsum(TopFiveCountry$Fraction)
TopFiveCountry$min <- c(0, head(TopFiveCountry$max, n=-1))
TopFiveCountry
TopFiveCountry$labelPosition <- (TopFiveCountry$max + TopFiveCountry$min) / 2
TopFiveCountry$label <- paste0(TopFiveCountry$Country, "\n value: ", formattable(TopFiveCountry$Fraction*100,digits = 2, format = "f"))
TopFiveCountry
ggplot(TopFiveCountry, aes(ymax=max, ymin=min, xmax=4, xmin=3, fill=Country)) +
  geom_rect() +
  geom_label( x=3.5, aes(y=labelPosition, label=label), size=6) +
  scale_fill_brewer(palette=8) +
  coord_polar(theta="y",start = 90) +
  xlim(c(2, 4)) +
  theme_void() +
  theme(legend.position = "none")

#Top Five Domains with most Billionaires

Domains<-as.data.frame(table(Billionaires_cleaned['Source']))
colnames(Domains)<-c("Source","Occurences")
Domains$Fraction <- Domains$Occurences / sum(Domains$Occurences)
Domains
Domains<-Domains[order(Domains$Occurences, decreasing = TRUE),]
Domains
TopFiveDomains<-head(Domains,5)
TopFiveDomains$Fraction <- TopFiveDomains$Occurences / sum(TopFiveDomains$Occurences)
TopFiveDomains
TopFiveDomains$max <- cumsum(TopFiveDomains$Fraction)
TopFiveDomains$min <- c(0, head(TopFiveDomains$max, n=-1))
TopFiveDomains
TopFiveDomains$labelPosition <- (TopFiveDomains$max + TopFiveDomains$min) / 2
TopFiveDomains$label <- paste0(TopFiveDomains$Source, "\n value: ", formattable(TopFiveDomains$Fraction*100,digits = 2, format = "f"))
TopFiveDomains
ggplot(TopFiveDomains, aes(ymax=max, ymin=min, xmax=4, xmin=3, fill=Source)) +
  geom_rect() +
  geom_label( x=3.5, aes(y=labelPosition, label=label), size=6) +
  scale_fill_brewer(palette=8) +
  coord_polar(theta="y",start = 90) +
  xlim(c(2, 4)) +
  theme_void() +
  theme(legend.position = "none")
