# Descriptive statistics
library(stringr)
library(xml2)
library(gesis)
library(devtools)
library(labelled)
library(Hmisc)
library("pols503")
library("rio")
library("ggplot2")
library("plyr")
library("dplyr")
library("broom")
library("devtools")
library("haven")
library("readr")
library("sjmisc")
library("car")
library("grep")
library(tidyr)
library(scales)

EB <- read_dta("EB.dta")


describe(EB)

varnames <- c('unemplratecorrect','inflratecorrect', 'growthcorrect', 'econ2correct'
)
for (i in varnames) {
  EB[[i]] <- ifelse(EB[[i]] %in% c(9),
                     NA, EB[[i]])
}

varnames <- c('finaexp', 'econexp', 'inflratecompared2013'
              )
for (i in varnames) {
  EB[[i]] <- ifelse(EB[[i]] %in% c(4, 8, 9),
                     NA, EB[[i]])
}

varnames <- c('situationatecon', 'situationeuecon', 'finasituation',
              'feeleuimmigration', 'feelnoneuimmigration',
              'betteroutsideeu', 'crisisreducepublicdebt','crisispublicdebtnopriority',
              'eusufficientpowers','crisisprivatesectorjobs', 'crisispublicinvestment',
              'statspoldecisions', 'satisdmo', 'satisdeu', 'citizen','stateintervenestoomuch',
              'toomuchtolerance', 'immigrantscontribute'
              )
for (i in varnames) {
  EB[[i]] <- ifelse(EB[[i]] %in% c(5, 6, 7, 8, 9),
                     NA, EB[[i]])
}

varnames <- c('trustnp', 'trustgov',
              'trusteu','heardep',
              'heardec', 'heardecb', 'trustep', 'trustec', 'trustecb',
              'euknownomemb', 'euknowswimemb', 'euknowepelect', 'statstrust')
for (i in varnames) {
  EB[[i]] <- ifelse(EB[[i]] %in% c(3, 8, 9),
                     NA, EB[[i]])
}

varnames <- c('euimage', 'soclass', 'educrec5'
              )
for (i in varnames) {
  EB[[i]] <- ifelse(EB[[i]] %in% c(6, 7, 8, 9),
                     NA, EB[[i]])
}


varnames <- c('unemplrate2014rec')
for (i in varnames) {
  EB[[i]] <- ifelse(EB[[i]] %in% c(8, 99),
                     NA, EB[[i]])
}

varnames <- c('inflrate2014rec', 'growth2014rec')
for (i in varnames) {
  EB[[i]] <- ifelse(EB[[i]] %in% c(9, 99),
                     NA, EB[[i]])
}

varnames <- c('unemplrate2014', 'inflrate2014', 'growth2014')
for (i in varnames) {
  EB[[i]] <- ifelse(EB[[i]] %in% c(99999, 999999),
                     NA, EB[[i]])
}

varnames <- c('married'
)
for (i in varnames) {
  EB[[i]] <- ifelse(EB[[i]] %in% c(15, 97),
                     NA, EB[[i]])
}

varnames <- c('educ'
)
for (i in varnames) {
  EB[[i]] <- ifelse(EB[[i]] %in% c(0, 97, 98, 99),
                     NA, EB[[i]])
}

varnames <- c('lrs', 'educrec11'
)
for (i in varnames) {
  EB[[i]] <- ifelse(EB[[i]] %in% c(97, 98),
                     NA, EB[[i]])
}

varnames <- c("regionBE",                  "regionFR",
              "regionNE",                   "regionDE",                   "regionIT",
              "regionLU",                   "regionDK",                   "regionIE",
              "regionUK",                   "regionGR",                   "regionES",
             "regionPT",                   "regionFI",                   "regionSE",
              "regionAU",                   "regionCY",                   "allregions",
              "regionCZ",                   "regionEE",                   "regionHU",
               "regionLV",                   "regionLT",                   "regionPL",
             "regionSK",                   "regionSI",                   "regionBG",
             "regionRO",                   "regionTR",                   "regionHR",
               "regionTCC",                  "regionMK",                   "regionRS",
              "regioneME",                  "regionAL"
)
for (i in varnames) {
  EB[[i]] <- ifelse(EB[[i]] %in% c(99),
                     NA, EB[[i]])
}

EB$age_gr <- cut(EB$age,
                  breaks = c(14, 25, 35, 45, 55, 65, 100),
                  labels = c("15-24", "25-34","35-44","45-54",
                             "55-64","65+"),
                  right = FALSE)

EB$country <- recode(EB$country, "'DE-E' = 'DE'; 'DE-W' = 'DE';
                       'GB-GBN' = 'UK'; 'GB-NIR' = 'UK'")

EU15<- filter(EB, country=='AT'|country=='BE'|country=='DE'|
                country=='DK'|country=='ES'|country=='FI' |
                country=='FR'|country=='UK'|country=='GR'|
                country=='IE'|country=='IT'|country=='NL'|
                country=='PT'|country=='SE'|country=='LU')

EU5 <- filter(EB, country=='DE'|
                   country=='ES'|
                   country=='FR'|country=='UK'|country=='IT')


EB$betteroutsideeurec <- recode(EB$betteroutsideeu, "1:2 = 1; 3:4 = 0")
EU15$betteroutsideeurec <- recode(EU15$betteroutsideeu, "1:2 = 1; 3:4 = 0")
EU5$betteroutsideeurec <- recode(EU5$betteroutsideeu, "1:2 = 1; 3:4 = 0")

ggplot(EU15[!is.na(EU15$betteroutsideeurec), ],
       aes(x=betteroutsideeurec)) +
  geom_histogram(aes(y=0.5*..density.., fill = as.factor(..x..)), binwidth=0.5)+
labs(x="Better future outside EU",
     y="Percent")+
  ggtitle("") +
  facet_wrap(~country)+
  scale_y_continuous(labels = percent)+
  scale_x_continuous(breaks=c(0,1), labels=c("disagree", "agree"))+
  guides(fill=FALSE)

source("http://pcwww.liv.ac.uk/~william/R/crosstab.r")
crosstab(EU15, row.vars = "country", col.vars = c("betteroutsideeurec"), type = "r")


betteroutsideUK <-EU15 %>%
  filter(!is.na(betteroutsideeurec), country=="UK") %>%
  group_by(age_gr) %>%
  summarise(meanbetterout = mean(betteroutsideeurec, na.rm=T))

betteroutside5 <-EU5 %>%
  filter(!is.na(betteroutsideeurec)) %>%
  group_by(country, age_gr) %>%
  summarise(meanbetterout = mean(betteroutsideeurec, na.rm=T))

ggplot(betteroutsideUK, aes(x=age_gr, y=meanbetterout)) + geom_point()+geom_line(group=1)+
  labs(x='Age group', y="Percentage who agree")+
  ggtitle("Better future outside EU")+
  scale_y_continuous(labels = percent)

ggplot(betteroutside5, aes(x=age_gr, y=meanbetterout, color=country, group=country)) +
  geom_point()+geom_line()+
  labs(x='Age group', y="Percentage who agree")+
  ggtitle("Better future outside EU")+
  scale_y_continuous(labels = percent)

EB$trustep <- ifelse(EB$trustep==2, 0, EB$trustep)
trustep <-EB %>%
  filter(!is.na(trustep)) %>%
  group_by(year, nation1) %>%
  summarise(meantrustep = mean(trustep, na.rm=T)) %>%
  arrange(year)

ggplot(trustep, aes(x=year, y=meantrustep)) + geom_line()+
  labs(x='Year', y="Proportion who agree")+
  ggtitle("Tend to trust EP")+
  facet_wrap(~nation1)

