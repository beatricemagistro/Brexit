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
library(simcf)
library(tile)
library(RColorBrewer)
library(MASS)
library(WhatIf)
source("http://pcwww.liv.ac.uk/~william/R/crosstab.r")


# Load data 

load("EB.Rdata")

# Look at data
describe(EB)

# recode missing values into NAs
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

#recode variables

# age group

EB$age_gr <- cut(EB$age,
                  breaks = c(14, 25, 35, 45, 55, 65, 100),
                  labels = c("15-24", "25-34","35-44","45-54",
                             "55-64","65+"),
                  right = FALSE)

# recode countries: merge two Germanys, and merge Northern Ireland with rest of GB
EB$country <- recode(EB$country, "'DE-E' = 'DE'; 'DE-W' = 'DE';
                      'GB-GBN' = 'UK'; 'GB-NIR' = 'UK'")
# recode sex 
EB$male <- recode(EB$sex, "'Man' = 1; 'Woman' = 0")
EB$male <- as.integer(EB$male)
EB$male <- recode(EB$male, "1 = 1; 2 = 0")

# only keep UK

UK <- filter(EB, country=='UK')

# let's recode variables and remove dk
varnames <- c('situationatecon',  'finasituation',
              'globopportunity'
)
for (i in varnames) {
  UK[[i]] <- ifelse(UK[[i]] %in% c(5),
                    NA, UK[[i]])
}

## Nice colors
brewer <- brewer.pal(9, "Set1")
red <- brewer[1]
blue <- brewer[2]
green <- brewer[3]
purple <- brewer[4]
orange <- brewer[5]
nicegray <- "gray45"


## Likelihood for 4 category ordered probit
llk.oprobit4 <- function(param, x, y) {
  # preliminaries
  os <- rep(1, nrow(x))
  x <- cbind(os, x)  
  b <- param[1:ncol(x)]
  t2 <- param[(ncol(x)+1)]
  t3 <- param[(ncol(x)+2)]
  
  # probabilities and penalty function
  xb <- x%*%b
  p1 <- log(pnorm(-xb))
  if (t2<=0)  p2 <- -(abs(t2)*10000)    # penalty function to keep t2>0
  else p2 <- log(pnorm(t2-xb)-pnorm(-xb))
  if (t3<=t2) p3 <- -((t2-t3)*10000)    # penalty to keep t3>t2
  else p3 <- log(pnorm(t3-xb)-pnorm(t2-xb))     
  p4 <- log(1-pnorm(t3-xb)) 
  
  # -1 * log likelihood (optim is a minimizer)
  -sum(cbind(y==1,y==2,y==3,y==4) * cbind(p1,p2,p3,p4))
}

# Model specification (for polr, simcf)
model <- betteroutsideeu ~ situationatecon + finasituation + feeleuimmigration +
  feelnoneuimmigration + globopportunity +
  male + soclass + age + educ + lrs

# betteroutsideeu: 1 st agree, 2 agree, 3 disagree, 4 st disagree
# situationnatecon: 1 very good, 2 rather good, 3 rather bad, 4 very bad, 5 dk
# finasituation: 1 very good, 2 rather good, 3 rather bad, 4 very bad, 5 dk
# feeleuimmigration: 1 very positive, 2 fairly positive, 3 fairly negative, 4 very negative, 5dk
# feelnoneuimmigration: 1 very positive, 2 fairly positive, 3 fairly negative, 4 very negative, 5dk
# globopportunity: 1 totally agree, 2 agree, 3 disagree, 4 totally disagree, 5 dk
# male: 1 male 0 female
# soclass: 1 working class, 2 low middle class, 3 middle class, 4 high middle class, 5 high class
# age
# educ: age when finished education
# lrs: left to right scale 1 left 10 right

# we also have occupation scale (occupscale), immigrants contribute (immigrantscontribute),
# questions on EU knowledge:
# euknownomemb: 28 member states: 1 True (CORRECT), 2 false, 3 dk
# euknowepelect: EP members are elected: 1 True (CORRECT), 2 false, 3 dk
# euknowswimemb: switzerland is member 1 True, 2 false (CORRECT), 3 dk

#delete missing data
mdata <- extractdata(model, UK, na.rm=TRUE) 

## Data from 2015
y <- mdata$betteroutsideeu   #better future outside eu

x <- cbind(mdata$situationatecon, mdata$finasituation, mdata$feeleuimmigration, 
           mdata$feelnoneuimmigration, mdata$globopportunity, mdata$male,
           mdata$soclass, mdata$age, mdata$educ, mdata$lrs)


# Use optim directly to get MLE
ls.result <- lm(model, data=mdata)   # use ls estimates as starting values
stval <- c(coef(ls.result),1,2)          # initial guesses
oprobit <- optim(stval, llk.oprobit4, method="BFGS", x=x, y=y, hessian=TRUE)
pe <- oprobit$par                # point estimates
vc <- solve(oprobit$hessian)     # var-cov matrix
se <- sqrt(diag(vc))                 # standard errors
ll <- -oprobit$value             # likelihood at maximum

# Simulate parameters from predictive distributions
sims <- 10000
simbetas <- mvrnorm(sims, pe, vc)       # draw parameters, using MASS::mvrnorm

# Create example counterfactuals
xhyp <- cfMake(model, mdata, nscen=20)

xhyp <- cfName(xhyp,"nat econ = very good", scen=1)
xhyp <- cfChange(xhyp, "situationnatecon", x=1, xpre=4, scen=1)

xhyp <- cfName(xhyp, "nat econ = very bad", scen=2)
xhyp <- cfChange(xhyp, "situationnatecon", x=4, xpre=1, scen=2)

xhyp <- cfName(xhyp, "fin situation = very good", scen=3)
xhyp <- cfChange(xhyp, "finasituation", x=1, xpre=4, scen=3)

xhyp <- cfName(xhyp, "fin situation = very bad", scen=4)
xhyp <- cfChange(xhyp, "male", x=4, xpre=1, scen=4)

xhyp <- cfName(xhyp, "eu immigration = very positive", scen=5)
xhyp <- cfChange(xhyp, "feeleuimmigration", x=1, xpre=4, scen=5)

xhyp <- cfName(xhyp, "eu immigration = very negative", scen=6)
xhyp <- cfChange(xhyp, "feeleuimmigration", x=4, xpre=1, scen=6)

xhyp <- cfName(xhyp, "non-eu immigration = very positive", scen=7)
xhyp <- cfChange(xhyp, "feelnoneuimmigration", x=1, xpre=4, scen=7)

xhyp <- cfName(xhyp, "non-eu immigration = very negative", scen=8)
xhyp <- cfChange(xhyp, "feelnoneuimmigration", x=4, xpre=1, scen=8)

xhyp <- cfName(xhyp, "globalization=strongly approves", scen=9)
xhyp <- cfChange(xhyp, "globopportunity", x=1, xpre=4, scen=9)

xhyp <- cfName(xhyp, "globalization=strongly disapproves", scen=10)
xhyp <- cfChange(xhyp, "globopportunity", x=4, xpre=1, scen=10)

xhyp <- cfName(xhyp, "male", scen=11)
xhyp <- cfChange(xhyp, "male", x=1, xpre=0, scen=11)

xhyp <- cfName(xhyp, "female", scen=12)
xhyp <- cfChange(xhyp, "male", x=0, xpre=1, scen=12)

xhyp <- cfName(xhyp, "Age + 1sd = 72", scen=13)
xhyp <- cfChange(xhyp, "age",
                 x=mean(na.omit(mdata$age))+sd(na.omit(mdata$age)),
                 xpre=mean(na.omit(mdata$age)),
                 scen=13)

xhyp <- cfName(xhyp, "Age - 1sd = 36", scen=14)
xhyp <- cfChange(xhyp, "age",
                 x=mean(na.omit(mdata$age))-sd(na.omit(mdata$age)),
                 xpre=mean(na.omit(mdata$age)),
                 scen=14)

xhyp <- cfName(xhyp, "Educ years-1sd=14", scen=15)
xhyp <- cfChange(xhyp, "educ", x=14, xpre=22, scen=15)

xhyp <- cfName(xhyp,"Educ years+1sd=22", scen=16)
xhyp <- cfChange(xhyp, "educ", x=22, xpre=14, scen=16)

xhyp <- cfName(xhyp,"High social class", scen=17)
xhyp <- cfChange(xhyp, "soclass",
                 x=5,
                 xpre=1,
                 scen=17)

xhyp <- cfName(xhyp,"Working class", scen=18)
xhyp <- cfChange(xhyp, "soclass",
                 x=1,
                 xpre=5,
                 scen=18)

xhyp <- cfName(xhyp, "Right", scen=19)
xhyp <- cfChange(xhyp, "lrs",
                 x=mean(na.omit(mdata$lrs))+sd(na.omit(mdata$lrs)),
                 xpre=mean(na.omit(mdata$lrs)),
                 scen=19)

xhyp <- cfName(xhyp, "Left", scen=20)
xhyp <- cfChange(xhyp, "lrs",
                 x=mean(na.omit(mdata$lrs))-sd(na.omit(mdata$lrs)),
                 xpre=mean(na.omit(mdata$lrs)),
                 scen=20)

# Simulate expected probabilities (all four categories)
oprobit.ev <- oprobitsimev(xhyp, simbetas, cat=4)

# Simulate first differences (all four categories)
oprobit.fd <- oprobitsimfd(xhyp, simbetas, cat=4)

# Plot predicted probabilities for all four categories, sorted by size
sorted <- order(oprobit.ev$pe[,1])
scenNames <- row.names(xhyp$x)

trace1 <- ropeladder(x = oprobit.ev$pe[sorted,1],
                     lower = oprobit.ev$lower[sorted,1],
                     upper = oprobit.ev$upper[sorted,1],
                     labels = scenNames[sorted],
                     size=0.5,
                     lex=1.5,
                     lineend="square",
                     plot=1
)

trace2 <- ropeladder(x = oprobit.ev$pe[sorted,2],
                     lower = oprobit.ev$lower[sorted,2],
                     upper = oprobit.ev$upper[sorted,2],
                     size=0.5,
                     lex=1.5,
                     lineend="square",
                     plot=2
)

trace3 <- ropeladder(x = oprobit.ev$pe[sorted,3],
                     lower = oprobit.ev$lower[sorted,3],
                     upper = oprobit.ev$upper[sorted,3],
                     size=0.5,
                     lex=1.5,
                     lineend="square",
                     plot=3
)

trace4 <- ropeladder(x = oprobit.ev$pe[sorted,4],
                     lower = oprobit.ev$lower[sorted,4],
                     upper = oprobit.ev$upper[sorted,4],
                     size=0.5,
                     lex=1.5,
                     lineend="square",
                     plot=4
)

tile(trace1, trace2, trace3, trace4,
     limits = c(0,0.6),
     gridlines = list(type="xt"),
     topaxis=list(add=TRUE, at=c(0,0.1,0.2,0.3,0.4,0.5, 0.6)),
     xaxistitle=list(labels="probability"),
     topaxistitle=list(labels="probability"),
     plottitle=list(labels=c("Strongly Agree", "Agree",
                             "Disagree", "Strongly Disagree")),
     width=list(spacer=3),
     height = list(plottitle=3,xaxistitle=3.5,topaxistitle=3.5))

## Re-simulate, now collapsing presentation to two categories ("SD/D" vs "SA/A")

## Simulate expected probabilities (all four categories)
oprobit.evc <- oprobitsimev(xhyp, simbetas, cat=4,
                            recode=list(c(1,2), c(3,4)) )

## Simulate first differences (all four categories)
oprobit.fdc <- oprobitsimfd(xhyp, simbetas, cat=4,
                            recode=list(c(1,2), c(3,4)) )

## Make a new rl plot, EV of Dd vs aA

sorted <- order(oprobit.evc$pe[,1])
scenNames <- row.names(xhyp$x)

trace1b <- ropeladder(x = oprobit.evc$pe[sorted,1],
                      lower = oprobit.evc$lower[sorted,1],
                      upper = oprobit.evc$upper[sorted,1],
                      labels = scenNames[sorted],
                      size=0.65,
                      lex=1.75,
                      lineend="square",
                      plot=1
)

trace2b <- ropeladder(x = oprobit.evc$pe[sorted,2],
                      lower = oprobit.evc$lower[sorted,2],
                      upper = oprobit.evc$upper[sorted,2],
                      size=0.65,
                      lex=1.75,
                      lineend="square",
                      plot=2
)

tile(trace1b, trace2b,
     limits = c(0,0.85),
     gridlines = list(type="xt"),
     xaxis=list(at=c(0, 0.2,0.3,0.4, 0.5, 0.6, 0.7,0.8)),
     topaxis=list(add=TRUE, at=c(0, 0.2,0.3,0.4, 0.5, 0.6, 0.7,0.8)),
     xaxistitle=list(labels="probability"),
     topaxistitle=list(labels="probability"),
     plottitle=list(labels=c("Agree or Str Agree",
                             "Disagree or Str Disagree")),
     width=list(spacer=3),
     height = list(plottitle=3,xaxistitle=3.5,topaxistitle=3.5)
)

#################
### edit this ###
#################

# Create example counterfactuals -- for diffs
xhyp <- cfMake(model, mdata, nscen=10)

xhyp <- cfName(xhyp, "Econ literacy (Illiteracy)", scen=1)
xhyp <- cfChange(xhyp, "econ2correct", x=1, xpre=0, scen=1)

xhyp <- cfName(xhyp, "Male (Female)", scen=2)
xhyp <- cfChange(xhyp, "male", x=1, xpre=0, scen=2)

xhyp <- cfName(xhyp, "30 Year Olds (67)", scen=3)
xhyp <- cfChange(xhyp, "age",
                 x=mean(na.omit(db29$age))-sd(na.omit(db29$age)),
                 xpre=mean(na.omit(db29$age)),
                 scen=3)

xhyp <- cfName(xhyp,"College (High School)", scen=4)
xhyp <- cfChange(xhyp, "educ", x=16, xpre=12, scen=4)

xhyp <- cfName(xhyp,"High class (Working)", scen=5)
xhyp <- cfChange(xhyp, "soclass",
                 x=5, xpre=1,scen=5)

# Simulate expected probabilities (all four categories)
oprobit.evc <- oprobitsimev(xhyp, simbetas, cat=4,
                            recode=list(c(1,2), c(3,4)) )

# Simulate first differences (all four categories)
oprobit.fdc <- oprobitsimfd(xhyp, simbetas, cat=4,
                            recode=list(c(1,2), c(3,4)) )



# Make a new ropeladder plot, showing just change in probability of any agreement
sortedc <- rev(order(oprobit.fdc$pe[,2]))
scenNames <- row.names(xhyp$x)

trace1c <- ropeladder(x = oprobit.fdc$pe[sortedc,2],
                      lower = oprobit.fdc$lower[sortedc,2],
                      upper = oprobit.fdc$upper[sortedc,2],
                      labels = scenNames[sortedc],
                      size=0.65,
                      lex=1.75,
                      lineend="square",
                      plot=1
)


sigMark1 <- oprobit.fdc$pe[sortedc,2]
is.na(sigMark1) <- (oprobit.fdc$lower[sortedc,2]<0)# < because I am using disagree and I have negative values
traceSig1 <- ropeladder(x=sigMark1,
                        col="white",
                        group=1,
                        plot=1)


vertmark <- linesTile(x=c(0,0), y=c(0,1), plot=1)

tile(trace1c, vertmark, traceSig1, 
     limits=c(-0.20,0.20),
     gridlines=list(type="xt"),
     topaxis=list(add=TRUE, at=seq(from=-0.2, to=0.2, by=0.05),
                  labels=c("-20%","-15%","-10%","-5%","0%","+5%", "+10%", "+15%", "+20%")),
     xaxis=list(at=seq(from=-0.2, to=0.2, by=0.05), 
                labels=c("-20%","-15%","-10%","-5%","0%","+5%", "+10%", "+15%", "+20%")),
     xaxistitle=list(labels="difference in probability disagree or strongly disagree"),
     topaxistitle=list(labels="difference in probability disagree or strongly disagree"),
     plottitle=list(labels="\"Globalization is opportunity\""),
     width=list(plot=2),
     height=list(plottitle=3,xaxistitle=3.5,topaxistitle=3.5)
)
