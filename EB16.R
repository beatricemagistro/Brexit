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
library("haven")
library("readr")
library("sjmisc")
library("car")
library("grep")
library(tidyr)
library(scales)
library(simcf)
library(tile)
library(verification)
library(RColorBrewer)
library(MASS)
library(WhatIf)
library(boot)
library(lmtest)
source("http://pcwww.liv.ac.uk/~william/R/crosstab.r")

#clear memory
rm(list=ls())
#load data
load("EB16.Rdata")

#transform variables from factors to integers for simulations

varnames <- c('finaexp', 'econexp','situationatecon', 'finasituation','jobsituation',
              'feeleuimmigration', 'feelnoneuimmigration','jobexp',
              'betteroutsideeu', 'satisdmo', 'satisdeu', 'citizen', 
              'immigrantscontribute', 'helprefugees','heardep',
              'heardec', 'heardecb', 'globopportunity','occup','occup_rec','occup_scale',
              'euknownomemb', 'euknowswimemb', 'euknowepelect','euimage', 'soclass'
)
for (i in varnames) {
  EB16[[i]] <- as.integer(EB16[[i]])
}

# recode missing values into NAs

varnames <- c('finaexp', 'econexp','jobexp'
)
for (i in varnames) {
  EB16[[i]] <- ifelse(EB16[[i]] %in% c(4, 8, 9),
                      NA, EB16[[i]])
}

varnames <- c('situationatecon', 'finasituation','jobsituation',
              'feeleuimmigration', 'feelnoneuimmigration',
              'betteroutsideeu', 'satisdmo', 'satisdeu', 'citizen', 
              'immigrantscontribute', 'helprefugees','globopportunity'
)
for (i in varnames) {
  EB16[[i]] <- ifelse(EB16[[i]] %in% c(5, 6, 7, 8, 9),
                      NA, EB16[[i]])
}

varnames <- c('heardep',
              'heardec', 'heardecb', 
              'euknownomemb', 'euknowswimemb', 'euknowepelect')
for (i in varnames) {
  EB16[[i]] <- ifelse(EB16[[i]] %in% c(3, 8, 9),
                      NA, EB16[[i]])
}

varnames <- c('euimage', 'soclass' 
)
for (i in varnames) {
  EB16[[i]] <- ifelse(EB16[[i]] %in% c(6, 7, 8, 9),
                      NA, EB16[[i]])
}


EB16$educ <- ifelse(EB16$educ %in% c(0, 97, 98, 99),
                    NA, EB16$educ)

EB16$lrs <- ifelse(EB16$lrs %in% c(97, 98),
                   NA, EB16$lrs)

EB16$regionUK <- ifelse(EB16$regionUK %in% c(99),
                        NA, EB16$regionUK)

# recode countries: merge two Germanys, and merge Northern Ireland with rest of GB

EB16$country <- recode(EB16$country, "'DE-E' = 'DE'; 'DE-W' = 'DE';
                       'GB-GBN' = 'UK'; 'GB-NIR' = 'UK'")

# recode sex so to make it an integer too
EB16$male <- recode(EB16$sex, "'Man' = 1; 'Woman' = 0")
EB16$male <- as.integer(EB16$male)
EB16$male <- recode(EB16$male, "1 = 0; 2 = 1")

# only keep the UK

UK <- filter(EB16, country=='UK')

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

# Model specification 
model <- betteroutsideeu ~ finasituation + feeleuimmigration +
  feelnoneuimmigration + globopportunity + occup_scale +
  male + age + educ + lrs

# betteroutsideeu: 1 st agree, 2 agree, 3 disagree, 4 st disagree
# finasituation: 1 very good, 2 rather good, 3 rather bad, 4 very bad, 5 dk
# jobsituation: 1 very good, 2 rather good, 3 rather bad, 4 very bad, 5 dk
# feeleuimmigration: 1 very positive, 2 fairly positive, 3 fairly negative, 4 very negative, 5dk
# feelnoneuimmigration: 1 very positive, 2 fairly positive, 3 fairly negative, 4 very negative, 5dk
# globopportunity: 1 totally agree, 2 agree, 3 disagree, 4 totally disagree, 5 dk
# male: 1 male 0 female
# occup_scale: 1 self employed, 2 managers, 3 other white collars, 4 manual workers,
# 5 house persons, 6 unemployed, 7 retired, 8 students
# age
# educ: age when finished education
# lrs: left to right scale 1 left 10 right

#delete missing data
mdata <- extractdata(model, UK, na.rm=TRUE) 

#check for multicollinearity

cor(mdata, use="complete.obs", method="kendall")

## Data from 2015
y <- mdata$betteroutsideeu   #better future outside eu

x <- cbind(mdata$finasituation, mdata$feeleuimmigration, 
           mdata$feelnoneuimmigration, mdata$globopportunity,
           mdata$occup_scale,
           mdata$male,
           mdata$age, mdata$educ, mdata$lrs)


# Use optim directly to get MLE
ls.result <- lm(model, data=mdata)   # use ls estimates as starting values
stval <- c(coef(ls.result),1,2)          # initial guesses
oprobit <- optim(stval, llk.oprobit4, method="BFGS", x=x, y=y, hessian=TRUE)
pe <- oprobit$par                # point estimates
vc <- solve(oprobit$hessian)     # var-cov matrix
se <- sqrt(diag(vc))                 # standard errors
ll <- -oprobit$value             # likelihood at maximum

#vif

vif(ls.result)

# Simulate parameters from predictive distributions
sims <- 10000
simbetas <- mvrnorm(sims, pe, vc)       # draw parameters, using MASS::mvrnorm

# Create example counterfactuals
xhyp <- cfMake(model, mdata, nscen=18)

xhyp <- cfName(xhyp, "Right", scen=1)
xhyp <- cfChange(xhyp, "lrs",
                 x=mean(na.omit(mdata$lrs))+sd(na.omit(mdata$lrs)),
                 xpre=mean(na.omit(mdata$lrs)),
                 scen=1)

xhyp <- cfName(xhyp, "Left", scen=2)
xhyp <- cfChange(xhyp, "lrs",
                 x=mean(na.omit(mdata$lrs))-sd(na.omit(mdata$lrs)),
                 xpre=mean(na.omit(mdata$lrs)),
                 scen=2)


xhyp <- cfName(xhyp, "Fin situation = very good", scen=3)
xhyp <- cfChange(xhyp, "finasituation", x=1, xpre=4, scen=3)

xhyp <- cfName(xhyp, "Fin situation = very bad", scen=4)
xhyp <- cfChange(xhyp, "finasituation", x=4, xpre=1, scen=4)

xhyp <- cfName(xhyp, "EU immigration = very positive", scen=5)
xhyp <- cfChange(xhyp, "feeleuimmigration", x=1, xpre=4, scen=5)

xhyp <- cfName(xhyp, "EU immigration = very negative", scen=6)
xhyp <- cfChange(xhyp, "feeleuimmigration", x=4, xpre=1, scen=6)

xhyp <- cfName(xhyp, "Non-EU immigration = very positive", scen=7)
xhyp <- cfChange(xhyp, "feelnoneuimmigration", x=1, xpre=4, scen=7)

xhyp <- cfName(xhyp, "Non-EU immigration = very negative", scen=8)
xhyp <- cfChange(xhyp, "feelnoneuimmigration", x=4, xpre=1, scen=8)

xhyp <- cfName(xhyp, "Globalization=strongly approves", scen=9)
xhyp <- cfChange(xhyp, "globopportunity", x=1, xpre=4, scen=9)

xhyp <- cfName(xhyp, "Globalization=strongly disapproves", scen=10)
xhyp <- cfChange(xhyp, "globopportunity", x=4, xpre=1, scen=10)

xhyp <- cfName(xhyp, "Male", scen=11)
xhyp <- cfChange(xhyp, "male", x=1, xpre=0, scen=11)

xhyp <- cfName(xhyp, "Female", scen=12)
xhyp <- cfChange(xhyp, "male", x=0, xpre=1, scen=12)

xhyp <- cfName(xhyp, "Age + 1sd = 69", scen=13)
xhyp <- cfChange(xhyp, "age",
                 x=mean(na.omit(mdata$age))+sd(na.omit(mdata$age)),
                 xpre=mean(na.omit(mdata$age)),
                 scen=13)

xhyp <- cfName(xhyp, "Age - 1sd = 35", scen=14)
xhyp <- cfChange(xhyp, "age",
                 x=mean(na.omit(mdata$age))-sd(na.omit(mdata$age)),
                 xpre=mean(na.omit(mdata$age)),
                 scen=14)

xhyp <- cfName(xhyp, "Educ years-1sd=15", scen=15)
xhyp <- cfChange(xhyp, "educ", x=mean(na.omit(mdata$educ))-sd(na.omit(mdata$educ)),
                 xpre=mean(na.omit(mdata$educ)), scen=15)

xhyp <- cfName(xhyp,"Educ years+1sd=23", scen=16)
xhyp <- cfChange(xhyp,"educ", x=mean(na.omit(mdata$educ))+sd(na.omit(mdata$educ)),
                 xpre=mean(na.omit(mdata$educ)), scen=16)

xhyp <- cfName(xhyp,"Blue collar", scen=17)
xhyp <- cfChange(xhyp, "occup_scale",
                 x=4,
                 xpre=3,
                 scen=17)

xhyp <- cfName(xhyp,"White collar", scen=18)
xhyp <- cfChange(xhyp, "occup_scale",
                 x=3,
                 xpre=4,
                 scen=18)


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
     limits = c(0,1),
     gridlines = list(type="xt"),
     xaxis=list(at=c(0.1,0.2,0.3,0.4, 0.5, 0.6, 0.7,0.8,0.9)),
     topaxis=list(add=TRUE, at=c(0.1,0.2,0.3,0.4, 0.5, 0.6, 0.7,0.8,0.9)),
     xaxistitle=list(labels="probability"),
     topaxistitle=list(labels="probability"),
     plottitle=list(labels=c("Agree or Str Agree",
                             "Disagree or Str Disagree")),
     width=list(spacer=3),
     height = list(plottitle=3,xaxistitle=3.5,topaxistitle=3.5)
)

# Create example counterfactuals -- for diffs
xhyp <- cfMake(model, mdata, nscen=9)

xhyp <- cfName(xhyp, "Left(Right)", scen=1)
xhyp <- cfChange(xhyp, "lrs", x=mean(na.omit(mdata$lrs))-sd(na.omit(mdata$lrs)),
                 xpre=mean(na.omit(mdata$lrs))+sd(na.omit(mdata$lrs)), scen=1)

xhyp <- cfName(xhyp, "Male (Female)", scen=2)
xhyp <- cfChange(xhyp, "male", x=1, xpre=0, scen=2)

xhyp <- cfName(xhyp, "35 Year Olds (69)", scen=3)
xhyp <- cfChange(xhyp, "age",
                 x=mean(na.omit(mdata$age))-sd(na.omit(mdata$age)),
                 xpre=mean(na.omit(mdata$age))+sd(na.omit(mdata$age)),
                 scen=3)

xhyp <- cfName(xhyp,"Educ years +1sd (Educ-1sd)", scen=4)
xhyp <- cfChange(xhyp, "educ", x=mean(na.omit(mdata$age))+sd(na.omit(mdata$age)),
                 xpre=mean(na.omit(mdata$age))-sd(na.omit(mdata$age)), scen=4)

xhyp <- cfName(xhyp, "Fin situation v good(V Negative)", scen=5)
xhyp <- cfChange(xhyp, "finasituation", x=1, xpre=4, scen=5)

xhyp <- cfName(xhyp, "EU immigration v positive (Negative)", scen=6)
xhyp <- cfChange(xhyp, "feeleuimmigration", x=1, xpre=4, scen=6)

xhyp <- cfName(xhyp, "Globalization v positive (Negative)", scen=7)
xhyp <- cfChange(xhyp, "globopportunity", x=1, xpre=4, scen=7)

xhyp <- cfName(xhyp, "Non-EU immigration v positive (V Negative)", scen=8)
xhyp <- cfChange(xhyp, "feelnoneuimmigration", x=1, xpre=4, scen=8)

xhyp <- cfName(xhyp, "Blue collar (White collar)", scen=9)
xhyp <- cfChange(xhyp, "occup_scale", x=4, xpre=3, scen=9)

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
is.na(sigMark1) <- (oprobit.fdc$lower[sortedc,2]>0)
traceSig1 <- ropeladder(x=sigMark1,
                        col="white",
                        group=1,
                        plot=1)


vertmark <- linesTile(x=c(0,0), y=c(0,1), plot=1)

tile(trace1c, vertmark, traceSig1, 
     limits=c(-0.25,0.75),
     gridlines=list(type="xt"),
     topaxis=list(add=TRUE, at=seq(from=-0.25, to=0.75, by=0.05),
                  labels=c("-25%","-20%","-15%","-10%","-5%","0%","+5%", 
                           "+10%", "+15%", "+20%","+25%","+30%",
                           "+35%","+40%","+45%","+50%","+55%","+60%","+65%","+70%","+75%")),
     xaxis=list(at=seq(from=-0.25, to=0.75, by=0.05), 
                labels=c("-25%","-20%","-15%","-10%","-5%","0%","+5%", 
                         "+10%", "+15%", "+20%","+25%",
                         "+30%","+35%","+40%","+45%","+50%","+55%","+60%","+65%","+70%","+75%")),
     xaxistitle=list(labels="difference in probability disagree or strongly disagree"),
     topaxistitle=list(labels="difference in probability disagree or strongly disagree"),
     plottitle=list(labels="\"Better future outside of EU\""),
     width=list(plot=2),
     height=list(plottitle=3,xaxistitle=3.5,topaxistitle=3.5)
)

# cross-validation

## A simple leave-one-out cross-validation function for multinom adapted for oprobit
# returns predicted probs
loocv <- function (obj, model, data) {
  ncat <- 4
  m <- nrow(mdata)
  form <- model
  loo <- matrix(NA, nrow=m, ncol=ncat)
  for (i in 1:m) {
    oprobit <- polr(as.factor(betteroutsideeu) ~ finasituation + feeleuimmigration +
                      feelnoneuimmigration + globopportunity + occup_scale +
                      male + age + educ + lrs, data=mdata[-i,], method="probit")
    loo[i,] <- predict(oprobit, newdata = mdata[i,], type="probs")
  }
  loo
}

#do ordered probit with polr, otherwise predict function won't work
oprobit2 <- polr(as.factor(betteroutsideeu) ~ finasituation + feeleuimmigration +
                   feelnoneuimmigration + globopportunity + occup_scale +
                   male + age + educ + lrs, data=mdata, method="probit")

predIS <- predict(oprobit2, type="probs")
predCV <- loocv(oprobit2, model, mdata)

ncat <- 4
predIScat <- apply(predIS, 1, function(x, ncat) order(x)[ncat], ncat=ncat)
predCVcat <- apply(predCV, 1, function(x, ncat) order(x)[ncat], ncat=ncat)

pcpIS <- mean(predIScat==mdata$betteroutsideeu)
pcpIS
pcpCV <- mean(predCVcat==mdata$betteroutsideeu)
pcpCV

#if this is correct, the model doesn't fit well. We need a bigger sample.


# should we do other tests comparing our model with the same model with no controls?

# likelihood ratio test
# BIC
# ROC
# Actual versus predicted plot

