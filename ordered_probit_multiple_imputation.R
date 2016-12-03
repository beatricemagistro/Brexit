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
library(Amelia)
library(mitools)

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

# only keep variables of interest

UK <- dplyr::select(UK, betteroutsideeu, finasituation, feeleuimmigration,
               feelnoneuimmigration, globopportunity, occup_scale,
               male, age,educ,lrs)

#model 

model <- as.factor(betteroutsideeu) ~ finasituation + feeleuimmigration +
  feelnoneuimmigration + globopportunity + occup_scale +
  male + age + educ + lrs

# impute NAs 
m <- 5
amelia_output <- amelia(UK, m = m,
                        ords = c("betteroutsideeu", "finasituation","feeleuimmigration",
                                 "feelnoneuimmigration", "globopportunity",
                                 "occup_scale", "educ", "lrs", "age"),
                        noms = c("male"))

miData <- amelia_output$imputations

# imputation

mi <- vector("list", m)

for (i in 1:m) {
  mi[[i]] <- polr(model, data=miData[[i]], method="probit")
}


sims <- 10000
simbetas <- NULL
for (i in 1:m) {
  simbetas <- rbind(simbetas,
                    mvrnorm(sims/m, c(mi[[i]]$coefficients, mi[[i]]$zeta), vcov(mi[[i]]))
  )
}


# Create example counterfactuals
xhyp <- cfMake(model, miData$imp1, nscen=18)

xhyp <- cfName(xhyp, "Right", scen=1)
xhyp <- cfChange(xhyp, "lrs",
                 x=mean(na.omit(miData$imp1$lrs))+sd(na.omit(miData$imp1$lrs)),
                 xpre=mean(na.omit(miData$imp1$lrs)),
                 scen=1)

xhyp <- cfName(xhyp, "Left", scen=2)
xhyp <- cfChange(xhyp, "lrs",
                 x=mean(na.omit(miData$imp1$lrs))-sd(na.omit(miData$imp1$lrs)),
                 xpre=mean(na.omit(miData$imp1$lrs)),
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

xhyp <- cfName(xhyp, "Age + 1sd = 73", scen=13)
xhyp <- cfChange(xhyp, "age",
                 x=mean(na.omit(miData$imp1$age))+sd(na.omit(miData$imp1$age)),
                 xpre=mean(na.omit(miData$imp1$age)),
                 scen=13)

xhyp <- cfName(xhyp, "Age - 1sd = 33", scen=14)
xhyp <- cfChange(xhyp, "age",
                 x=mean(na.omit(miData$imp1$age))-sd(na.omit(miData$imp1$age)),
                 xpre=mean(na.omit(miData$imp1$age)),
                 scen=14)

xhyp <- cfName(xhyp, "Mean Educ years=18", scen=15)
xhyp <- cfChange(xhyp, "educ", x=mean(na.omit(miData$imp1$educ)),
                 xpre=mean(na.omit(miData$imp1$educ))+sd(na.omit(miData$imp1$educ)), scen=15)

xhyp <- cfName(xhyp,"Educ years+1sd=22", scen=16)
xhyp <- cfChange(xhyp,"educ", x=mean(na.omit(miData$imp1$educ))+sd(na.omit(miData$imp1$educ)),
                 xpre=mean(na.omit(miData$imp1$educ)), scen=16)

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
oprobit.ev <- oprobitsimev(xhyp, simbetas, constant=NA, cat=4)

# Simulate first differences (all four categories)
oprobit.fd <- oprobitsimfd(xhyp, simbetas, constant=NA, cat=4)

## Re-simulate, now collapsing presentation to two categories ("SD/D" vs "SA/A")

## Simulate expected probabilities (all four categories)
oprobit.evc <- oprobitsimev(xhyp, simbetas, constant=NA, cat=4,
                            recode=list(c(1,2), c(3,4)) )

## Simulate first differences (all four categories)
oprobit.fdc <- oprobitsimfd(xhyp, simbetas, cat=4, constant=NA,
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
xhyp <- cfMake(betteroutsideeu ~ finasituation + feeleuimmigration +
                 feelnoneuimmigration + globopportunity + occup_scale +
                 male + age + educ + lrs, miData$imp1, nscen=9)

xhyp <- cfName(xhyp, "Left(Right)", scen=1)
xhyp <- cfChange(xhyp, "lrs", x=mean(na.omit(miData$imp1$lrs))-sd(na.omit(miData$imp1$lrs)),
                 xpre=mean(na.omit(miData$imp1$lrs))+sd(na.omit(miData$imp1$lrs)), scen=1)

xhyp <- cfName(xhyp, "Male (Female)", scen=2)
xhyp <- cfChange(xhyp, "male", x=1, xpre=0, scen=2)

xhyp <- cfName(xhyp, "33 Year Olds (73)", scen=3)
xhyp <- cfChange(xhyp, "age",
                 x=mean(na.omit(miData$imp1$age))-sd(na.omit(miData$imp1$age)),
                 xpre=mean(na.omit(miData$imp1$age))+sd(na.omit(miData$imp1$age)),
                 scen=3)

xhyp <- cfName(xhyp,"Educ years +1sd (Mean)", scen=4)
xhyp <- cfChange(xhyp, "educ", x=mean(na.omit(miData$imp1$educ))+sd(na.omit(miData$imp1$educ)),
                 xpre=mean(na.omit(miData$imp1$educ)), scen=4)

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
oprobit.evc <- oprobitsimev(xhyp, simbetas, constant=NA, cat=4,
                            recode=list(c(1,2), c(3,4)) )

# Simulate first differences (all four categories)
oprobit.fdc <- oprobitsimfd(xhyp, simbetas, constant=NA, cat=4,
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

# pcp

x <- cbind(miData$imp1$finasituation, miData$imp1$feeleuimmigration, 
           miData$imp1$feelnoneuimmigration, miData$imp1$globopportunity,
           miData$imp1$occup_scale,
           miData$imp1$male,
           miData$imp1$age, miData$imp1$educ, miData$imp1$lrs)
y <- miData$imp1$betteroutsideeu   #better future outside eu

oprobitm1 <- polr(model, data=miData$imp1, method="probit")

b <- c(oprobitm1$coefficients, oprobitm1$zeta)

pcp.oprobit <- function(x, y, b, constant=NA, ncat=4, type="model") { # other types:  null, improve
  
  require(simcf)
  b <- matrix(b,nrow=100,ncol=length(b),byrow=TRUE)
  simy <- oprobitsimev(x, b, constant=constant, cat=ncat)
  
  cats <- sort(unique(y))
  
  predcatN <- cats[rev(order(table(y)))][1]
  
  n <- length(y)
  pcp <- pcpNull <- predcatM <- rep(NA,n)
  for (i in 1:n) {
    predcatM[i] <- cats[rev(order(simy$pe[i,]))][1]
    pcp[i] <- predcatM[i]==y[i]
    pcpNull[i] <- predcatN==y[i]
  }
  
  pcp <- mean(pcp)
  pcpNull <- mean(pcpNull)
  pcpImprove <- (pcp-pcpNull)/(1-pcpNull)
  
  if (type=="model")
    return(pcp)
  if (type=="null")
    return(pcpNull)
  if (type=="improve")
    return(pcpImprove)
  
}

pcp.oprobit(x, y, b, constant=NA, ncat=4, type="model")
pcp.oprobit(x, y, b, constant=NA, ncat=4, type="null")
pcp.oprobit(x, y, b, constant=NA, ncat=4, type="improve")

# we have to adapt it to the new imputed data

## A simple leave-one-out cross-validation function for multinom adapted for oprobit
# returns predicted probs
loocv <- function (obj, model, data) {
  ncat <- 4
  nrow <- nrow(miData$imp1)
  form <- model
  loo <- matrix(NA, nrow=nrow, ncol=ncat)
  oprobit <- polr(model, data=miData$imp1[-i,], method="probit")
  for (i in 1:nrow) {
    loo[i,] <- predict(oprobit, newdata = miData$imp1[i,], type="probs")
  }
  loo
}

#do ordered probit with polr

predIS <- predict(oprobitm1, type="probs")
predCV <- loocv(oprobitm1, model, miData$imp1)

ncat <- 4
predIScat <- apply(predIS, 1, function(x, ncat) order(x)[ncat], ncat=ncat)
predCVcat <- apply(predCV, 1, function(x, ncat) order(x)[ncat], ncat=ncat)

pcpIS <- mean(predIScat==miData$imp1$betteroutsideeu)
pcpIS
pcpCV <- mean(predCVcat==miData$imp1$betteroutsideeu)
pcpCV


# should we do other tests comparing our model with the same model with no controls?

# likelihood ratio test
# BIC
# ROC
# Actual versus predicted plot


