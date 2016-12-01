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
library(nnet)
source("http://pcwww.liv.ac.uk/~william/R/crosstab.r")

#clear memory
rm(list=ls())
#load data
load("EB16.Rdata")

#transform variables from factors to integers for simulations

varnames <- c('finaexp', 'econexp','situationatecon', 'finasituation','jobsituation',
              'feeleuimmigration', 'feelnoneuimmigration','jobexp',
              'satisdmo', 'satisdeu', 'citizen', 
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
              'satisdmo', 'satisdeu', 'citizen', 
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

varnames <- c("betteroutsideeu"
)
for (i in varnames) {
  UK[[i]] <- ifelse(UK[[i]] %in% c("Inap. (not 1 in eu28)"),
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

# Model specification 
model <- betteroutsideeu ~ finasituation + feeleuimmigration +
  feelnoneuimmigration + globopportunity + occup_scale +
  male + age + educ + lrs

#delete missing data
mdata <- extractdata(model, UK, na.rm=TRUE) 

coef.multinom <- function(x) {
  nlevel <- length(mlogit.result$lev)
  ncoef <- length(mlogit.result$coefnames)
  coef <- x$wts[(ncoef+2):length(x$wts)]
  coef[-((0:(nlevel-2))*(ncoef+1) + 1)]
}

mlogit.result <- multinom(model, mdata, Hess=TRUE)
pe <- coef(mlogit.result)
pe
vc <- solve(mlogit.result$Hess)       # var-cov matrix
se <- sqrt(diag(vc))
se
ll <- (mlogit.result$deviance/-2)
ll

sims <- 10000
simbetas <- mvrnorm(sims,pe,vc)       # draw parameters, using MASS::mvrnorm
dim(simbetas)
simB <- array(NA, dim = c(sims,10,4))  # re-arrange simulates to array format
simB[,,1] <- simbetas[,1:10]           #   agree
simB[,,2] <- simbetas[,11:20]          # disagree
simB[,,3] <- simbetas[,21:30]           # totally disagree
simB[,,4] <- simbetas[,31:40]           #don't know

# Create full factorial set of counterfactuals for euimm
feeleuimmrange <- seq(1,4,by=1)
xhyp1 <- cfFactorial(formula=model,data=mdata,feeleuimmigration=feeleuimmrange)

# Simulate expected probabilities with 95% CI
mlogit.ev1 <- mlogitsimev(xhyp1, simB, ci=0.95)
mlogit.ev1

# Get 5 colors
cols <- brewer.pal(5,"Set1")

# Create one trace for each predicted category of the response, and each sex
trace1a <- lineplot(x=xhyp1$x$feeleuimmigration, #agree
                    y=mlogit.ev1$pe[,1],
                    lower=mlogit.ev1$lower[,1,],
                    upper=mlogit.ev1$upper[,1,],
                    ci=list(mark="shaded"),
                    plot=1
)

trace2a <- lineplot(x=xhyp1$x$feeleuimmigration, #disagree
                    y=mlogit.ev1$pe[,2],
                    lower=mlogit.ev1$lower[,2,],
                    upper=mlogit.ev1$upper[,2,],
                    ci=list(mark="shaded"),
                    plot=2
)

trace3a <- lineplot(x=xhyp1$x$feeleuimmigration, #strongly disagree
                    y=mlogit.ev1$pe[,3],
                    lower=mlogit.ev1$lower[,3,],
                    upper=mlogit.ev1$upper[,3,],
                    ci=list(mark="shaded"),
                    plot=3
)

trace4a <- lineplot(x=xhyp1$x$feeleuimmigration, #don't know
                    y=mlogit.ev1$pe[,4],
                    lower=mlogit.ev1$lower[,4,],
                    upper=mlogit.ev1$upper[,4,],
                    ci=list(mark="shaded"),
                    plot=4
)

trace5a <- lineplot(x=xhyp1$x$feeleuimmigration, #strongly agree
                    y=mlogit.ev1$pe[,5],
                    lower=mlogit.ev1$lower[,5,],
                    upper=mlogit.ev1$upper[,5,],
                    ci=list(mark="shaded"),
                    plot=5
)

at.x <- c(1,2,3,4)
at.y <- c(0,0.2,0.4,0.6,0.8,1)

# Plot traces using tile
tile(trace1a,
     trace2a,
     trace3a,
     trace4a,
     trace5a,
     RxC=c(2,3),
     limits = c(1,4,0,1),
     xaxis = list(at=at.x),
     yaxis = list(at=at.y, major=FALSE),
     xaxistitle = list(labels="EU immigration positive to negative"),
     maintitle = list(labels="Probability respondent thinks better future outside EU..."),
     plottitle = list(labels=c("Agree",
                                 "Disagree",
                                 "Strongly Disagree",
                                 "Dont know",
                                 "Strongly Agree")),
     height=list(columntitle=5),
     width=list(rowtitle=1.5),
     gridlines = list(type="xy")
)

# Create full factorial set of counterfactuals for noneuimm
feelnoneuimmrange <- seq(1,4,by=1)
xhyp1 <- cfFactorial(formula=model,data=mdata,feelnoneuimmigration=feelnoneuimmrange)

# Simulate expected probabilities with 95% CI
mlogit.ev1 <- mlogitsimev(xhyp1, simB, ci=0.95)
mlogit.ev1

# Get 5 colors
cols <- brewer.pal(5,"Set1")

# Create one trace for each predicted category of the response, and each sex
trace1a <- lineplot(x=xhyp1$x$feelnoneuimmigration, #agree
                    y=mlogit.ev1$pe[,1],
                    lower=mlogit.ev1$lower[,1,],
                    upper=mlogit.ev1$upper[,1,],
                    ci=list(mark="shaded"),
                    plot=1
)

trace2a <- lineplot(x=xhyp1$x$feelnoneuimmigration, #disagree
                    y=mlogit.ev1$pe[,2],
                    lower=mlogit.ev1$lower[,2,],
                    upper=mlogit.ev1$upper[,2,],
                    ci=list(mark="shaded"),
                    plot=2
)

trace3a <- lineplot(x=xhyp1$x$feelnoneuimmigration, #strongly disagree
                    y=mlogit.ev1$pe[,3],
                    lower=mlogit.ev1$lower[,3,],
                    upper=mlogit.ev1$upper[,3,],
                    ci=list(mark="shaded"),
                    plot=3
)

trace4a <- lineplot(x=xhyp1$x$feelnoneuimmigration, #don't know
                    y=mlogit.ev1$pe[,4],
                    lower=mlogit.ev1$lower[,4,],
                    upper=mlogit.ev1$upper[,4,],
                    ci=list(mark="shaded"),
                    plot=4
)

trace5a <- lineplot(x=xhyp1$x$feelnoneuimmigration, #strongly agree
                    y=mlogit.ev1$pe[,5],
                    lower=mlogit.ev1$lower[,5,],
                    upper=mlogit.ev1$upper[,5,],
                    ci=list(mark="shaded"),
                    plot=5
)

at.x <- c(1,2,3,4)
at.y <- c(0,0.2,0.4,0.6,0.8,1)

# Plot traces using tile
tile(trace1a,
     trace2a,
     trace3a,
     trace4a,
     trace5a,
     RxC=c(2,3),
     limits = c(1,4,0,1),
     xaxis = list(at=at.x),
     yaxis = list(at=at.y, major=FALSE),
     xaxistitle = list(labels="non EU immigration positive to negative"),
     maintitle = list(labels="Probability respondent thinks better future outside EU..."),
     plottitle = list(labels=c("Agree",
                               "Disagree",
                               "Strongly Disagree",
                               "Dont know",
                               "Strongly Agree")),
     height=list(columntitle=5),
     width=list(rowtitle=1.5),
     gridlines = list(type="xy")
)

