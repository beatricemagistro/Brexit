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
source("http://pcwww.liv.ac.uk/~william/R/crosstab.r")

#data cleaning 

EB16 <- import("EB16.dta")
 save(EB16, file = "EB16.Rdata")

 load("EB16.Rdata")
 
EB16 <- EB16 %>%
  dplyr::select(isocntry, qa1a_1, qa1a_4, qa2a_2, qa2a_3, qa9, qa14_1, qa14_2, qa14_3,qa1a_3, 
          qa16_1, qa16_2, qa16_3, qa16t1, qa18a, qa18b, qa19a_4, qa19a_5, qd1_1, d15a_r1,qa2a_5,
          qd4_2, qd4_5, d1, d8, d10, d11, d15a_r2, d15a, d63, p7gb, w4, w92, qb4_1, qb4_2)
  save(EB16, file="EB16.Rdata")

# renaming
 
EB16 <- plyr::rename(EB16, c('isocntry'='country',
                              'w4'='weightuk',  'w92'='weighttotal',
                              'qa2a_3'='finaexp', 'qa1a_1'='situationatecon',
                              'qa1a_4'='finasituation', 'qa2a_5'='jobexp',
                              'qa2a_2'='econexp','qa1a_3'='jobsituation',
                              'qa9'='euimage',
                              'qb4_1'='feeleuimmigration',
                              'qb4_2'='feelnoneuimmigration',
                              'qa14_1'='heardep',
                              'qa14_2'='heardec',
                              'qa14_3'='heardecb',
                              'qa16_1'='euknownomemb',
                              'qa16_3'='euknowswimemb','qa16_2'='euknowepelect',
                             'qa16t1'='euknowcorrect',
                             'qa19a_4'='globopportunity',
                             'qa19a_5'='betteroutsideeu',
                            'qa18a'='satisdmo',
                              'qa18b'='satisdeu','d15a_r1'='occup_rec',
                              'qd1_1'='citizen','qd4_2'='immigrantscontribute',
                              'qd4_5'='helprefugees',
                              'd10'='sex',
                              'd63'='soclass',
                              'd11'='age','d8'='educ',
                              'd1'='lrs',
                              'd15a_r2'='occup_scale','d15a'='occup',
                              'p7gb'='regionUK'
                              ))
save(EB16, file="EB16.Rdata")
