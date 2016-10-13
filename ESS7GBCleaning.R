#ESS GB 2014 Data Cleaning
#510 Brexit Project
#Oct. 12, 2016

library(rio)
library(dplyr)
library(ggplot2)
library(magrittr)


ESSGB7 <- import("ESS7GB.sav")
save(ESSGB7, file = "ESS7GB.Rdata")

table(ESSGB7$euftf)

ggplot(ESSGB7, aes(x = euftf)) + geom_bar()

E2 <- ESSGB7
E2 <- E2 %>% filter(!is.na(euftf))
E2$euftf <- as.numeric(E2$euftf)

E2$euftf2 <- ifelse((as.numeric(E2$euftf) >= 6), c("MORE UNIFICATION"), c("LESS UNIFICATION"))
table(E2$euftf2)
ggplot(E2, aes(x = euftf2)) + geom_bar()

E3 <- ESSGB7
E3 <- E3 %>% filter(!is.na(euftf))
E3$euftf2[E3$euftf == 1 | E3$euftf == 2 | E3$euftf == 3] <- "LESS UNIFICATION (STRONG)"
E3$euftf2[E3$euftf == 8 | E3$euftf == 9 | E3$euftf == "Unification go further"] <- "MORE UNIFICATION (STRONG)"
ggplot(E3, aes(x = euftf2)) + geom_bar()
ggplot(E3, aes(x = euftf)) + geom_bar()