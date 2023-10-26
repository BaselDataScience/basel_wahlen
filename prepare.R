library(readxl)
library(philentropy)
library(tidyr)

distinctness <- function(x) philentropy::H(table(x, useNA = 'ifany')/length(x)) / log(length(x), 2)

# read in data if not already present
if (!exists('dat0')) {
  download.file('https://data.bs.ch/api/explore/v2.1/catalog/datasets/100281/exports/xlsx', tf <- tempfile(fileext = '.xlsx'))
  dat0 <- readxl::read_xlsx(tf)
}

# determine constant columns, remove them to "general" dataframe
xx <- sapply(dat0, distinctness)
general <- unique(dat0[,names(xx[xx==0])])

dat1 <- dat0[, setdiff(names(dat0), names(general))]

# split into Wahlkreise
wahlkreise <- split(dat1, dat1$wahlkreisbezeichnung)
# concentrate on Kanton
kanton0 <- wahlkreise[['Kanton Basel-Stadt']]

# split out kanton constants
xx <- sapply(kanton0, distinctness)
kanton_general <- unique(kanton[,names(xx[xx==0])])

kanton1 <- kanton0[, setdiff(names(kanton0), names(kanton_general))]

## age distribution of candidates
age_distr <- function(dat, dataspec=NULL) {
  hist(dat$alter_am_jahresende_2023, freq = FALSE,
       main = paste('Altersverteilung der Kandidaten', dataspec), xlab = 'Kandidatenalter',
       breaks = seq(15,90,5))
  lines(density(dat$alter_am_jahresende_2023, adjust=.5), col='blue')
  lines(density(dat$alter_am_jahresende_2023), col='red')
}
age_distr(kanton1)

# by gender
par(mfrow=c(2,1))
age_distr(subset(kanton1, geschlecht=='F'), 'weiblich')
age_distr(subset(kanton1, geschlecht=='M'), 'männlich')
par(mfrow=c(1,1))
