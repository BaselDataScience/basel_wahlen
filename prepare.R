library(readxl)
library(philentropy)

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
