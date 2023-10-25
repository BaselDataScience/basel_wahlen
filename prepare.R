library(readxl)
library(philentropy)

distinctness <- function(x) philentropy::H(table(x)/length(x)) / log(length(x), 2)

# read in data if not already present
if (!exists('dat0')) {
  download.file('https://data.bs.ch/api/explore/v2.1/catalog/datasets/100281/exports/xlsx', tf <- tempfile(fileext = '.xlsx'))
  dat0 <- readxl::read_xlsx(tf)
}

