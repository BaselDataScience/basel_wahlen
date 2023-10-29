library(data.tree)
library(dplyr)
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

# split into Wahlkreise
wahlkreise <- split(dat1, dat1$wahlkreisbezeichnung)
# concentrate on Kanton
kanton0 <- wahlkreise[['Kanton Basel-Stadt']]

# split out kanton constants
xx <- sapply(kanton0, distinctness)
kanton_general <- unique(kanton0[,names(xx[xx==0])])

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

### candidate votes
kanton1 %>% 
  dplyr::select(kandidaten_nr, parteikurzbezeichnung, bisher, gewahlt, ganzer_name, stimmen_total_aus_wahlzettel, hlv_nr) %>% 
  dplyr::arrange(desc(stimmen_total_aus_wahlzettel))

### Lists
table(kanton1$listen_nr, useNA = 'ifany')  # 32 lists run
lists <- kanton1 %>% 
  group_by(listen_nr, hlv_nr, ulv_nr, partei_id, parteikurzbezeichnung, parteibezeichnung) %>% 
  summarise(listenstimmen=mean(kandidatenstimmen_unveranderte_wahlzettel+kandidatenstimmen_veranderte_wahlzettel+
                                zusatzstimmen_unveranderte_wahlzettel+zusatzstimmen_veranderte_wahlzettel),
            kandidates=n())
  
sapply(lists, distinctness)

# tree structure of the lists:
lists$pathString <- paste('basel',
                          dplyr::coalesce(lists$hlv_nr, ''),
                          dplyr::coalesce(lists$ulv_nr, ''),
                          lists$listen_nr,
                          sep = '/'
                          )
zz <- data.tree::as.Node(lists)
print(zz, 'parteikurzbezeichnung')
# number of candidates per Hauptliste
zz$Do( function(x) {
  x$kandidaten_total <- Aggregate(node = x,
                       attribute = "kandidates",
                       aggFun = sum)
                  }
     )
data.tree::Get(zz$children, 'kandidaten_total')

## top-level votes
hl_votes <- sort(data.tree::Get(zz$children, function(x) data.tree::Aggregate(x, 'listenstimmen', sum)), decreasing = TRUE)
# 1st seat distribution
quorum1 <- ceiling(sum(hl_votes)/(4+1))
seats1 <- floor(hl_votes/quorum1)
# 2nd seat distribution
sort(hl_votes/(1+seats1), decreasing = TRUE)
