
#import babyname datasets ----
library(tidyverse)
#read_plus reads all files in a folder into a data.frame
read_plus <- function(flnm) {
  read_csv(flnm, col_names = FALSE, col_types = cols(.default = "c")) %>% 
    mutate(filename = flnm)
}

tbl_with_sources <-
  list.files(path = "./babynames/",
             pattern = "*.txt", 
             full.names = T) %>% 
  map_df(~read_plus(.))

#clean uo data ----
colnames(tbl_with_sources) <- c("name","sex","popularity","year")
year <- gsub("\\D", "", tbl_with_sources$year)
tbl_with_sources$year <- year
head(tbl_with_sources)

#extract a single name ----
chad <- tbl_with_sources[tbl_with_sources$name=="Chad" & tbl_with_sources$sex=="M",]
plot(chad$year,chad$popularity)

sarah <- tbl_with_sources[tbl_with_sources$name=="Sarah" & tbl_with_sources$sex=="F",]
plot(sarah$year,sarah$popularity)

evelyn <- tbl_with_sources[tbl_with_sources$name=="Evelyn" & tbl_with_sources$sex=="F",]
plot(evelyn$year,evelyn$popularity)

#get top 50 male and female names ----
by_pop <- tbl_with_sources[order(as.numeric(as.character(tbl_with_sources$popularity)), decreasing=TRUE),]
M_by_pop <- by_pop[by_pop$sex=="M",]
F_by_pop <- by_pop[by_pop$sex=="F",]
M_modern_pop <- M_by_pop[M_by_pop$year>1980,]
F_modern_pop <- F_by_pop[F_by_pop$year>1980,]
M_old_pop <- M_by_pop[M_by_pop$year<1950,]
F_old_pop <- F_by_pop[F_by_pop$year<1950,]
#filter out repeated names to get a final list of names sorted by those with highest peaks
M_unique_pop <- unique(M_by_pop$name, fromLast = FALSE, incomparables = FALSE)
M_unique_modern_pop <- unique(M_modern_pop$name, fromLast = FALSE, incomparables = FALSE)
F_unique_pop <- unique(F_by_pop$name, fromLast = FALSE, incomparables = FALSE)
F_unique_modern_pop <- unique(F_modern_pop$name, fromLast = FALSE, incomparables = FALSE)
F_unique_old_pop <- unique(F_old_pop$name, fromLast = FALSE, incomparables = FALSE)
M_unique_old_pop <- unique(M_old_pop$name, fromLast = FALSE, incomparables = FALSE)
head(M_unique_old_pop, 50)

#top 30 from both eras
top_30_M_old <- M_unique_old_pop[1:30]
top_30_M_modern <- M_unique_modern_pop[1:30]
top_30_F_old <- F_unique_old_pop[1:30]
top_30_F_modern <- F_unique_modern_pop[1:30]
top_30_M <- c(top_30_M_old,top_30_M_modern)
top_50_M <- unique(top_30_M, fromLast = FALSE, incomparables = FALSE)
top_30_F <- c(top_30_F_old,top_30_F_modern)
top_50_F <- unique(top_30_F, fromLast = FALSE, incomparables = FALSE)
top_50_F <- top_50_F[1:50]
top_50_M <- top_50_M[1:50]
#convert top 100 names to data.frame----

babyNames <- list()
for (i in 1:length(top_50_M)){
  title <- vector()
  highlights <- vector()
  blurb <- vector()
  x_label <- vector()
  y_label <- vector()
  x_values <- vector()
  y_values <- vector()
  x_axis <- vector()
  y_axis <- vector()
  title <- c(title, c('the popularity of the name', 'over time in the U.S.'))
  blurb <- c(blurb, 'data sourced from https://www.ssa.gov/oact/babynames/limits.html')
  highlights <- c(highlights, {top_50_M[i]})
  x_label <- c(x_label, 'year')
  y_label <- c(y_label, 'births')
  tmp_births <- c(tbl_with_sources[tbl_with_sources$name==top_50_M[i] & tbl_with_sources$sex=="M","popularity"])
  tmp_births <- as.numeric(unlist(tmp_births))
  tmp_years <- c(tbl_with_sources[tbl_with_sources$name==top_50_M[i] & tbl_with_sources$sex=="M","year"])
  tmp_years <- as.numeric(unlist(tmp_years))
  x_values <- c(x_values, c(tmp_years))
  y_values <- c(y_values, c(tmp_births))
  x_axis <- c(x_axis, c(seq(from = 1880, to = 2020, by = 20)))
  y_axis <- c(y_axis, c(round(seq.int(0, max(tmp_births)*runif(1, 1.1,1.5), length.out = 11))))
  tmp_name <- list("title" = title, 
                   "blurb" = blurb,
                   "x_label" = x_label,
                   "y_label" = y_label,
                   "x_values" = x_values,
                   "y_values" = y_values,
                   "x_axis" = x_axis,
                   "y_axis" = y_axis
  )
  babyNames <- append(babyNames, list(tmp_name))
}
for (i in 1:length(top_50_F)){
  title <- vector()
  highlights <- vector()
  blurb <- vector()
  x_label <- vector()
  y_label <- vector()
  x_values <- vector()
  y_values <- vector()
  x_axis <- vector()
  y_axis <- vector()
  title <- c(title, c('the popularity of the name', 'over time in the U.S.'))
  blurb <- c(blurb, 'data sourced from https://www.ssa.gov/oact/babynames/limits.html')
  highlights <- c(highlights, {top_50_F[i]})
  x_label <- c(x_label, 'year')
  y_label <- c(y_label, 'births')
  tmp_births <- c(tbl_with_sources[tbl_with_sources$name==top_50_F[i] & tbl_with_sources$sex=="F","popularity"])
  tmp_births <- as.numeric(unlist(tmp_births))
  tmp_years <- c(tbl_with_sources[tbl_with_sources$name==top_50_F[i] & tbl_with_sources$sex=="F","year"])
  tmp_years <- as.numeric(unlist(tmp_years))
  x_values <- c(x_values, c(tmp_years))
  y_values <- c(y_values, c(tmp_births))
  x_axis <- c(x_axis, c(seq(from = 1880, to = 2020, by = 20)))
  y_axis <- c(y_axis, c(round(seq.int(0, max(tmp_births)*runif(1, 1.1,1.5), length.out = 11))))
  tmp_name <- list("title" = title, 
                   "blurb" = blurb,
                   "x_label" = x_label,
                   "y_label" = y_label,
                   "x_values" = x_values,
                   "y_values" = y_values,
                   "x_axis" = x_axis,
                   "y_axis" = y_axis
  )
  babyNames <- append(babyNames, list(tmp_name))
}
?max
babyNames
library(rjson)
JSONBabyNames <- toJSON(babyNames, indent=0)
JSONBabyNames
write(JSONBabyNames, file="babyNames.JSON")
