## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----eval=FALSE---------------------------------------------------------------
# output<- extract(varname, date, index, ncases=NULL,
#   unit="A", mult=1, begindt=NA, enddt=NA, npass=1,
#   smoothing=TRUE, endmonth=12)

## -----------------------------------------------------------------------------
library(DyadRatios)
data(jennings)
str(jennings$date)

## -----------------------------------------------------------------------------
library(lubridate)
temp <- data.frame(survey_date = c("2025-01-01", "2024-10-13", "2020-05-13"))
str(temp)
temp$survey_date <- ymd(temp$survey_date)
str(temp)

## -----------------------------------------------------------------------------
temp <- data.frame(year= c(2025, 2024, 2020), 
                   month = c(1, 10, 5), 
                   day = c(1, 13, 13))
temp$date <- lubridate::ymd(paste(temp$year, temp$month, temp$day, sep="-"))
str(temp)

## -----------------------------------------------------------------------------
temp <- data.frame(date = c("January 1, 1993", "Jan 1, 1993", "Aug 2, 2020"))
temp$date <- mdy(temp$date)

## -----------------------------------------------------------------------------
temp <- data.frame(date = ymd("2025-01-01", "1990-01-01", "1970-01-01", "1950-01-01"))
xl_temp <- tempfile(fileext = ".xlsx")
csv_temp <- tempfile(fileext = ".csv")

rio::export(temp, xl_temp)
rio::export(temp, csv_temp)


temp_xl <- rio::import(xl_temp)
temp_csv <- rio::import(csv_temp)

str(temp_xl)
str(temp_csv)


## -----------------------------------------------------------------------------
all(temp$date == temp_xl$date)
all(temp$date == temp_csv$date)

## -----------------------------------------------------------------------------
library(DyadRatios)
data(jennings)
head(jennings)

## -----------------------------------------------------------------------------
library(dplyr) 
jennings %>% 
  group_by(variable) %>% 
  tally() %>% 
  arrange(desc(n))

## -----------------------------------------------------------------------------
jennings_out <- extract(
  varname = jennings$variable, 
  date = jennings$date, 
  index = jennings$value, 
  ncases = jennings$n, 
  begindt = min(jennings$date), 
  enddt = max(jennings$date), 
  npass=1
)


## -----------------------------------------------------------------------------
print(jennings_out)

## -----------------------------------------------------------------------------
summary(jennings_out)

## -----------------------------------------------------------------------------
plot(jennings_out)

## -----------------------------------------------------------------------------
ests <- get_mood(jennings_out)
ests

