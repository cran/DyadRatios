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
  arrange(desc(n)) %>% 
  print(n=40)

## -----------------------------------------------------------------------------
jennings_out <- DyadRatios::extract(
  varname = jennings$variable, 
  date = jennings$date, 
  index = jennings$value, 
  ncases = jennings$n, 
  begindt = lubridate::ymd("1985-01-01", tz="GMT"), 
  enddt = max(jennings$date), 
  npass=1
)

## -----------------------------------------------------------------------------
print(jennings_out)

## -----------------------------------------------------------------------------
summary(jennings_out)

## ----out.width="75%", fig.align="center", fig.height=5, fig.width=8-----------
plot(jennings_out)

## -----------------------------------------------------------------------------
ests <- get_mood(jennings_out)
ests

## -----------------------------------------------------------------------------
data(jennings)
jennings_boot <- boot_dr(
  varname = jennings$variable, 
  date = jennings$date, 
  index = jennings$value, 
  ncases = jennings$n, 
  begindt = lubridate::ymd("1985-01-01"), 
  enddt = max(jennings$date), 
  npass=1, 
  R=1000, 
  parallel=FALSE, 
  pw = TRUE
)

## -----------------------------------------------------------------------------
head(jennings_boot$ci)

## ----out.width="75%", fig.align="center", fig.height=5, fig.width=8-----------
library(ggplot2)
ggplot(jennings_boot$ci, aes(x=period)) + 
  geom_ribbon(aes(ymin=lwr, ymax=upr), alpha=0.2) + 
  geom_line(aes(y=latent1)) + 
  labs(y="Mood (Distrust in Government)", x="Year") + 
  theme_bw()

## -----------------------------------------------------------------------------
jennings_boot$pw %>% 
  filter(p_diff > .95) %>% 
  head()

## ----out.width="75%", fig.align="center", fig.height=8, fig.width=8-----------
library(tidyr)
pwdiff <- jennings_boot$pw %>% 
  mutate(diff_sig = ifelse(p_diff > .95, diff, 0))

ggplot(pwdiff, aes(x=as.factor(p1), y=as.factor(p2), fill=diff_sig)) + 
  geom_tile(color="black") + 
  scale_fill_gradient2(low="#377eb8", mid="white", high="#e41a1c", na.value = "grey90") +
  labs(x="From", y="To", fill = "To - From") + 
  theme_classic() + 
  theme(axis.text.x = element_text(angle=90, vjust=0.5, hjust=1), 
        legend.position = "inside", 
        legend.position.inside = c(.85, .33))



## -----------------------------------------------------------------------------
jennings_boot$pw %>% 
  filter(p_diff > .95 & p2 - p1 == 1) %>% 
  arrange(diff)

