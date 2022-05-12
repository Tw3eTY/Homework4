#####Problem 1#####
library(tidyquant)
library(tidyverse)
library(dplyr)
library(lubridate)
# 1.Download the stock prices for AMZN, FB, NFLX, stocks from 2019-01-01 
# to 2021-04-01. Keep only the symbol/date/adjusted columns.

data <-tq_get(c("AMZN", "FB", "NFLX"),
              get="stock.prices",
              from="2019-01-01",
              to="2021-04-01") %>%
  select(symbol, date, adjusted)
view(data)

# 2.Add all the missing dates(such as 2019-01-01), so that we have 
# observations for every single date. Fill in the missing values for adjusted 
# with the last non-missing observation.

Dates<- data.frame(Date=seq.Date(from=ymd("2019-01-01"),
                                 to=ymd("2021-04-01"),
                                 by="day"))

View(Dates)

Join<-Dates %>%
  dplyr::left_join(data, by=c("Date"="date"))

View(Join)

data<-tq_get(c("AMZN","FB", "NFLX"),
             get="stock.prices",
             from="2019-01-01",
             to="2021-04-01") %>%
  select(symbol, date, adjusted)

Dates<- data.frame(Date=seq.Date(from=ymd("2019-01-01"),
                                 to=ymd("2021-04-01"),
                                 by="day",3),
                   Symbol=c(rep("AMZN", 822), rep("FB", 822), rep("NFLX", 822)))

Final <- Dates %>%
  left_join(data, by = c("Dates" = "date", "Symbol" = "symbol"))%>%
  group_by(Symbol)%>%
  fill(adjusted, .direction = "downup")


# 3.Create a new data frame, which consist only of stocks from AMZN or FB and 
# has observations from 2019-01-01 to 2019-07-01 or 2020-04-01 to 2020-07-01. 
# Arrange the data frame first by the symbol name and by the date in 
# descending order.

Results <- Final %>% 
  filter((Dates >= ymd("2019-01-01") & Dates <= ymd("2019-07-01")) |
           (Dates >= ymd("2020-04-01") & Dates <= ymd("2020-07-01")),
         Symbol %in% c("AMZN", "FB")) %>%
  arrange(Symbol, desc(Dates))

# 4.Select the first and last observation of the aforementioned dataframe
# for each of the two stocks - AMZN and FB.

Select4 <- Results %>%
  group_by(Symbol)%>%
  slice(c(1, n()))%>%
  ungroup()

# 5.Select the last observation for each stock, for each month. 
# In order to do this, first create a new column, which will show you the 
# year and the month. You can do this using the functions substr() or floor_date.

Select5 <- Final %>%
  mutate(DatesNew = base::substr( Dates , 1, 7)) %>%
  group_by(Symbol, DatesNew) %>%
  slice_tail()%>%
  ungroup()

#####Problem 1#####



#####Problem 2#####
#Use the dataframe from problem 1.2.
# Use the SMA function from the tidyquant package to calculate the 10day SMA 
# and the 26 day SMA for each of the 3 stocks. 
# How many times did the 10 day SMA line cross 26 day SMA line from below? 
# How many times did the 10 day SMA line cross 26 day SMA line from above?
# You can take a look at this article: https://www.investopedia.com/trading/macd/
# Essentially by cross from above/below I want you to find the buy/sell signals.

data<-tq_get(c("AMZN","FB", "NFLX"),
             get="stock.prices",
             from="2019-01-01",
             to="2021-04-01") %>%
  select(symbol, date, adjusted)

Dates<- data.frame(Date=seq.Date(from=ymd("2019-01-01"),
                                 to=ymd("2021-04-01"),
                                 by="day",3),
                   Symbol=c(rep("AMZN", 822), rep("FB", 822), rep("NFLX", 822)))
Join<-Dates %>%
  left_join(data, by=c("Date"="date","Symbol"="symbol")) %>%
  group_by(Symbol) %>%
  fill(adjusted, .direction="downup") %>%
  mutate(SMA10=SMA(adjusted,n=10),
         SMA26=SMA(adjusted, n=26))

#####Problem 2#####