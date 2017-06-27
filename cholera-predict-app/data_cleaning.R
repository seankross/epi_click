library(readr)
library(dplyr)
library(lubridate)

read_csv("humancast.csv") %>%
  select(-TR) %>%
  mutate(TL = mdy(TL)) %>%
  write_csv("outbreaks.csv")

read_csv("humancast.csv") %>%
  select(TL, cases) %>%
  group_by(TL) %>%
  summarize(avg = mean(cases)) %>%
  mutate(TL = mdy(TL)) %>%
  write_csv("outbreak-avgs.csv")
