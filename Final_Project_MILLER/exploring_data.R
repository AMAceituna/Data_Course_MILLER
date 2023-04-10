# Exploring Data

library(tidyverse)
library(dplyr)
library(easystats)
library(modelr)
library(GGally)

dfyear <- read.csv("./dfyear.csv")

# Let's just take a looksee

dfyear %>%
  select(impressions,engagements, shares) %>% 
  ggpairs() +
  theme(axis.text.x = element_text(angle = 90))
  # That's a little hard to read with so many observations.
  # It looks like there are 3 outliers consistently messing with things.
    # Those probably represent our posts that went really viral.
    # It looks like there is a cluster of 5 or so posts that were also quite
    # successful sitting behind them, but at the front of the pack.

dfyear %>% 
  ggplot(aes(x = impressions, y = engagements)) +
  geom_point() +
  theme_lucid() +
  theme(axis.text.x = element_text(angle = 90))

dfyear %>% 
  ggplot(aes(x = time, y = engagements)) +
  geom_point() +
  theme_lucid() +
  theme(axis.text.x = element_text(angle = 90))

dfyear %>% 
  ggplot(aes(x = date, y = engagements)) +
  geom_point() +
  theme_lucid() +
  theme(axis.text.x = element_text(angle = 90))

dfyear %>% 
  ggplot(aes(y = impressions)) +
  geom_histogram() +
  theme_lucid() +
  theme(axis.text.x = element_text(angle = 90))

# These are all sort of just worse versions of that.
  # Fix OC and date/time variables and come back to this.

