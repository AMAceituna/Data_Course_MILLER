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
# Eh.

dfyear %>% 
  ggplot(aes(x = log10(impressions),color = isOc)) +
  geom_histogram() +
  theme_lucid() +
  theme(axis.text.x = element_text(angle = 0)) 
# Oh this actually gud

dfyear %>% 
  group_by(isOc) %>% 
  summarize(count=n(), meanI = mean(impressions), sd(impressions))

dfyear %>% 
  group_by(isOc) %>% 
  summarize(count=n(), meanS = mean(shares), sd(shares))

lratio <- dfyear %>%
  mutate(likeratio = likes/impressions)

# Looking around at some stuff

