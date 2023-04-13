# Cleaning Data

library(tidyverse)
library(dplyr)
library(easystats)
library(janitor)
library(lubridate)
# library(hms)

# Facebook only lets you download 3 months at a time for some reason.
  # I want a full year. More observations is better.
apr <- read.csv("./raw_csvs/Apr-01-2022_Jun-30-2022.csv")
jan <- read.csv("./raw_csvs/Dec-31-2021_Mar-31-2022.csv")
  # hol on a sec
  head(jan)
  # Nevermind. Does not actually include an observation for Dec 31.
jul <- read.csv("./raw_csvs/Jul-01-2022_Sep-30-2022.csv")
oct <- read.csv("./raw_csvs/Oct-01-2022_Dec-30-2022.csv")

head(apr)
glimpse(apr)
# Wow this is going to be annoying
  # Why is my facebook in Spanish again?
  # Soy ehstoopeed

apr <- janitor::clean_names(apr)
jan <- janitor::clean_names(jan)
jul <- janitor::clean_names(jul)
oct <- janitor::clean_names(oct)
  # Got rid of the tildes
  # Es menos ehstoopeed

# Wasted a bunch of time looking up "merge" instead of "append".
dfyear <- rbind(jan, apr, jul, oct)
  # This was much easier than I thought.
  # Practice practice practice.

# Got the full year of data
  # Let's get rid of some of these irrelevant/empty columns
  # Date and time ought to be split up too

dfyear <- dfyear %>% remove_empty_columns()
  # Hmmm this needs a bit more. Easier to pick out what *is* relevant
dfyear %>% colnames()
dfyear <- dfyear %>% select(date_hour=hora_de_publicacion,post_id=identificador_de_la_publicacion,
                            title=titulo, description=descripcion, total_clicks=total_de_clics, 
                            other_clicks=otros_clics, photo_views=visualizaciones_de_fotos,
                            link_clicks=clics_en_el_enlace, three_second_video_plays=reproduccion_de_video_de_3_segundos,
                            comments=comentarios, likes=me_gusta, shares=veces_compartido,
                            engagements=interacciones, reach=personas_alcanzadas,
                            impressions=impresiones, hidden_unique_negative_comments=comentarios_negativos_unicos_de_los_usuarios_ocultar,
                            all_hidden_unique_negative_comments=comentarios_negativos_unicos_de_los_usuarios_ocultar_todo)

# Now it's less busy and has Anglo-ish column names.

# This selection bugs me for a few reasons.

  # It should be possible to differentiate what is original content or not by
    # what originates from other pages. The identificador_de_pagina (page id) and
    # nombre_de_la_pagina (page name) variables should show this. 
    # For some reason Facebook doesn't record page of origin in either of these.
    # Why bother putting in useful data when you could just write our own page name
    # Over and over instead?
    # Luckily we put "OC" (or variations) on our original content post descriptions.

  # I picked what actually had data and seemed like it *could* be relevant.
    # Still probably not going to use half these columns. Might select again later.

  # I also don't know exactly how all these things are calculated by Meta.
    # What is an "impression" and why does it seem like there are values in negative_comments
    # which aren't included in negative_comments_total?

dfyear <- dfyear %>% 
  separate_wider_delim(date_hour, delim = " ", names = c("date", "time"))
  # Time already in 24hr time because fr why does anyone use anything else?

class(dfyear$date)
class(dfyear$time)
  # Needs fixing

# as.Date(dfyear$date)
# as.Date(dfyear$time)
  # This doesn't work because wrong format.

# Use as.POSIXct()

day <- "02/07/2022" %>% 
  as.POSIXct(format="%m/%d/%Y")

# POSIX internaitonal standard
  # R actually good at this, just needs right format
  # Can play with timezones in arguments (tz=gmt+- etc.)

# Tidyverse has lubridate package
lubridate::day(day)
lubridate::week(day)
  # There's also a holidays function for different cultures etc.
weekdays(day)
  # This super useful for checking weekends and such. Remember this

dfyear$date <- dfyear$date %>% 
  as.POSIXct(format="%m/%d/%Y")
  # Awesome.
class(dfyear$date)
  # Okay gives a POSIXct class. I bet R will work with that, but let's make sure.
dfyear$date <- as.Date(dfyear$date)
  # Ahhh, gets ride of the timezone. Less cluttered at least.
class(dfyear$date)

dfyear$time <- dfyear$time %>% 
  as.POSIXct(format="%H:%M")

# dfyear$time <- as_hms(ymd_hms(dfyear$time))
  # This works with library(hms)
  # But it leaves the empty seconds and makes it a weird class

# sprintf("%02d:%02d", hour(dfyear$time), minute(dfyear$time))
  # This works without adding hms package, and it can even get rid of the minutes
  # But it makes it a character
  # Maybe it'll still work

dfyear$time <- strftime(dfyear$time, format="%H:%M")
  # Same as the last one. Maybe can get one of these to work somehow, but I've wasted
  # Too much time on it already.

class(dfyear$time)


# We need a new variable to show what is or is not OC
  # Need to pick out character strings from the descriptions

dfyear$description %>% 
  unique()

oc <- grep("OC",dfyear$description)
oc2 <- grep("O.c",dfyear$description)
  # Should also pick up "O.c."
oc3 <- grep("oc", dfyear$description)
oc4 <- grep("O.C",dfyear$description)
oc5 <- grep("\\(O\\)",dfyear$description)
  # There was a hot minute where a certain admin thought it was funny to write "OC" as
  # dumb stuff like "Myth(O)poeti(C) homies where you at" inside the words with parentheses
    # ...it was me
oc6 <- grep("\\(o\\)",dfyear$description)
  # At least my stupidity knows *some* bounds

oc<-append(oc, oc2)
oc<-append(oc, oc3)
oc<-append(oc, oc4)
oc<-append(oc, oc5)
  # 'taint pretty, but it works.

oc <- oc %>% 
  unique()
  # Caught one of the descriptions with the word "social". Gotta deal with repeats.

dfyear$vectorific <- vectorific <- c(1:693)
class(vectorific)
isOc <- vectorific %in% oc 

# dfyear %>% 
#   filter(.by = vectorific %in% oc)

dfyear$isOc <-  isOc
  # Got it in there. Logical might be a problem, but it's there now.

# Gonna need to manually input it if we want to show who made which posts.
  # That means manually scrolling the page's Facebook feed and writing who made what.
  # ...For 693 posts across a whole year.
  # Why doesn't Meta just record which admin made what in Business Suite?
  # The Zucc does not like to be questioned.

# Look at prophet package for new thing to learn
  # Date and time series
  # Made for facebook data
