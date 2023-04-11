# Cleaning Data

library(tidyverse)
library(dplyr)
library(easystats)
library(janitor)


# Facebook only lets you download 3 months at a time for some reason.
  # I want a full year.
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
  # Date and hour ought to be split up too

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

# Now it's less busy and has Anglo-ish columns.

# This selection bugs me for a few reasons.

  # It should be possible to differentiate what is original content or not by
    # what originates from other pages.The identificador_de_pagina (page id) and
    # nombre_de_la_pagina (page name) variables should show this. 
    # For some reasonFacebook doesn't record page of origin in either of these.
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
# Okay should probably change that
# dfyear$date <- as.Date(dfyear$date)
# That's not working out great. Doesn't seem to recognize the mm/dd/yyyy format.
# Figure out how to fix this later

# We need a new variable to show what is or is not OC
  # Need to pick out character strings from the descriptions

dfyear$description %>% 
  unique()

oc <- grep("OC",dfyear$description)
oc2 <- grep("O.c",dfyear$description)
oc3 <- grep("oc", dfyear$description)
oc4 <- grep("O.C",dfyear$description)
oc5 <- grep("\\(O\\)",dfyear$description)
oc6 <- grep("\\(o\\)",dfyear$description)

oc<-append(oc, oc2)
oc<-append(oc, oc3)
oc<-append(oc, oc4)
oc<-append(oc, oc5)

oc <- oc %>% 
  unique()

dfyear$vectorific <- vectorific <- c(1:693)
class(vectorific)
isOc<- vectorific %in% oc 
dfyear %>% 
  filter(.by = vectorific %in% oc)

dfyear$isOc = isOc

# Gonna need to manually input it if we want to show who made which posts.
  # That means manually scrolling the page's Facebook feed and writing who made what.
  # ...For hundreds of posts across a whole year.
  # Why doesn't Meta just record which admin made what in Business Suite?
  # The Zucc does not like to be questioned.







