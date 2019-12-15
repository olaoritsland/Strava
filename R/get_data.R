# Config

# Packages
library(rStrava)
library(tidyverse)
library(lubridate)
library(leaflet)
library(gganimate)
library(patchwork)

# Strava API
client_id <- 41646
secret <- "89660e7dad7709b984863bdd81f4d60580477006"
app_name <- "WorkoutAnalytics"

# Get token and get data (this part is commented out to save time, read in RDS instead)
stoken <- httr::config(token = strava_oauth(app_name, client_id, secret, app_scope="activity:read_all"))
#my_acts <- get_activity_list(stoken)

#df <- compile_activities(my_acts)
#saveRDS(df, file = here::here("./data//strava_df.rds"))
df <- readRDS(here::here("./data//strava_df.rds"))
