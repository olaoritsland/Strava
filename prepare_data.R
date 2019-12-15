
# Define class for specific variables
character_vars <- c("athlete.id", 
                    "external_id", 
                    "map.id", 
                    "map.summary_polyline", 
                    "upload_id_str")

numeric_vars <- c("average_cadence", 
                  "average_heartrate", 
                  "average_temp",
                  "max_heartrate")

df <- df %>%
  
  # date formatting
  mutate(start_date = as_date(start_date),
         year = year(start_date),
         day_of_year = yday(start_date),
         month = month(start_date),
         day = wday(start_date, label = TRUE),
         week = week(start_date)) %>% 
  
  # character to factor 
  mutate_if(is.character, as.factor) %>% 
  mutate_if(.predicate = names(df) %in% character_vars, as.character) %>% 
  mutate_if(.predicate = names(df) %in% numeric_vars, as.numeric)
  