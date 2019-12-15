theme_set(theme_bw())

# Plot cumulative distance per year (>2016) --------------------------------------------

df %>%
  group_by(year) %>%
  arrange(start_date) %>%
  mutate(cumulative_distance = cumsum(distance)) %>%
  ungroup() %>%
  ggplot(aes(x = day_of_year, y = cumulative_distance, color = factor(year))) +
  geom_line() +
  scale_color_brewer(palette = "Set1") +
  gghighlight::gghighlight(year > 2016) +
  labs(title = "Cumulative distance per year",
       subtitle = "Last 3 years highlighted")


# Calendar heatmap ---------------------------------------------------------------------

ggplot(df %>% filter(year > 2011), aes(x = week, y = factor(day))) +
  geom_tile(aes(fill = distance)) +
  scale_fill_continuous(low = "lightgreen", high = "red") +
  facet_wrap(~ year, scales = "free_x") +
  labs(x = "Week", y = "") 


# Relationships ------------------------------------------------------------------------

# distance and speed
df %>%
  ggplot(aes(x = distance, y = average_speed)) + 
  geom_hex() +
  scale_fill_viridis_c()

df %>% 
  ggplot(aes(x = distance, y = average_speed)) +
  geom_point(shape = 21, color = 'forestgreen') +
  geom_smooth(color = 'forestgreen')

# heart rate and speed for running activities
df %>% 
  filter(type == "Run") %>% 
  ggplot(aes(x = average_speed, y = average_heartrate)) +
  geom_point(shape = 21, color = 'forestgreen') +
  geom_smooth(color = 'forestgreen')

# heart rate and elevation for running activities
df %>% 
  filter(type == "Run") %>% 
  ggplot(aes(x = average_speed, y = elev_high)) +
  geom_point(shape = 21, color = 'forestgreen') +
  geom_smooth(color = 'forestgreen')

# heart rate and elevation for running activities
df %>% 
  filter(type == "Run") %>% 
  ggplot(aes(x = average_heartrate, y = elev_high)) +
  geom_point(shape = 21, color = 'forestgreen') +
  geom_smooth(color = 'forestgreen')



# Analysing an individual activity -----------------------------------------------------

get_activity <- function(id) {
  
  types <- list("time", "latlng", "distance", "altitude", "velocity_smooth", "heartrate", "cadence", "watts", "temp", "moving", "grade_smooth")
  activity_raw <- get_streams(stoken, id = id)
  
  activity <- activity_raw %>%
    purrr::transpose() %>% 
    tibble::as_tibble() %>% 
    dplyr::select(type, data) %>% 
    dplyr::mutate(type = unlist(type), 
                  data = purrr::map(data, function(x) {
                    idx <- !sapply(x, length)
                    x[idx] <- NA
                    return(x)
                  }))
  
  lat_lng_to_df <- function(x) {
    purrr::set_names(x, nm = c("lat", "lng")) %>% tibble::as_tibble()
  }
  
  activity <- activity %>% 
    tidyr::spread(data = ., key = type, value = data) %>%
    tidyr::unnest() %>%
    dplyr::mutate(latlng = purrr::map(latlng, lat_lng_to_df)) %>%
    tidyr::unnest() %>%
    mutate(velocity_kph = 3.6 * velocity_smooth,
           climb = altitude - lag(altitude),
           row_numer = row_number())
  
  return(activity)
  
}
#activity <- get_activity(id = "2799967097")

plot_activity_map <- function(id) {

activity <- get_activity(id = id)

leaflet(activity) %>%
  addTiles() %>%
  addPolylines(lng = ~ lng, 
               lat = ~ lat)

}
plot_activity_map(id = "1474730475")

# Using gganimate:
animate_map <- plot_activity_map("2799967097")

animate_map + 
  transition_reveal(row_number)





# Analysing velocity and altitude:

plot_activity_profile <- function(id) {
  
activity <- get_activity(id = id)

activity <- activity %>%
  mutate(distance_km = distance / 1000)

altitude <- activity %>%
  ggplot(aes(x = distance_km, y = altitude)) +
  geom_area(fill = 'forestgreen', alpha = .6) +
  coord_cartesian(ylim = c(0, 2000)) 

speed <- activity %>%
  ggplot(aes(x = distance_km, y = velocity_kph)) +
  geom_line(color = 'slategrey') +
  geom_hline(yintercept = mean(activity$velocity_kph), linetype = "dashed", color = "black") 

p <- altitude + speed + plot_layout(ncol = 1, nrow = 2) 

return(p)
}
plot_activity_profile(id = "1481842769")

# Relationships
activity %>% 
  filter(moving == TRUE) %>% 
  ggplot(aes(x = heartrate, y = velocity_kph, color = climb)) +
  geom_hex()

activity %>% 
  filter(moving == TRUE) %>% 
  ggplot(aes(x = heartrate, y = velocity_kph)) +
  geom_point(shape = 21, color = 'forestgreen') +
  geom_smooth(color = 'forestgreen')

