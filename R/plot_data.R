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


# Relationship between distance and speed ----------------------------------------------

df %>%
  ggplot(aes(x = distance, y = average_speed)) + 
  geom_hex() +
  scale_fill_viridis_c()

df %>% 
  ggplot(aes(x = distance, y = average_speed)) +
  geom_point(shape = 21, color = 'forestgreen') +
  geom_smooth(color = 'forestgreen')

# Analysing an individual activity -----------------------------------------------------

plot_activity_map <- function(id) {

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
  mutate(velocity_kph = 3.6 * velocity_smooth)

plot <- leaflet(activity) %>%
  addTiles() %>%
  addPolylines(lng = ~ lng, 
               lat = ~ lat)

return(plot)

}
plot_activity_map(id = "1474730475")

# Using gganimate:





# Analysing velocity and altitude:

plot_activity_profile <- function(id) {

activity <- activity %>%
  mutate(distance_km = distance / 1000)

altitude <- activity %>%
  ggplot(aes(x = distance_km, y = altitude)) +
  geom_area(fill = 'forestgreen', alpha = .6) 
  #coord_cartesian(ylim = c(0, 300)) 

speed <- activity %>%
  ggplot(aes(x = distance_km, y = velocity_kph)) +
  geom_line(color = 'slategrey') +
  geom_hline(yintercept = mean(activity$velocity_kph), linetype = "dashed", color = "black") 

p <- altitude + speed + plot_layout(ncol = 1, nrow = 2) 

return(p)
}


