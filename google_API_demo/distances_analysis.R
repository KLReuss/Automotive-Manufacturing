library(tidyverse)
library(tigris)
library(here)
library(ggsflabel)
library(sf)
library(usmap)
library(tmap)

# Load data
enforcement_dist <- read_csv(here('data',
                                  'filtered_enforcement_dist.csv'))

# Driving distances are difficult in HI and PR so excluded for this analysis
us_states <- states(cb = TRUE, resolution = "20m") %>% 
  filter(!STUSPS %in% c('HI','PR'))

# Plot driving times

avg_time <- enforcement_dist %>% 
  group_by(st_cd) %>% 
  # Times provided in seconds - calculation converts to minutes
  summarize(avg_time = mean(duration_value, na.rm=TRUE)/60) %>%  
  filter(st_cd %in% c(state.abb, 'DC'))

enf_time_shp <- us_states %>% 
  left_join(avg_time, by = c('STUSPS' = 'st_cd'))


enf_time_shifted <- shift_geometry(enf_time_shp)


ggplot() +
  theme_void() +
  geom_sf(data = enf_time_shifted, aes(geometry=geometry, fill= avg_time)) +
  scale_fill_gradient(low = '#ffff99', high = '#b10026', na.value = '#ffffcc' ) +
  geom_sf_label_repel(data = enf_time_shifted, aes(label = round(avg_time, 1)),
                      force = 10, nudge_x = 0, seed = 10) +
  labs(title = "Average Driving Times Between FLSA Violators and the Closest WHD Office",
       fill = "Average Driving Times (minutes)") +
  theme(plot.title = element_text(family = "serif",              # Font family
                                  face = "bold",                    # Font color
                                  size = 20,
                                  hjust = 0.5))

# Plot distances (as crow flies)

avg_dist <- enforcement_dist %>% 
  group_by(st_cd) %>% 
  # Distances provided in meters - calculation converts to miles
  summarize(dist_mi = mean(closest_office_dist, na.rm=TRUE)*0.000621371) %>%  
  filter(st_cd %in% c(state.abb, 'DC'))

enf_dist_shp <- us_states %>% 
  left_join(avg_dist, by = c('STUSPS' = 'st_cd'))


enf_dist_shifted <- shift_geometry(enf_dist_shp)


ggplot() +
  theme_void() +
  geom_sf(data = enf_dist_shifted, aes(geometry=geometry, fill= dist_mi)) +
  scale_fill_gradient(low = '#ffff99', high = '#b10026', na.value = '#ffffcc' ) +
  geom_sf_label_repel(data = enf_dist_shifted, aes(label = round(dist_mi, 1)),
                      force = 10, nudge_x = 0, seed = 10) +
  labs(title = "Average Distance Between FLSA Violators and the Closest WHD Office",
       fill = "Average Distance (miles)") +
  theme(plot.title = element_text(family = "serif",              # Font family
                                  face = "bold",                    # Font color
                                  size = 20,
                                  hjust = 0.5))


# Plot driving distances

avg_d_dist <- enforcement_dist %>% 
  group_by(st_cd) %>% 
  # Distances provided in meters - calculation converts to miles
  summarize(dist_mi = mean(distance_value, na.rm=TRUE)*0.000621371) %>%  
  filter(st_cd %in% c(state.abb, 'DC'))

enf_d_dist_shp <- us_states %>% 
  left_join(avg_d_dist, by = c('STUSPS' = 'st_cd'))


enf_d_dist_shifted <- shift_geometry(enf_d_dist_shp)


ggplot() +
  theme_void() +
  geom_sf(data = enf_d_dist_shifted, aes(geometry=geometry, fill= dist_mi)) +
  scale_fill_gradient(low = '#ffff99', high = '#b10026', na.value = '#ffffcc' ) +
  geom_sf_label_repel(data = enf_d_dist_shifted, aes(label = round(dist_mi, 1)),
                      force = 10, nudge_x = 0, seed = 10) +
  labs(title = "Average Driving Distance Between FLSA Violators and the Closest WHD Office",
       fill = "Average Driving Distance (miles)") +
  theme(plot.title = element_text(family = "serif",              # Font family
                                  face = "bold",                    # Font color
                                  size = 20,
                                  hjust = 0.5))


# plot whd offices vs flsa violations

whd_offices <- read_csv(here('data', 'office_coords.csv'))

enforcement_dist_sf <- enforcement_dist %>% 
  filter(st_cd %in% c(state.abb, 'DC')) %>% 
  st_as_sf(coords = c('lng', 'lat')) %>%
  st_set_crs(4326) %>% 
  shift_geometry()
  

whd_offices_sf <- whd_offices %>% 
  filter(!is.na(Lat),
         !(Office %in% c('Mayaguez FO',
                         'Arecibo FO',
                         'Ponce FO',
                         'Caribbean DO',
                         'Guam AO',
                         'Saipan FO'))) %>% 
  st_as_sf(coords = c('Lon', 'Lat')) %>%
  st_set_crs(4326) %>% 
  shift_geometry()


tm_shape(us_states_shifted %>% 
           filter(!(STUSPS %in% c('PR','HI')))) +
  tm_borders() +
  tm_shape(enforcement_dist_sf) +
  tm_symbols(col = 'black',
             shape = 21,
             size = .1,
             alpha = .05) +
  tm_shape(whd_offices_sf) +
  tm_symbols(size = .2,
             shape = 8,
             col = 'red')


