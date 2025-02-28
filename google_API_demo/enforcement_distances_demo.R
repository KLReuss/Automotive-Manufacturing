library(tidydol)
library(tigris)
library(ggplot2)
library(sf)
library(dplyr)
library(googleway)
library(purrr)
library(readr)
library(here)
library(crsuggest)
library(spatialrisk)

# Return WHD enforcement data for violators with at least one flsa violation count
# during the year of 2019

# To use this you must first install the tidydol package and receive a DOL API key - contact bottomley.daniel.e@dol.gov for
# more information on how to download and use tidydol
whd_enforcement <- get_dol(agency_name = "whd",
                           dataset_name = "wh_enforcement",
                           limit = -1,
                           filters = filter_logic("and",
                                                  filter_object(field = "findings_start_date",
                                                                operator = "gt",
                                                                value = "2019-01-01"),
                                                  filter_object(field = "findings_start_date",
                                                                operator = "lt",
                                                                value = "2020-01-01"),
                                                  filter_object(field = "flsa_violtn_cnt",
                                                                operator = "gt",
                                                                value = '0')))
              
              
              

# Format address for geocoding              
whd_enforcement <- whd_enforcement %>% 
  mutate(address = paste(street_addr_1_txt, cty_nm, st_cd, zip_cd, sep = ", "))


# Construct geocoding function and assign default NA value for errors
# To run this code you must first register for a google developer account and receive 
# an API key. You will also need to set your API key in the googleway package.
# See here for more information https://developers.google.com/maps
geocoder <- function(address) {
  
  r = google_geocode(address =  address,
                              simplify = TRUE)
  
  return(geocode_coordinates(r) %>% 
           mutate(address = address))

}

geocode_err <- data.frame(lat = NA, lng = NA, address = NA)

poss_geocoder <- possibly(geocoder, otherwise = geocode_err)

# geogcode address, deduplicate, and merge with enforcement data
lat_longs <- map_df(whd_enforcement$address, ~poss_geocoder(.x))

lat_longs_dedup <- lat_longs %>%
  filter(duplicated(address) == FALSE)

geocoded_whd_enforcement <- whd_enforcement %>% 
  left_join(lat_longs_dedup)

# Run only if geocoded data is not saved locally. This will prevent you from 
# having to redo expensive and time consuming API queries
write_csv(geocoded_whd_enforcement, here('data','geocoded_whd_enforcement.csv'))

geocoded_whd_enforcement <- read_csv(here('data','geocoded_whd_enforcement.csv'))

office_coords <- read_csv(here('data','office_coords.csv'))


# Calculate the closest enforcement office to each FLSA violator
closest_office <- function(address, lon_center, lat_center) {
  
  r <- spatialrisk::points_in_circle(office_coords,
                                     lon_center = lon_center,
                                     lat_center = lat_center,
                                     lon = Lon, 
                                     lat = Lat, 
                                     radius = 1e1000)[1,] %>% 
    mutate(address = address)
  
  return(r)
  
}


closest_offices_enf <- pmap_dfr(list(geocoded_whd_enforcement$address, geocoded_whd_enforcement$lng,  geocoded_whd_enforcement$lat), ~closest_office(address = ..1, lon_center = ..2, lat_center= ..3))

closest_offices_enf <- closest_offices_enf %>% 
  rename(closest_district_office = District_Office,
         closest_office_lat = Lat,
         closest_office_lon = Lon,
         closest_office_name = Office,
         closest_office_dist = distance_m)

# Merge closest WHD office with geocoded enforcement data
geocoded_whd_enforcement <- geocoded_whd_enforcement %>% 
  bind_cols(closest_offices_enf)

# Remove duplicate columns
geocoded_whd_enforcement <- geocoded_whd_enforcement %>% 
  select(-address...119) %>% 
  rename(address = address...111)

# Save result locally
write_csv(geocoded_whd_enforcement, here('data','whd_enforcement_offices.csv'))

# Run code below if you would like to start with the whd enforcement data with the closest office
# whd_enforcement_offices <- read_csv(here('data','whd_enforcement_offices.csv'))

# Build function for iterating through google_distance API calls

get_dist <- function(orgin_lat, origin_lng, dest_lat, dest_lng) {

  res <- google_distance(origins = c(orgin_lat, origin_lng),
                         destinations = c(dest_lat, dest_lng),
                         mode = 'driving',
                         units = 'imperial')

  # Returns response elements that we want for our analysis
  res <- res$rows$elements[[1]] %>%
    mutate(origin_addr = res$origin_addresses,
           dest_addr = res$destination_addresses)

  return(res)
}

# set up arguments as a list to pass into a pmap function
dist_args = list(whd_enforcement_offices$lat, whd_enforcement_offices$lng, whd_enforcement_offices$closest_office_lat, whd_enforcement_offices$closest_office_lon)

# This is the function that iterates through google distance api calls and returns the results in a dataframe
# Note: Google charges per AP call. Ensure that you have enough credit or funds in your account before running. This 
# will also take a long time to complete (>1hr)
enforcement_w_distance <- pmap_df(dist_args, ~get_dist(orgin_lat=..1, origin_lng = ..2, dest_lat = ..3, dest_lng=..4))

# Merge the google distance api into our main data set and reshape the data
enforcement_distances <- bind_cols(whd_enforcement_offices,enforcement_w_distance)

enforcement_distances <- enforcement_distances %>% 
  mutate(duration_value = duration$value,
         duration_text = duration$text,
         distance_value = distance$value,
         distance_text = distance$text) %>% 
  select(-c(duration, distance))

# Save the unfiltered data locally
write_csv(enforcement_distances, here('data','enforcement_distances.csv'))

# Filter out all records that did not return a valid driving distance. This
# filters out roughly 282 or 4% of records
filtered_enforcement_dist <- enforcement_distances %>% 
  filter(status == 'OK')

# Save the file locally so additional analysis can be conducted without re-running expensive
# or time consuming processes
write_csv(filtered_enforcement_dist, here('data','filtered_enforcement_dist.csv'))


