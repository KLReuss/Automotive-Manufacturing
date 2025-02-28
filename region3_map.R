library(tidyverse)
library(ggmap)
library(maps)
# library(mapdata)

library(tidygeocoder)
library(ggrepel)




# has_google_key()

data_path <- here::here("data", "car_plant_data.csv")

plant_data <- read_csv(data_path)


plant_data <- plant_data |> 
  mutate(Status = ifelse(is.na(ActiveAutomakerPresence), "Planned", "Active")) |> 
  mutate(City = ifelse(City == "Springhill", "Spring Hill", City))


region3_states <- c("alabama", "florida", "georgia", "kentucky", "mississippi" ,
                   "north carolina", "south carolina", "tennessee") 


region3 <- map_data("state") |> 
  filter(region %in% (region3_states))


ggplot(data = region3) + 
  geom_polygon(aes(x = long, y = lat, group = group), fill = "palegreen", color = "black") + 
  coord_fixed(1.3) +
  theme_nothing()



plant_geo <- geocode(plant_data, city = City, state = StateCode)




map <- ggplot(data = region3) + 
  geom_polygon(aes(x = long, y = lat, group = group), fill = "gray70", color = "white", size = 1) + 
  coord_fixed(1.3) +
  # theme_nothing() +
  theme_void() +
  geom_point(data = plant_geo, aes(x = long, y = lat, color = Status), size = 5, shape = 16,
             position = position_dodge(width = .6)) +
  geom_label_repel(data = plant_geo, aes(x = long, y = lat, label = paste0(Name, " (", City, ")"), 
                                         fill = Status),size = 5)


paste0(Name, " (", City, ")")



map1 <- get_stamenmap( bbox = c(bottom = 25, left = -92, top = 40, right = -75), zoom = 6, maptype = "watercolor")
map2 <- get_stamenmap( bbox = c(bottom = 25, left = -92, top = 40, right = -75), zoom = 6, maptype = "terrain")
map  <- ggmap(map2) + 
  geom_polygon(data = region3, aes(x = long, y = lat, group = group), fill = NA, color = "gray40", size = 1.3) +
  theme_void() + 
  theme(
    plot.title = element_text(colour = "orange"), 
    panel.border = element_rect(colour = "grey", fill=NA, size=2)) +
  geom_point(data = plant_geo, aes(x = long, y = lat, color = Status), size = 5, shape = 16,
             position = position_dodge(width = .6)) +
  geom_label_repel(data = plant_geo, aes(x = long, y = lat, label = paste0(Name, " (", City, ")")), size = 4)

map +
  scale_color_manual(values=c("deepskyblue1", "indianred1")) +
  scale_fill_manual(values=c("deepskyblue1", "indianred1")) +
  ggtitle("Automotive Activity in Region 3",
          subtitle = "Automakers with Active and Planned Locations") +
  theme(plot.title = element_text(colour = "black")) +
  guides(fill="none")
  


plant_geo_active <- plant_geo |> 
  filter(Status == "Active")

map <- ggplot(data = region3) + 
  geom_polygon(aes(x = long, y = lat, group = group), fill = "gray70", color = "white", size = 1) + 
  coord_fixed(1.3) +
  # theme_nothing() +
  theme_void() +
  geom_point(data = plant_geo_active, aes(x = long, y = lat), size = 5, shape = 16,
             position = position_dodge(width = .6), color = "deepskyblue1") +
  geom_label_repel(data = plant_geo_active, aes(x = long, y = lat, label = paste0(Name, " (", City, ")")),
                   size = 5, fill = "deepskyblue1")
map +
  ggtitle("Automotive Activity in Region 3",
          subtitle = "Automakers with Active Locations") +
  theme(plot.title = element_text(colour = "black")) 


plant_geo_planned <- plant_geo |> 
  filter(Status == "Planned")

map <- ggplot(data = region3) + 
  geom_polygon(aes(x = long, y = lat, group = group), fill = "gray70", color = "white", size = 1) + 
  coord_fixed(1.3) +
  # theme_nothing() +
  theme_void() +
  geom_point(data = plant_geo_planned, aes(x = long, y = lat), size = 5, shape = 16,
             position = position_dodge(width = .6), color = "indianred1") +
  geom_label_repel(data = plant_geo_planned, aes(x = long, y = lat, label = paste0(Name, " (", City, ")")),
                   size = 5, fill = "indianred1")
map +
  ggtitle("Automotive Activity in Region 3",
          subtitle = "Automakers with Planned Locations") +
  theme(plot.title = element_text(colour = "black")) 





install.packages("choroplethrMaps")

library(choroplethr) 
library(choroplethrMaps)
data(df_pop_county)

choro = CountyChoropleth$new(df_pop_county)
choro$title = "2012 Population Estimates"
choro$ggplot_scale = scale_fill_brewer(name="Population", palette=2, drop=FALSE)
choro$render()



gcounty <- map_data("county") |> 
  filter(region %in% (region3_states))











map_all <- left_join(plant_data, city_df, by = c("StateCode", "City"))




city_info <- plant_data |> 
  mutate(City = as.character(City), StateCode = as.character(StateCode)) |> 
  geocode(city = City, state = StateCode, method = "census")


register_google(key = "AIzaSyAkcngiYryr-4PuseCAnEsW0qb7SGDiPdM")



install.packages("tidygeocoder")



plant_cities <- cbind(geocode(as.character(plant_data$City)), plant_data)


city_df <- us.cities %>% 
  rename(StateCode = country.etc) |> 
  mutate(name = word(name, 1, -2)) %>% 
  mutate(name = str_to_title(name)) %>% 
  mutate(group = 1) |> 
  rename(City = name)






