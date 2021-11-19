# load libraries ----
library(sf)
library(rgdal)
library(geojsonio)
library(tidyverse)
library(osmdata)
library(osmplotr)
library(ggtext)
library(viridis)

# load fonts ----
library(sysfonts)
library(showtext)
font_add_google("Playfair Display")
font_add_google("Roboto Condensed")
showtext_auto()
showtext_opts(dpi = 300)


# Jabar Hex Map ----
# > load datasets ----

## load the hexagonal map file from https://cartogrid.vercel.app/

jabar_hex_sf <- geojson_sf("data_raw/jabar_hex_5x5.geojson")
jabar_hex_json <- readOGR(dsn = "data_raw/jabar_hex_5x5.geojson")

jakarta_triangle <- readOGR(dsn = "data_raw/jakarta_tri_1x1.geojson")
banten_tri <- readOGR(dsn = "data_raw/banten_tri_3x3.geojson")


## load boundary box 
jabar_bb <- get_bbox(c(106.374464 , -7.804251 , 
                       108.842836 , -5.935049))

dki_bb <- get_bbox(c(106.68586 , -6.37267,
                     106.975432, -6.075894)
                   )

banten_bb <- get_bbox(c(105.09985 , -7.01678,
                        106.785209, -5.883636)
)
# > load health facility points from OSM ----

### health facilities include hospital, clinic, doctors, dentist, pharmacy

jabar_health <- get_bbox(c(106.374464 , -7.804251 , 
                           108.842836 , -5.935049)) %>% 
  opq() %>% 
  add_osm_feature(
    key = "amenity",
    value = c("hospital", "clinic", "doctors", "dentist", "pharmacy")
  ) %>% 
  osmdata_sf()

jabar_health_sf <- st_as_sf(
  jabar_health$osm_points)

## load DKI jakarta health facility in order to exclude it from jabar area

dki_health <- get_bbox(c(106.68586 , -6.37267,
                         106.975432, -6.075894)) %>% 
  opq() %>% 
  add_osm_feature(
    key = "amenity",
    value = c("hospital", "clinic", "doctors", "dentist", "pharmacy")
  ) %>% 
  osmdata_sf()

dki_health_sf <- st_as_sf(
  dki_health$osm_points)

dki_osm <- dki_health_sf %>% pull(osm_id)

## goes the same with banten

banten_health <- get_bbox(c(105.09985 , -7.01678,
                            106.785209, -5.883636)) %>% 
  opq() %>% 
  add_osm_feature(
    key = "amenity",
    value = c("hospital", "clinic", "doctors", "dentist", "pharmacy")
  ) %>% 
  osmdata_sf()

banten_health_sf <- st_as_sf(
  banten_health$osm_points)

banten_osm <- banten_health_sf %>% pull(osm_id)

## filter jabar health facilities only

jabar_health_sf_fil <- jabar_health_sf %>% 
  dplyr::filter(!osm_id %in% c(dki_osm, banten_osm))

## join 
jabar_health_join <- jabar_health_sf_fil %>% 
  st_union()

## no sphere
sf::sf_use_s2(FALSE)

## measure distance
jabar_distance <- st_distance(
  jabar_hex_sf, jabar_health_join
) %>% units::set_units("km")

jabar_distance

jabar_hf <- jabar_hex_json %>% 
  st_as_sf() %>% 
  mutate(jarak = jabar_distance) %>% 
  drop_na()

# > plot ----

my_sub <- "Health facilities consisted of hospitals, clinics, doctors, dentists, and pharmacies. These facilities are mainly centralized around Bandung Metropolitan Area and DKI Jakarta's neighbouring cities such as Bekasi, Bogor, and Depok. However, people who live in the western part of West Java may have to struggle more reaching those enlisted health facilities."

jabarhealth_plot_hex <- ggplot() +
  geom_sf(
    data = jabar_hf,
    aes(fill = as.numeric(jarak)),
    color = "NA"
  ) +
  coord_sf(xlim = c(106.374464, 108.842836),
           ylim = c(-7.804251, -5.935049)) +
  scale_fill_gradientn(colours = mako(12),
                       breaks = c(0, 10, 20, 30, 40, 50),
                       labels = c("0km", "10km", "20km", 
                                  "30km", "40km", "50km"),
                       guide = guide_colourbar(
                         title.position = "top",
                         direction = "horizontal",
                         barwidth = 17,
                         barheight = 0.5,
                         frame.colour = "black"
                       )) +
  geom_sf(data = jabar_health_sf_fil, 
          size = 1.5, shape = 21, 
          fill = "#FFC800", alpha=.7, stroke = .5) +
  labs(title = "Distance to Health Facilities in West Java",
       subtitle = str_wrap(my_sub, width = 108),
       fill = "Distance to nearest West Java Health Facilites (orange dots)",
       caption = "Data: OpenStreetMap | Map: R. Ramadhan for Jabar Digital Service ") +
  theme_void() +
  theme(panel.background = element_rect(fill = "#e9e6eb", colour = "#e9e6eb"),
        plot.background = element_rect(fill = "#e9e6eb", colour = "#e9e6eb"),
        plot.title = element_text(hjust = 0, 
                                  family = "Playfair Display",
                                  colour = "#0D47A1",
                                  size = 16,
                                  margin = margin(10, 0, -5, 0)),
        plot.subtitle = element_text(hjust = 0, 
                                     family = "Roboto Condensed",
                                     colour = "#0D47A1",
                                     size = 8,
                                     margin = margin(10, 0, -5, 0)),
        plot.caption = element_text(family = "Roboto Condensed",
                                    size = 4,
                                    colour = "black"),
        legend.direction = "horizontal",
        legend.position = "bottom",
        legend.title = element_text(family = "Roboto Condensed", 
                                    hjust = 0.5,
                                    colour = "black", size = 6),
        legend.text = element_text(colour = "black",
                                   size = 6))

  ggsave(
    jabarhealth_plot_hex,
    filename = "Outfile/202111_health_facilities_hex.png",
    width = 5,
    height = 5,
    dpi = 300
  )
  
# Grid Version ----
  
  ## load the grid file
  jabar_grid <- geojson_sf("data_raw/jabar_square_3x3.geojson")
  jabar_grid_ori <- readOGR(dsn = "data_raw/jabar_square_3x3.geojson")
  
  ## load boundary box 
  jabar_bb <- getbb(
    "West Java",
    featuretype = "state",
    format_out = "sf_polygon")
  
  jabar_bb2 <- getbb(
    "West Java",
    featuretype = "county",
    format_out = "sf_polygon")
  ## load health facility points from OSM
  
  ### health facilities include hospital, clinic, doctors, dentist, pharmacy
  
  jabar_health <- get_bbox(c(106.051, -8.020748, 109.0698, -4.038794)) %>% 
    opq() %>% 
    add_osm_feature(
      key = "amenity",
      value = c("hospital", "clinic", "doctors", "dentist", "pharmacy")
    ) %>% 
    osmdata_sf()
  
  jabar_health_sf <- st_as_sf(
    jabar_health$osm_points) %>% 
    st_union()
  
  jabar_health_plot <- st_as_sf(
    jabar_health$osm_points)
  
  ## no sphere
  sf::sf_use_s2(FALSE)
  
  ## measure distance
  jabar_distance <- st_distance(
    jabar_grid, jabar_health_sf
  ) %>% units::set_units("km")
  
  jabar_distance
  
  jabar_hf <- jabar_grid_ori %>% 
    st_as_sf() %>% 
    mutate(jarak = jabar_distance) %>% 
    drop_na()
  
  # plot
  
  jabarhealth_plot <- ggplot() +
    geom_sf(
      data = jabar_hf,
      aes(fill = as.numeric(jarak)),
      color = "NA",
      alpha = 0.95
    ) +
    coord_sf(xlim = c(st_bbox(jabar_bb)[1], st_bbox(jabar_bb)[3]),
             ylim = c(st_bbox(jabar_bb)[2], st_bbox(jabar_bb)[4])) +
    scale_fill_gradientn(colours = mako(12),
                         breaks = c(0, 10, 20, 30, 40, 50, 60),
                         labels = c("0km", "10km", "20km", "30km", "40km", "50km","60km"),
                         guide = guide_colourbar(
                           title.position = "top",
                           direction = "horizontal",
                           barwidth = 17,
                           barheight = 0.5,
                           frame.colour = "black"
                         )) +
    geom_sf(data = jabar_health_plot, 
            size = 1.5, shape = 21, 
            fill = "gray40", alpha=.7, stroke = .5) +
    labs(title = "Distance to Health Facilities in West Java",
         subtitle = "Health facilities include Hospitals, Clinics, Doctors, Dentists, and Pharmacies",
         fill = "Distance to nearest West Java Health Facilites (white dots)",
         caption = "R. Ramadhan for JDS | source = OSM Data ") +
    theme_void() +
    theme(panel.background = element_rect(fill = "#e9e6eb", colour = "#e9e6eb"),
          plot.background = element_rect(fill = "#e9e6eb", colour = "#e9e6eb"),
          plot.title = element_text(hjust = 0.5, family = "Playfair Display",
                                    colour = "black",
                                    size = 20,
                                    margin = margin(20, 0, -5, 0)),
          plot.caption = element_text(family = "Roboto Condensed",
                                      size = 4,
                                      colour = "black"),
          legend.direction = "horizontal",
          legend.position = "bottom",
          legend.title = element_text(family = "Roboto Condensed", hjust = 0.5,
                                      colour = "black", size = 8),
          legend.text = element_text(colour = "black",
                                     size = 5))
  
  ggsave(
    jabarhealth_plot,
    filename = "outfile/jabar_health_facilities.png",
    width = 8,
    height = 5,
    dpi = 300
  )