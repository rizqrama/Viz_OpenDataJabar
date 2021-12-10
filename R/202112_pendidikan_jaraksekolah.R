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
font_add_google("Fira Mono")
fontrb <- "Roboto Condensed"
showtext_auto()
showtext_opts(dpi = 300)

# load datasets ----
apm_raw <- read_delim("data_raw/pendidikan/apm_jabar.csv", delim = ";")

# wrangling and exploration ----
apm_df <- apm_raw %>% 
  pivot_longer(-1, names_to = "var_1", values_to = "nilai_apm") %>% 
  mutate(
    tingkat = str_sub(var_1, end = -6),
    tahun = as.integer(str_sub(var_1, -4))) %>% 
  select(1,-2,4,5,3) %>% 
  janitor::clean_names()

## perkembangan nilai APM seluruh kota/kab
apm_df %>%
  dplyr::filter(tingkat == "SMA") %>%
  ggplot(aes(tahun, nilai_apm)) +
  geom_line(color = "magenta", size = 1.2) +
  facet_wrap(~wilayah, nrow = 5) +
  theme_minimal()+
  labs(
    title = "Nilai APM SMA Kabupaten dan Kota di Jawa Barat",
    x = "tahun",
    y = "nilai APM"
  ) +
  scale_x_continuous(
    breaks = c(2017, 2018, 2019)
  )

## Peringkat APM SMA kota/kabupaten 
apm_sma <- apm_df %>% 
  dplyr::filter(tingkat == "SMA" &
                  tahun == 2019 &
                  wilayah != "Provinsi Jawa Barat") %>% 
  arrange(nilai_apm) %>% 
  mutate(median_apm = median(nilai_apm),
         mean_apm = mean(nilai_apm)) %>% 
  left_join(
    apm_df %>% 
      dplyr::filter(wilayah == "Provinsi Jawa Barat" & 
                      tingkat == "SMA") %>% select(3:4),
    by = "tahun"
  ) %>% 
  rename(
    nilai_apm = nilai_apm.x,
    apm_jabar = nilai_apm.y)


## Peringkat APM SMP kota/kabupaten 
apm_df %>% 
  dplyr::filter(tingkat == "SMP" & tahun == 2019 & wilayah != "Provinsi Jawa Barat") %>% 
  arrange(nilai_apm) %>% 
  mutate(median_apm = median(nilai_apm),
         mean_apm = mean(nilai_apm)) %>% 
  View()

# load map files ----

## geojson grid map
## 1. Sumedang
json_sumedang <- readOGR(dsn = "data_raw/peta/grid500m_sumedang.geojson")
sf_sumedang <- geojson_sf("data_raw/peta/grid500m_sumedang.geojson")
xlim_sumedang <- c(107.742539, 108.218271)
ylim_sumedang <- c(-7.039497,  -6.580843)

## 2. Cianjur
json_cianjur <- readOGR(dsn = "data_raw/peta/grid500m_cianjur.geojson")
sf_cianjur <- geojson_sf("data_raw/peta/grid500m_cianjur.geojson")
xlim_cianjur <- c(106.776679, 107.484211)
ylim_cianjur <- c(-7.503495,  -6.604175)

## 3. Bandung Raya
json_bandungraya <- readOGR(dsn = "data_raw/peta/grid500m_bandungraya.geojson")
sf_bandungraya <- geojson_sf("data_raw/peta/grid500m_bandungraya.geojson")
xlim_bandungraya <- c(107.182825, 107.930855)
ylim_bandungraya <- c(-7.314834,  -6.689806)


## 4. Sukabumi
json_sukabumi <- readOGR(dsn = "data_raw/peta/grid500m_sukabumi.geojson")
sf_sukabumi <- geojson_sf("data_raw/peta/grid500m_sukabumi.geojson")
xlim_sukabumi <- c(106.373121, 107.062409)
ylim_sukabumi <- c(-7.437913,  -6.718457)

## 5. ciamis
json_ciamis <- readOGR(dsn = "data_raw/peta/grid500m_ciamis.geojson")
sf_ciamis <- geojson_sf("data_raw/peta/grid500m_ciamis.geojson")
xlim_ciamis <- c(108.176508, 108.720849)
ylim_ciamis <- c(-7.573488,  -7.051882)

## 6. garut
json_garut <- readOGR(dsn = "data_raw/peta/grid500m_garut.geojson")
sf_garut <- geojson_sf("data_raw/peta/grid500m_garut.geojson")
xlim_garut <- c(107.421926 , 108.134384)
ylim_garut <- c(-7.738626  , -6.947224)

## 7. subang
json_subang <- readOGR(dsn = "data_raw/peta/grid500m_subang.geojson")
sf_subang <- geojson_sf("data_raw/peta/grid500m_subang.geojson")
xlim_subang <- c(107.526738, 107.929782)
ylim_subang <- c(-6.812732,  -6.183208)

## 8. bogor
json_bogor <- readOGR(dsn = "data_raw/peta/grid500m_bogor.geojson")
sf_bogor <- geojson_sf("data_raw/peta/grid500m_bogor.geojson")
xlim_bogor <- c(106.402015, 107.226175)
ylim_bogor <- c(-6.788201 , -6.302569)

## 9. indramayu
json_indramayu <- readOGR(dsn = "data_raw/peta/grid500m_indramayu.geojson")
sf_indramayu <- geojson_sf("data_raw/peta/grid500m_indramayu.geojson")
xlim_indramayu <- c(107.85087, 108.53902)
ylim_indramayu <- c(-6.67672, -6.22706)

# load shp
## SMA
sma_shp <- read_sf("data_raw/peta/SEBARAN_PENDIDIKAN_PT/PENDIDIKAN_SHP/Sebaran_SMA.shp")

sma_shp <- sma_shp %>% 
  mutate(
    lat = unlist(map(sma_shp$geometry, 1)),
    long = unlist(map(sma_shp$geometry, 2))
  )

# summarise jumlah sma

jumlah_sma <- sma_shp %>% 
  count(KABKOT, name = "n_sma") %>% 
  as.data.frame() %>% 
  select(-3) %>% 
  rename(wilayah = KABKOT)

## SMP
smp_shp <- read_sf("data_raw/peta/SEBARAN_PENDIDIKAN_PT/PENDIDIKAN_SHP/Sebaran_SMP.shp")

smp_shp <- smp_shp %>% 
  mutate(
    lat = unlist(map(smp_shp$geometry, 1)),
    long = unlist(map(smp_shp$geometry, 2))
  )


# plot apm and number of schools ----

apm_sma <- apm_sma %>% 
  left_join(jumlah_sma, by = "wilayah") 

apm_sma <- apm_sma %>% 
  mutate(wilayah = factor(wilayah, levels = unique(wilayah)))

apm_sma_plot <- 
  apm_sma %>% 
  ggplot(
    aes(x = wilayah, y = nilai_apm, color = wilayah)
  ) +
  geom_point( # add points to show districts' APM value
    aes(size = n_sma) # the size of point represents number of schools
  ) +
  geom_segment( # add lines from points to Jabar APM value
    aes(
      xend = wilayah,
      yend = apm_jabar
    )
  ) +
  geom_hline( # add line to index jabar APM value
    aes(yintercept = apm_jabar),
    color = "grey72",
    linetype = "longdash",
    size = .5) +
  geom_label(
    aes(
      "Kota Depok", # horizontal position of labels
      57.2, # vertical position of labels
      label = glue::glue("APM Jawa Barat: {nilai_apm_jabar}%")
    ),
    color = "grey42",
    fill = NA,
    family = "Roboto Condensed",
    hjust = 1,
    size =3,
    label.padding = unit(.2, "lines"),
    label.r = unit(.25, "lines"), # radius of the rounder corners.
    label.size = .5
  ) +
  scale_y_continuous(
    expand = c(.03, .03),
    breaks = seq(45, 75, by = 5)
  ) +
  scale_color_paletteer_d(
    "khroma::smooth_rainbow",
    direction = -1,
    dynamic = FALSE,
    guide = "none"
  ) +
  scale_size_binned(name = "Jumlah Sekolah", range = c(.3, 3)) +
  labs(
    title = "Angka Partisipasi Murni Tingkat SMA/SMK di Jawa Barat Tahun 2019",
    subtitle = "Angka Partisipasi Murni (APM) tingkat SMA/SMK adalah proporsi anak sekolah di suatu daerah yang bersekolah pada tingkat menengah sesuai dengan kelompok usia 16 hingga 18 tahun",
    x = NULL, 
    y = "Nilai APM (%)",
    caption = "Data: Open Data Jawa Barat & BPS | Viz: R. Ramadhan for JDS"
  ) +
  guides(
    size = guide_bins(
      show.limits = TRUE,
      direction = "horizontal",
      title.position = "top",
      title.hjust = .5)
  )+
  theme(
    plot.background = element_rect(fill = "#fdf9f5", colour = "#fdf9f5"),
    panel.background = element_rect(fill = NA, color = NA),
    panel.border = element_rect(fill = NA, color = NA),
    panel.grid.major.y  = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text.x = element_text(
      family = "Fira Mono",
      colour = "#090740",
      size = 10
    ),
    axis.text.y = element_text(
      family = "Fira Mono",
      colour = "#090740",
      size = 10      
    ),
    axis.ticks = element_blank(),
    axis.title.x = element_text(
      family = "Roboto Condensed",
      size = 12, 
      margin = margin(5, 0, 5, 0)),
    axis.title.y = element_text(
      family = "Roboto Condensed",
      size = 12,
      margin = margin(5, 0, 5, 0)),
    plot.title = element_text(
      hjust = 0, 
      family = "Playfair Display",
      colour = "#090740",
      size = 20,
      margin = margin(5, 0, 5, 0)),
    plot.subtitle = element_text(
      hjust = 0.0, family = "Roboto Condensed",
      colour = "#090740",
      size = 10,
      margin = margin(3, 0, 5, 0)),
    plot.caption = element_text(
      family = "Roboto Condensed",
      size = 10,
      color = "grey70",
      hjust = .5,
      margin = margin(5, 0, 10, 0)
    ),
    legend.title = element_text(
      family = "Fira Mono",
      color = "#090740",
      size = 8),
    legend.text = element_text(
      family = "Fira Mono",
      color = "grey20",
      size = 6),
    legend.justification = c(1,0), 
    legend.position = c(1,0), 
    legend.key = element_rect(
      fill = NA, colour = NA),
    legend.key.width = unit(2, "lines"),
    legend.background = element_blank(),
    plot.margin = margin(10, 25, 10, 25)
  ) +
  coord_flip()
  
ggsave(
  apm_sma_plot,
  filename = "Outfile/202112_apmsma_bar.png",
  width = 13,
  height = 8,
  dpi = 300
)  


# wrangle and plot map files ----
## we are going to measure the distance of SHS in each district

# > 1. sumedang ----
sumedang_sma <- sma_shp %>% 
  dplyr::filter(KABKOT == "Sumedang")

sumedang_sma_join <- sma_shp %>% 
  dplyr::filter(KABKOT == "Sumedang") %>% 
  st_union()

sumedang_distance <- st_distance(
  sf_sumedang,
  sumedang_sma_join
) %>% 
  units::set_units("km")

sumedang_sma_dist <- json_sumedang %>% 
  st_as_sf() %>% 
  mutate(jarak = sumedang_distance) %>% 
  drop_na()

sumedang_sma_plot <- 
  ggplot()+
  geom_sf(
    data = sumedang_sma_dist,
    aes(fill = as.numeric(jarak)),
    color = "NA"
  ) +
  coord_sf(
    xlim = xlim_sumedang,
    ylim = ylim_sumedang
  ) +
  scale_fill_gradientn(
    colours = rocket(10),
    guide = guide_colourbar(
      title.position = "top",
      direction = "horizontal",
      barwidth = 15,
      barheight = 0.5,
      frame.colour = "black"
    )
  ) +
  geom_sf(
    data = sumedang_sma,
    size = 1.5, shape = 21, 
    fill = "#FFC800", alpha=.7, stroke = .5
  )+
  labs(title = "Distance to Nearest Senior High Schools in Sumedang",
       subtitle = "APM Index: 44.20%",
       fill = "Distance (km)",
       caption = "Data: Jawa Barat Education Office | Map: R. Ramadhan for JDS") +
  theme_void() +
  theme(panel.background = element_rect(fill = "#fdf9f5", colour = "#fdf9f5"),
        plot.background = element_rect(fill = "#fdf9f5", colour = "#fdf9f5"),
        plot.title = element_text(hjust = 0.5, family = "Playfair Display",
                                  colour = "#090740",
                                  size = 14,
                                  margin = margin(10, 0, -5, 0)),
        plot.subtitle = element_text(hjust = 0.5, family = "Roboto Condensed",
                                  colour = "#090740",
                                  size = 10,
                                  margin = margin(8, 0, -5, 0)),
        plot.caption = element_text(family = "Roboto Condensed",
                                    size = 4,
                                    hjust = .5,
                                    colour = "#090740"),
        legend.direction = "horizontal",
        legend.position = "bottom",
        legend.title = element_text(family = "Roboto Condensed", hjust = 0.5,
                                    colour = "black", size = 8),
        legend.text = element_text(colour = "black",
                                   size = 5))

ggsave(
  sumedang_sma_plot,
  filename = "Outfile/202112_jaraksma_1sumedang.png",
  width = 6,
  height = 5,
  dpi = 300
)

# > 2. cianjur ----
cianjur_sma <- sma_shp %>% 
  dplyr::filter(KABKOT == "Cianjur")

cianjur_sma_join <- sma_shp %>% 
  dplyr::filter(KABKOT == "Cianjur") %>% 
  st_union()

cianjur_distance <- st_distance(
  sf_cianjur,
  cianjur_sma_join
) %>% 
  units::set_units("km")

cianjur_sma_dist <- json_cianjur %>% 
  st_as_sf() %>% 
  mutate(jarak = cianjur_distance) %>% 
  drop_na()

cianjur_sma_plot <- 
  ggplot()+
  geom_sf(
    data = cianjur_sma_dist,
    aes(fill = as.numeric(jarak)),
    color = "NA"
  ) +
  coord_sf(
    xlim = xlim_cianjur,
    ylim = ylim_cianjur
  ) +
  scale_fill_gradientn(
    colours = rocket(10),
    guide = guide_colourbar(
      title.position = "top",
      direction = "horizontal",
      barwidth = 15,
      barheight = 0.5,
      frame.colour = "black"
    )
  ) +
  geom_sf(
    data = cianjur_sma,
    size = 1.5, shape = 21, 
    fill = "#FFC800", alpha=.7, stroke = .5
  )+
  labs(title = "Distance to Nearest Senior High Schools in Cianjur",
       subtitle = "APM Index: 44.55%",
       fill = "Distance (km)",
       caption = "Data: Jawa Barat Education Office | Map: R. Ramadhan for JDS") +
  theme_void() +
  theme(panel.background = element_rect(fill = "#fdf9f5", colour = "#fdf9f5"),
        plot.background = element_rect(fill = "#fdf9f5", colour = "#fdf9f5"),
        plot.title = element_text(hjust = 0.5, family = "Playfair Display",
                                  colour = "#090740",
                                  size = 14,
                                  margin = margin(10, 0, -5, 0)),
        plot.subtitle = element_text(hjust = 0.5, family = "Roboto Condensed",
                                     colour = "#090740",
                                     size = 10,
                                     margin = margin(8, 0, -5, 0)),
        plot.caption = element_text(family = "Roboto Condensed",
                                    size = 4,
                                    hjust = .5,
                                    colour = "black"),
        legend.direction = "horizontal",
        legend.position = "bottom",
        legend.title = element_text(family = "Roboto Condensed", hjust = 0.5,
                                    colour = "black", size = 8),
        legend.text = element_text(colour = "black",
                                   size = 5))

ggsave(
  cianjur_sma_plot,
  filename = "Outfile/202112_jaraksma_2cianjur.png",
  width = 6,
  height = 5,
  dpi = 300
)

# > 3. Bandung Raya ----
bandung_sma <- sma_shp %>% 
  dplyr::filter(KABKOT %in% c("Kota Bandung", "Bandung", "Bandung Barat", "Kota Cimahi"))

bandung_sma_join <- bandung_sma %>% 
  st_union()

bandung_distance <- st_distance(
  sf_bandungraya,
  bandung_sma_join
) %>% 
  units::set_units("km")

bandung_sma_dist <- json_bandungraya %>% 
  st_as_sf() %>% 
  mutate(jarak = bandung_distance) %>% 
  drop_na()

bandung_sma_plot <- 
  ggplot()+
  geom_sf(
    data = bandung_sma_dist,
    aes(fill = as.numeric(jarak)),
    color = "NA"
  ) +
  coord_sf(
    xlim = xlim_bandungraya,
    ylim = ylim_bandungraya
  ) +
  scale_fill_gradientn(
    colours = rocket(10),
    guide = guide_colourbar(
      title.position = "top",
      direction = "horizontal",
      barwidth = 15,
      barheight = 0.5,
      frame.colour = "black"
    )
  ) +
  geom_sf(
    data = bandung_sma,
    size = 1.5, shape = 21, 
    fill = "#FFC800", alpha=.7, stroke = .5
  )+
  labs(title = "Distance to Nearest Senior High Schools in Bandung Region",
       subtitle = "Bandung Barat: 44.55% | Bandung: 52.75% | Kota Bandung* 68%",
       fill = "Distance (km)",
       caption = "Data: Jawa Barat Education Office | Map: R. Ramadhan for JDS") +
  theme_void() +
  theme(panel.background = element_rect(fill = "#fdf9f5", colour = "#fdf9f5"),
        plot.background = element_rect(fill = "#fdf9f5", colour = "#fdf9f5"),
        plot.title = element_text(hjust = 0.5, family = "Playfair Display",
                                  colour = "#090740",
                                  size = 14,
                                  margin = margin(10, 0, -5, 0)),
        plot.subtitle = element_text(hjust = 0.5, family = "Roboto Condensed",
                                     colour = "#090740",
                                     size = 10,
                                     margin = margin(8, 0, -5, 0)),
        plot.caption = element_text(family = "Roboto Condensed",
                                    size = 4,
                                    hjust = .5,
                                    colour = "black"),
        legend.direction = "horizontal",
        legend.position = "bottom",
        legend.title = element_text(family = "Roboto Condensed", hjust = 0.5,
                                    colour = "black", size = 8),
        legend.text = element_text(colour = "black",
                                   size = 5))

ggsave(
  bandung_sma_plot,
  filename = "Outfile/202112_jaraksma_3bandung.png",
  width = 6,
  height = 5,
  dpi = 300
)

# > 4. sukabumi ----
sukabumi_sma <- sma_shp %>% 
  dplyr::filter(KABKOT == "Sukabumi")

sukabumi_sma_join <- sukabumi_sma %>% 
  st_union()

sukabumi_distance <- st_distance(
  sf_sukabumi,
  sukabumi_sma_join
) %>% 
  units::set_units("km")

sukabumi_sma_dist <- json_sukabumi %>% 
  st_as_sf() %>% 
  mutate(jarak = sukabumi_distance) %>% 
  drop_na()

sukabumi_sma_plot <- 
  ggplot()+
  geom_sf(
    data = sukabumi_sma_dist,
    aes(fill = as.numeric(jarak)),
    color = "NA"
  ) +
  coord_sf(
    xlim = xlim_sukabumi,
    ylim = ylim_sukabumi
  ) +
  scale_fill_gradientn(
    colours = rocket(10),
    guide = guide_colourbar(
      title.position = "top",
      direction = "horizontal",
      barwidth = 15,
      barheight = 0.5,
      frame.colour = "black"
    )
  ) +
  geom_sf(
    data = sukabumi_sma,
    size = 1.5, shape = 21, 
    fill = "#FFC800", alpha=.7, stroke = .5
  )+
  labs(title = "Distance to Nearest Senior High Schools in Sukabumi",
       subtitle = "APM Index: 50.65%",
       fill = "Distance (km)",
       caption = "Data: Jawa Barat Education Office | Map: R. Ramadhan for JDS") +
  theme_void() +
  theme(panel.background = element_rect(fill = "#fdf9f5", colour = "#fdf9f5"),
        plot.background = element_rect(fill = "#fdf9f5", colour = "#fdf9f5"),
        plot.title = element_text(hjust = 0.5, family = "Playfair Display",
                                  colour = "#090740",
                                  size = 14,
                                  margin = margin(10, 0, -5, 0)),
        plot.subtitle = element_text(hjust = 0.5, family = "Roboto Condensed",
                                     colour = "#090740",
                                     size = 10,
                                     margin = margin(8, 0, -5, 0)),
        plot.caption = element_text(family = "Roboto Condensed",
                                    size = 4,
                                    hjust = .5,
                                    colour = "black"),
        legend.direction = "horizontal",
        legend.position = "bottom",
        legend.title = element_text(family = "Roboto Condensed", hjust = 0.5,
                                    colour = "black", size = 8),
        legend.text = element_text(colour = "black",
                                   size = 5))

ggsave(
  sukabumi_sma_plot,
  filename = "Outfile/202112_jaraksma_4sukabumi.png",
  width = 6,
  height = 5,
  dpi = 300
)

# > 5. ciamis ----
ciamis_sma <- sma_shp %>% 
  dplyr::filter(KABKOT == "Ciamis")

ciamis_sma_join <- ciamis_sma %>% 
  st_union()

ciamis_distance <- st_distance(
  sf_ciamis,
  ciamis_sma_join
) %>% 
  units::set_units("km")

ciamis_sma_dist <- json_ciamis %>% 
  st_as_sf() %>% 
  mutate(jarak = ciamis_distance) %>% 
  drop_na()

ciamis_sma_plot <- 
  ggplot()+
  geom_sf(
    data = ciamis_sma_dist,
    aes(fill = as.numeric(jarak)),
    color = "NA"
  ) +
  coord_sf(
    xlim = xlim_ciamis,
    ylim = ylim_ciamis
  ) +
  scale_fill_gradientn(
    colours = rocket(10),
    guide = guide_colourbar(
      title.position = "top",
      direction = "horizontal",
      barwidth = 15,
      barheight = 0.5,
      frame.colour = "black"
    )
  ) +
  geom_sf(
    data = ciamis_sma,
    size = 1.5, shape = 21, 
    fill = "#FFC800", alpha=.7, stroke = .5
  )+
  labs(title = "Distance to Nearest Senior High Schools in Ciamis",
       subtitle = "APM Index: 50.68%",
       fill = "Distance (km)",
       caption = "Data: Jawa Barat Education Office | Map: R. Ramadhan for JDS") +
  theme_void() +
  theme(panel.background = element_rect(fill = "#fdf9f5", colour = "#fdf9f5"),
        plot.background = element_rect(fill = "#fdf9f5", colour = "#fdf9f5"),
        plot.title = element_text(hjust = 0.5, family = "Playfair Display",
                                  colour = "#090740",
                                  size = 14,
                                  margin = margin(10, 0, -5, 0)),
        plot.subtitle = element_text(hjust = 0.5, family = "Roboto Condensed",
                                     colour = "#090740",
                                     size = 10,
                                     margin = margin(8, 0, -5, 0)),
        plot.caption = element_text(family = "Roboto Condensed",
                                    size = 4,
                                    hjust = .5,
                                    colour = "black"),
        legend.direction = "horizontal",
        legend.position = "bottom",
        legend.title = element_text(family = "Roboto Condensed", hjust = 0.5,
                                    colour = "black", size = 8),
        legend.text = element_text(colour = "black",
                                   size = 5))

ggsave(
  ciamis_sma_plot,
  filename = "Outfile/202112_jaraksma_5ciamis.png",
  width = 6,
  height = 5,
  dpi = 300
)

# > 6. garut ----
garut_sma <- sma_shp %>% 
  dplyr::filter(KABKOT == "Garut")

garut_sma_join <- garut_sma %>% 
  st_union()

garut_distance <- st_distance(
  sf_garut,
  garut_sma_join
) %>% 
  units::set_units("km")

garut_sma_dist <- json_garut %>% 
  st_as_sf() %>% 
  mutate(jarak = garut_distance) %>% 
  drop_na()

garut_sma_plot <- 
  ggplot()+
  geom_sf(
    data = garut_sma_dist,
    aes(fill = as.numeric(jarak)),
    color = "NA"
  ) +
  coord_sf(
    xlim = xlim_garut,
    ylim = ylim_garut
  ) +
  scale_fill_gradientn(
    colours = rocket(10),
    guide = guide_colourbar(
      title.position = "top",
      direction = "horizontal",
      barwidth = 15,
      barheight = 0.5,
      frame.colour = "black"
    )
  ) +
  geom_sf(
    data = garut_sma,
    size = 1.5, shape = 21, 
    fill = "#FFC800", alpha=.7, stroke = .5
  )+
  labs(title = "Distance to Nearest Senior High Schools in Garut",
       subtitle = "APM Index: 51.04%",
       fill = "Distance (km)",
       caption = "Data: Jawa Barat Education Office | Map: R. Ramadhan for JDS") +
  theme_void() +
  theme(panel.background = element_rect(fill = "#fdf9f5", colour = "#fdf9f5"),
        plot.background = element_rect(fill = "#fdf9f5", colour = "#fdf9f5"),
        plot.title = element_text(hjust = 0.5, family = "Playfair Display",
                                  colour = "#090740",
                                  size = 14,
                                  margin = margin(10, 0, -5, 0)),
        plot.subtitle = element_text(hjust = 0.5, family = "Roboto Condensed",
                                     colour = "#090740",
                                     size = 10,
                                     margin = margin(8, 0, -5, 0)),
        plot.caption = element_text(family = "Roboto Condensed",
                                    size = 4,
                                    hjust = .5,
                                    colour = "black"),
        legend.direction = "horizontal",
        legend.position = "bottom",
        legend.title = element_text(family = "Roboto Condensed", hjust = 0.5,
                                    colour = "black", size = 8),
        legend.text = element_text(colour = "black",
                                   size = 5))

ggsave(
  garut_sma_plot,
  filename = "Outfile/202112_jaraksma_6garut.png",
  width = 6,
  height = 5,
  dpi = 300
)

# > 7. subang ----
subang_sma <- sma_shp %>% 
  dplyr::filter(KABKOT == "Subang")

subang_sma_join <- subang_sma %>% 
  st_union()

subang_distance <- st_distance(
  sf_subang,
  subang_sma_join
) %>% 
  units::set_units("km")

subang_sma_dist <- json_subang %>% 
  st_as_sf() %>% 
  mutate(jarak = subang_distance) %>% 
  drop_na()

subang_sma_plot <- 
  ggplot()+
  geom_sf(
    data = subang_sma_dist,
    aes(fill = as.numeric(jarak)),
    color = "NA"
  ) +
  coord_sf(
    xlim = xlim_subang,
    ylim = ylim_subang
  ) +
  scale_fill_gradientn(
    colours = rocket(10),
    guide = guide_colourbar(
      title.position = "top",
      direction = "horizontal",
      barwidth = 15,
      barheight = 0.5,
      frame.colour = "black"
    )
  ) +
  geom_sf(
    data = subang_sma,
    size = 1.5, shape = 21, 
    fill = "#FFC800", alpha=.7, stroke = .5
  )+
  labs(title = "Distance to Nearest Senior High Schools in Subang",
       subtitle = "APM Index: 51.82%",
       fill = "Distance (km)",
       caption = "Data: Jawa Barat Education Office | Map: R. Ramadhan for JDS") +
  theme_void() +
  theme(panel.background = element_rect(fill = "#fdf9f5", colour = "#fdf9f5"),
        plot.background = element_rect(fill = "#fdf9f5", colour = "#fdf9f5"),
        plot.title = element_text(hjust = 0.5, family = "Playfair Display",
                                  colour = "#090740",
                                  size = 14,
                                  margin = margin(10, 0, -5, 0)),
        plot.subtitle = element_text(hjust = 0.5, family = "Roboto Condensed",
                                     colour = "#090740",
                                     size = 10,
                                     margin = margin(8, 0, -5, 0)),
        plot.caption = element_text(family = "Roboto Condensed",
                                    size = 4,
                                    hjust = .5,
                                    colour = "black"),
        legend.direction = "horizontal",
        legend.position = "bottom",
        legend.title = element_text(family = "Roboto Condensed", hjust = 0.5,
                                    colour = "black", size = 8),
        legend.text = element_text(colour = "black",
                                   size = 5))

ggsave(
  subang_sma_plot,
  filename = "Outfile/202112_jaraksma_7subang.png",
  width = 6,
  height = 5,
  dpi = 300
)

# > 8. Bogor ----
bogor_sma <- sma_shp %>% 
  dplyr::filter(KABKOT %in% c("Bogor", "Kota Bogor"))

bogor_sma_join <- bogor_sma %>% 
  st_union()

bogor_distance <- st_distance(
  sf_bogor,
  bogor_sma_join
) %>% 
  units::set_units("km")

bogor_sma_dist <- json_bogor %>% 
  st_as_sf() %>% 
  mutate(jarak = bogor_distance) %>% 
  drop_na()

bogor_sma_plot <- 
  ggplot()+
  geom_sf(
    data = bogor_sma_dist,
    aes(fill = as.numeric(jarak)),
    color = "NA"
  ) +
  coord_sf(
    xlim = xlim_bogor,
    ylim = ylim_bogor
  ) +
  scale_fill_gradientn(
    colours = rocket(10),
    guide = guide_colourbar(
      title.position = "top",
      direction = "horizontal",
      barwidth = 15,
      barheight = 0.5,
      frame.colour = "black"
    )
  ) +
  geom_sf(
    data = bogor_sma,
    size = 1.5, shape = 21, 
    fill = "#FFC800", alpha=.7, stroke = .5
  )+
  labs(title = "Distance to Nearest Senior High Schools in Bogor Region",
       subtitle = "Bogor: 53.31% | Kota Bogor: 62.21%",
       fill = "Distance (km)",
       caption = "Data: Jawa Barat Education Office | Map: R. Ramadhan for JDS") +
  theme_void() +
  theme(panel.background = element_rect(fill = "#fdf9f5", colour = "#fdf9f5"),
        plot.background = element_rect(fill = "#fdf9f5", colour = "#fdf9f5"),
        plot.title = element_text(hjust = 0.5, family = "Playfair Display",
                                  colour = "#090740",
                                  size = 14,
                                  margin = margin(10, 0, -5, 0)),
        plot.subtitle = element_text(hjust = 0.5, family = "Roboto Condensed",
                                     colour = "#090740",
                                     size = 10,
                                     margin = margin(8, 0, -5, 0)),
        plot.caption = element_text(family = "Roboto Condensed",
                                    size = 4,
                                    hjust = .5,
                                    colour = "black"),
        legend.direction = "horizontal",
        legend.position = "bottom",
        legend.title = element_text(family = "Roboto Condensed", hjust = 0.5,
                                    colour = "black", size = 8),
        legend.text = element_text(colour = "black",
                                   size = 5))

ggsave(
  sumedang_sma_plot,
  filename = "Outfile/202112_jaraksma_8bogor.png",
  width = 6,
  height = 5,
  dpi = 300
)

# > 9. indramayu ----
indramayu_sma <- sma_shp %>% 
  dplyr::filter(KABKOT == "Indramayu")

indramayu_sma_join <- indramayu_sma %>% 
  st_union()

indramayu_distance <- st_distance(
  sf_indramayu,
  indramayu_sma_join
) %>% 
  units::set_units("km")

indramayu_sma_dist <- json_indramayu %>% 
  st_as_sf() %>% 
  mutate(jarak = indramayu_distance) %>% 
  drop_na()

indramayu_sma_plot <- 
  ggplot()+
  geom_sf(
    data = indramayu_sma_dist,
    aes(fill = as.numeric(jarak)),
    color = "NA"
  ) +
  coord_sf(
    xlim = xlim_indramayu,
    ylim = ylim_indramayu
  ) +
  scale_fill_gradientn(
    colours = rocket(10),
    guide = guide_colourbar(
      title.position = "top",
      direction = "horizontal",
      barwidth = 15,
      barheight = 0.5,
      frame.colour = "black"
    )
  ) +
  geom_sf(
    data = indramayu_sma,
    size = 1.5, shape = 21, 
    fill = "#FFC800", alpha=.7, stroke = .5
  )+
  labs(title = "Distance to Nearest Senior High Schools in Indramayu",
       subtitle = "APM Index: 55.89%",
       fill = "Distance (km)",
       caption = "Data: Jawa Barat Education Office | Map: R. Ramadhan for JDS") +
  theme_void() +
  theme(panel.background = element_rect(fill = "#fdf9f5", colour = "#fdf9f5"),
        plot.background = element_rect(fill = "#fdf9f5", colour = "#fdf9f5"),
        plot.title = element_text(hjust = 0.5, family = "Playfair Display",
                                  colour = "#090740",
                                  size = 14,
                                  margin = margin(10, 0, -5, 0)),
        plot.subtitle = element_text(hjust = 0.5, family = "Roboto Condensed",
                                     colour = "#090740",
                                     size = 10,
                                     margin = margin(8, 0, -5, 0)),
        plot.caption = element_text(family = "Roboto Condensed",
                                    size = 4,
                                    hjust = .5,
                                    colour = "black"),
        legend.direction = "horizontal",
        legend.position = "bottom",
        legend.title = element_text(family = "Roboto Condensed", hjust = 0.5,
                                    colour = "black", size = 8),
        legend.text = element_text(colour = "black",
                                   size = 5))

ggsave(
  indramayu_sma_plot,
  filename = "Outfile/202112_jaraksma_9indramayu.png",
  width = 6,
  height = 5,
  dpi = 300
)
