library(dplyr)
library(ggplot2)
library(magrittr)
library(mclust)
library(raster)
library(splines)
library(tidyr)
library(sf)

# 2 minute video / 24 fps
n_frames <- 2 * 60 * 24

# Latitude: -90, 90 
n_frames / 180

df_grid = expand.grid(lat = seq(90, -89), lon = seq(-180, 179))

# NDVI (Vegetation index, density)
df <- read.csv("data/MOD_NDVI_M_2024-09-01_rgb_360x180.CSV", header = FALSE)
df[df == 99999] <- NA
df_grid$density = unlist(df) 

# Temperature (needed to capture no density regions)
df_temp <- read.csv("data/MOD_LSTD_CLIM_M_2001-12-01_rgb_360x180.CSV", header = FALSE)
df_temp[df_temp == 99999] <- NA
df_grid$temperature = unlist(df_temp)

df_density_filter <- df_grid %>%
  filter(!is.na(density)) %>%
  mutate(min_dens = min(density),
         max_dens = max(density),
         density = (density - min_dens) / (max_dens - min_dens)) %>%
  dplyr::select(lat, lon, density)

gmm_model <- Mclust(df_density_filter$density, G = 3)
density_vals <- df_density_filter$density
gmm_pred <- predict(gmm_model, newdata = density_vals)
gmm_paras <- gmm_model$parameters

df_density_filter$density_class <- gmm_pred$classification
df_density_filter$dens_1 <- dnorm(density_vals, mean = gmm_paras$mean[[1]], 
                                  sd = sqrt(gmm_paras$variance$sigmasq[[1]]))
df_density_filter$dens_2 <- dnorm(density_vals, mean = gmm_paras$mean[[2]], 
                                  sd = sqrt(gmm_paras$variance$sigmasq[[2]]))
df_density_filter$dens_3 <- dnorm(density_vals, mean = gmm_paras$mean[[3]], 
                                  sd = sqrt(gmm_paras$variance$sigmasq[[3]]))
df_density_filter %<>%
  mutate(dens = gmm_model$parameters$pro[[1]] * dens_1 +
                gmm_model$parameters$pro[[2]] * dens_2 +
                gmm_model$parameters$pro[[3]] * dens_3)

bw <- 0.025
n_class_1 <- nrow(df_density_filter[df_density_filter$density_class == 1, ])
n_class_2 <- nrow(df_density_filter[df_density_filter$density_class == 2, ])
n_class_3 <- nrow(df_density_filter[df_density_filter$density_class == 3, ])
df_density_filter %>%
  ggplot(aes(x = density, 
             color = factor(density_class), 
             fill = factor(density_class))) +
  geom_histogram(color = "white", binwidth = bw, alpha = 0.5) +
  geom_line(aes(x = density, 
                y = dens * nrow(df_density_filter) * bw), 
            color = "darkgrey", linewidth = 1.5) +
  geom_line(aes(x = density, 
                y = dens_1 * n_class_1 * bw),
            color = "#F8766D", linewidth = 2) +
  geom_line(aes(x = density, 
                y = dens_2 * n_class_2 * bw),
            color = "#00BA38", linewidth = 2) +
  geom_line(aes(x = density, 
                y = dens_3 * n_class_3 * bw),
            color = "#619CFF", linewidth = 2) +
  theme_bw()

df_density_filter %>%
  ggplot(aes(x = lon, 
             y = lat, 
             fill = factor(density_class))) +
  geom_tile(alpha = 1) +
  theme_bw()

df_map <- df_density_filter %>%
  mutate(class = density_class) %>%
  dplyr::select(lat, lon, density, class) %>%
  mutate(biome = case_when(
    class == 1 & (lat > 52 | lat < -52) ~ 'tundra',
    class == 1 & (lat <= 52 & lat >= -52) ~ "desert",
    class == 2 & (lat > 55 | lat < -55) ~ "coniferous",
    class == 2 & (lat < 25 & lat > -25) ~ "shrubland",
    class == 2 & (lat >= 25 | lat <= -25) ~ "grassland",
    class == 3 & (lat > 32 | lat < -32) ~ "deciduous",
    class == 3 & (lat <= 32 & lat >= -32) ~ "rainforest",
    TRUE ~ 'other'
  ))

# Complement with no density regions
df_map <- left_join(
  df_grid %>% 
    filter(!is.na(density) | !is.na(temperature)) %>%
    dplyr::select(lat, lon),
  df_map,
  by = c("lat", "lon")
) 
df_map %<>%
  mutate(biome = ifelse(is.na(biome), "ice", biome),
         density = ifelse(is.na(density), 0, density))

df_map_bg <- df_map %>%
  dplyr::select(lat, lon)

df_map %>%
  ggplot(aes(x = lon, y = lat, fill = biome)) +
  geom_tile(data = df_map_bg, fill = "lightgrey") +
  geom_tile(alpha = 1) +
  #facet_wrap(~ biome) +
  theme_bw()

png("img/world_map_rainforest_new.png", 
    width = 1920, height = 1080, units = "px", bg = "darkgrey")

df_map_bg %>%
  # Shift so that Pacific is in the center = more space for biome animation
  mutate(lon = ifelse(lon < 0, 180 - abs(lon), lon - 180)) %>%
  mutate(lon = lon + 180) %>%
  ggplot(aes(x = lon, y = lat)) +
  geom_tile(fill = "lightgrey") +
  geom_tile(data = df_map %>% 
              mutate(lon = ifelse(lon < 0, 180 - abs(lon), lon - 180)) %>%
              mutate(lon = lon + 180) %>%
              filter(biome == "rainforest"), fill = "lightpink1") +
  theme_void()

dev.off()

diff_round <- function(x) { 
  diff(c(0, round(cumsum(x)))) 
}
df_map_lat <- df_map %>%
  mutate(lat_bin = floor(lat / 2) * 2) %>%
  group_by(lat_bin, biome) %>%
  summarize(n = n()) %>%
  mutate(frac = diff_round(100 * n / sum(n))) %>%
  tibble()

df_map_lat <- left_join(
  data.frame(lat_bin = seq(-90, 90, by = 2)),
  df_map_lat,
  by = "lat_bin"
)
df_map_lat %<>%
  mutate(biome = ifelse(is.na(biome), "none", biome),
         frac = ifelse(is.na(frac), 100, frac))

df_dens_lat <- df_map %>%
  mutate(lat_bin = floor(lat / 2) * 2) %>%
  group_by(lat_bin) %>%
  summarize(mean_density = mean(density, na.rm = TRUE))

df_dens_lat <- left_join(
  data.frame(lat_bin = seq(-90, 90, by = 2)),
  df_dens_lat,
  by = "lat_bin"
)
df_dens_lat %<>%
  mutate(mean_density = 100 * ifelse(is.na(mean_density), 0, mean_density))

df_map_lat %>%
  ggplot(aes(x = lat_bin, y = frac)) +
  geom_bar(aes(fill = biome), stat = "identity") +
  theme_bw()

df_dens_lat %>%
  ggplot(aes(x = lat_bin, y = mean_density)) +
  geom_line(color = "darkgrey",
            linewidth = 2) +
  theme_bw()


df_out <- inner_join(
  df_map_lat %>% 
    dplyr::select(lat_bin, biome, frac) %>% 
    spread(biome, -lat_bin),
  df_dens_lat,
  by = "lat_bin"
)
df_out[is.na(df_out)] <- 0

write.table(df_out, file = "data/biome_control_new.csv", sep = ";",
            quote = FALSE, row.names = FALSE) 
