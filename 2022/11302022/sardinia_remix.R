# 30DayMapChallenge | November 30, 2022 | Remix
# Day 7 Raster + Day 28 3D
# Sardinia, Italy

# thanks again to the tutorials from @MrPecners and @tylermorganwall
# relied heavily on many of the defaults and settings from their tutorials

# load libraries ----------------------------------------------------------
library(tidyverse)
library(osmdata)
library(raster)
library(sf)
library(rayshader)
library(elevatr)
library(magick)

# get shape boundary ------------------------------------------------------
s_bb <- getbb("Sardinia, Italy", format_out = "sf_polygon")

# major streets -----------------------------------------------------------
s_big <- s_bb %>%
  opq(timeout = 60) %>%
  add_osm_feature(key = "highway",
                  value = c("motorway", "primary", 
                            "secondary", "tertiary", "trunk")) %>% 
  osmdata_sf()

# links --------------------------------------------------------------------
s_links <- s_bb %>%
  opq(timeout = 60) %>%
  add_osm_feature(key = "highway",
                  value = c("motorway_link", "primary_link", 
                            "secondary_link", "tertiary_link", "trunk_link")) %>% 
  osmdata_sf()

# minor streets -----------------------------------------------------------
s_small <- s_bb %>%
  opq(timeout = 60) %>%
  add_osm_feature(key = "highway",
                  value = c("residential", "pedestrian", "road", "footway")) %>% 
  osmdata_sf()

# water -----------------------------------------------------------
s_water <- s_bb %>%
  opq()%>%
  add_osm_feature(key = "natural",
                  value = c("water")) %>% 
  osmdata_sf()

# get the elevation data for sardinia --------------------------------------
dem_sardinia <- get_elev_raster(locations = s_bb$polygon, z = 9, clip = "locations", neg_to_na = "TRUE")

# convert to matrix format ------------------------------------------------
mat_s <- raster_to_matrix(dem_sardinia)

# base map ----------------------------------------------------------------
base_map = mat_s %>% 
  height_shade() %>% 
  add_overlay(sphere_shade(mat_s, texture = "unicorn", 
                           zscale = 4, colorintensity = 5), alphalayer = 0.5) %>%
  add_shadow(lamb_shade(mat_s, zscale = 6), 0) %>%
  add_shadow(ambient_shade(mat_s), 0) %>%
  add_shadow(texture_shade(mat_s, detail = 8/10, contrast = 9, brightness = 11), 0.1)

#plot_map(base_map)

# create roads layer ------------------------------------------------------
roads_layer = generate_line_overlay(s_big$osm_lines, extent = extent(dem_sardinia),
                                    linewidth = 1, color = "#F2F2F2",
                                    heightmap = mat_s) %>%
  add_overlay(generate_line_overlay(s_links$osm_lines, extent = extent(dem_sardinia),
                                    linewidth = 0.75, color = "#F2F2F2",
                                    heightmap = mat_s)) %>%
  add_overlay(generate_line_overlay(s_small$osm_lines, extent = extent(dem_sardinia),
                                    linewidth = 0.5, color = "#F2F2F2",
                                    heightmap = mat_s))

# create water layer ------------------------------------------------------
water_layer = generate_polygon_overlay(
  s_water$osm_polygons, extent = extent(dem_sardinia),
  linewidth = 0, palette = "#91a7bc",
  heightmap = mat_s)

# set sizing --------------------------------------------------------------
# copied from @MrPecners tutorial -----------------------------------------
# https://spencerschien.info/post/data_viz_how_to/high_quality_ray --------

w <- nrow(mat_s)
h <- ncol(mat_s)

wr <- w / max(c(w,h))
hr <- h / max(c(w,h))

if (min(c(wr, hr)) < .75) {
  if (wr < .75) {
    wr <- .75
  } else {
    hr <- .75
  }
}

# create 3d map -----------------------------------------------------------
base_map %>% 
  add_overlay(roads_layer) %>%
  add_overlay(water_layer) %>%
  plot_3d(mat_s,
          windowsize = c(800 * wr, 800 * hr),
          solid = TRUE,
          zscale = 12,
          phi = 90,
          zoom = 0.65,
          theta = 0, 
          background = "#F2F2F2")

# create render high quality ----------------------------------------------
render_highquality(
  "sardinia_remix.png", 
  parallel = TRUE, 
  samples = 300,
  light = TRUE, 
  interactive = FALSE,
  intensity_env = 1.7,
  rotate_env = 180,
  width = round(3000 * wr), 
  height = round(3000 * hr))

#rgl::rgl.close()

# create annotations and final image --------------------------------------
# load image --------------------------------------------------------------
mv <- image_read("sardinia_remix.png") %>% 
  image_annotate("SARDINIA", color = "#003399", size = 250, weight = 1000, 
                 gravity = "northwest",location = "+60+60") %>% 
  image_annotate("Italy", color = "#003399", size = 150,  gravity = "northwest", 
                 location = "+60+280") %>% 
  image_annotate("#30DayMapChallenge | Design: Ryan Hart", color = "#003399", size = 55, 
                 gravity = "south", location = "+0+60") %>% 
  image_write("sardinia_remix_final.png")

