# 30DayMapChallenge | November 28, 2022 | 3D
# Mission Viejo, CA

# thanks to the tutorials from @MrPecners and @tylermorganwall
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
mv_bb <- getbb("Mission Viejo, California", format_out = "sf_polygon")

# major streets -----------------------------------------------------------
mv_big <- mv_bb %>%
  opq()%>%
  add_osm_feature(key = "highway",
                  value = c("motorway", "primary", 
                            "secondary", "tertiary", "trunk")) %>% 
  osmdata_sf()

# links --------------------------------------------------------------------
mv_links <- mv_bb %>%
  opq()%>%
  add_osm_feature(key = "highway",
                  value = c("motorway_link", "primary_link", 
                            "secondary_link", "tertiary_link", "trunk_link")) %>% 
  osmdata_sf()

# minor streets -----------------------------------------------------------
mv_small <- mv_bb %>%
  opq()%>%
  add_osm_feature(key = "highway",
                  value = c("residential", "pedestrian", "road", "footway")) %>% 
  osmdata_sf()

# water -----------------------------------------------------------
mv_water <- mv_bb %>%
  opq()%>%
  add_osm_feature(key = "natural",
                  value = c("water")) %>% 
  osmdata_sf()

# get the elevation data for malibu ---------------------------------------
dem_mv <- get_elev_raster(locations = mv_bb, z = 13, clip = "locations", neg_to_na = "TRUE")

# convert to matrix format ------------------------------------------------
mat_mv <- raster_to_matrix(dem_mv)

# base map ----------------------------------------------------------------
base_map = mat_mv %>% 
  height_shade() %>% 
  add_overlay(sphere_shade(mat_mv, texture = "unicorn", 
                           zscale = 4, colorintensity = 5), alphalayer=0.5) %>%
  add_shadow(lamb_shade(mat_mv, zscale = 6), 0) %>%
  add_shadow(ambient_shade(mat_mv), 0) %>%
  add_shadow(texture_shade(mat_mv, detail = 8/10, contrast = 9, brightness = 11), 0.1)

#plot_map(base_map)

# create roads layer ------------------------------------------------------
roads_layer = generate_line_overlay(mv_big$osm_lines, extent = extent(dem_mv),
                                    linewidth = 1.5, color = "#F2F2F2",
                                    heightmap = mat_mv) %>%
  add_overlay(generate_line_overlay(mv_links$osm_lines, extent = extent(dem_mv),
                                    linewidth = 1, color = "#F2F2F2",
                                    heightmap = mat_mv)) %>%
  add_overlay(generate_line_overlay(mv_small$osm_lines, extent = extent(dem_mv),
                                    linewidth = 0.75, color = "#F2F2F2",
                                    heightmap = mat_mv))

# create water layer ------------------------------------------------------
water_layer = generate_polygon_overlay(
    mv_water$osm_polygons, extent = extent(dem_mv),
    linewidth = 0, palette = "#91a7bc",
    heightmap = mat_mv)

# set sizing --------------------------------------------------------------
# copied from @MrPecners tutorial -----------------------------------------
# https://spencerschien.info/post/data_viz_how_to/high_quality_ray --------

w <- nrow(mat_mv)
h <- ncol(mat_mv)

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
  plot_3d(mat_mv,
          windowsize = c(800 * wr,800 * hr),
          solid = TRUE,
          zscale = 25,
          phi = 30,
          zoom = 0.5,
          theta = 0, 
          background = "#F2F2F2")

# create render high quality ----------------------------------------------
render_highquality(
  "mv_highres.png", 
  parallel = TRUE, 
  samples = 300,
  light = TRUE, 
  interactive = FALSE,
  intensity_env = 1.5,
  rotate_env = 180,
  width = round(6000 * wr), 
  height = round(6000 * hr))

#rgl::rgl.close()

# create annotations and final image --------------------------------------
# load image --------------------------------------------------------------
mv <- image_read("mv_highres.png") %>% 
  image_annotate("MISSION VIEJO", color = "black", size = 300, weight = 1000, 
                 gravity = "northwest",location = "+400+500") %>% 
  image_annotate("California", color = "black", size = 250,  gravity = "northwest", 
                 location = "+400+800") %>% 
  image_annotate("#30DayMapChallenge | Design: Ryan Hart", color = "black", size = 100, 
                 gravity = "south", location = "+0+500") %>% 
image_write("mv_highres_final.png")



