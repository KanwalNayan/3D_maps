# 1. LIBRARIES
#-------------

install.packages("pacman")
pacman::p_load(
  terra,
  elevatr,
  sf,
  geodata,
  tidyverse,
  rayshader
)

# 2. COUNTRY BORDERS
#-------------------
setwd("C:/Users/KanwalNayan.Singh/Desktop/Codes/R/New_maps/3D_Elevation_and_River_Maps/rudraprayag/")
path <- getwd()

country_sf <- read_sf("C:/Users/KanwalNayan.Singh/Desktop/Codes/R/New_maps/3D_Elevation_and_River_Maps/rudraprayag/rudraprayag.shp")

# country_sf <- geodata::gadm(
#   country = "CHE",
#   level = 0,
#   path = path
# ) |>
#   sf::st_as_sf()

# 3. DOWNLOAD RIVERS
#-------------------

# url <- "https://data.hydrosheds.org/file/HydroRIVERS/HydroRIVERS_v10_eu_shp.zip"
# destfile <- basename(url)
# 
# download.file(
#   url = url,
#   destfile = destfile,
#   mode = "wb"
# )
# 
# unzip(destfile)

# 4. LOAD RIVERS
#---------------

filename <- list.files(
  path = "HydroRIVERS_v10_as_shp",
  pattern = ".shp",
  full.names = TRUE
)


country_sf_proj <- st_transform(country_sf, crs="+proj=longlat +datum=WGS84")
country_sf <- country_sf_proj


country_bbox <- sf::st_bbox(country_sf)

# Switzerland
# xmin      ymin      xmax      ymax
#  5.956063 45.817059 10.495112 47.808483

# bbox_wkt <- "POLYGON((
#     5.956063 45.817059,
#     5.956063 47.808483,
#     10.495112 47.808483,
#     10.495112 45.817059,
#     5.956063 45.817059
# ))"

# Rudraprayag
# xmin     ymin     xmax     ymax 
# 77.57563 28.70948 81.03763 31.45230
# Bounding box:  xmin: 78.81772 ymin: 30.16507 xmax: 79.35838 ymax: 30.80799 - Rudraprayag

bbox_wkt <- "POLYGON((
    78.81772 30.16507,
    78.81772 30.80799,
    79.35838 30.80799,
    79.35838 30.16507,
    78.81772 30.16507
))"

country_rivers <- sf::st_read(
  filename,
  wkt_filter = bbox_wkt
) |>
  sf::st_intersection(
    country_sf
  )

plot(sf::st_geometry(country_rivers))

# 5. RIVER WIDTH
#---------------

sort(
  unique(
    country_rivers$ORD_FLOW
  )
)

crs_country <- "+proj=longlat +datum=WGS84"

country_river_width <- country_rivers |>
  dplyr::mutate(
    width = as.numeric(
      ORD_FLOW
    ),
    width = dplyr::case_when(
      width == 4 ~ 8, 
      width == 5 ~ 6,
      width == 6 ~ 4,
      width == 7 ~ 2,
      TRUE ~ 0
    )
  ) |>
  sf::st_as_sf() |>
  sf::st_transform(crs = crs_country)

# 6. DEM
#-------

dem <- elevatr::get_elev_raster(
  locations = country_sf,
  z = 9, clip = "locations"
)

dem_country <- dem |>
  terra::rast() |>
  terra::project(crs_country)

dem_matrix <- rayshader::raster_to_matrix(
  dem_country
)

# 7. RENDER SCENE
#----------------

dem_matrix |>
  rayshader::height_shade(
    texture = colorRampPalette(
      c(
        "#fcc69f",
        "#c67847"
      )
    )(512)
  ) |>
  rayshader::add_overlay(
    rayshader::generate_line_overlay(
      geometry = country_river_width,
      extent = dem_country,
      heightmap = dem_matrix,
      color = "#387B9C",
      linewidth = country_river_width$width,
      data_column_width = "width"
    ), alphalayer = 1
  ) |>
  rayshader::plot_3d(
    dem_matrix,
    zscale = 15,
    solid = FALSE,
    shadow = TRUE,
    shadow_darkness = 1,
    background = "white",
    windowsize = c(600, 600),
    zoom = .5,
    phi = 85,
    theta = 0
  )


rayshader::render_camera(
  zoom = .75
)

# 8. RENDER OBJECT
#-----------------

u <- "https://dl.polyhaven.org/file/ph-assets/HDRIs/hdr/4k/photo_studio_loft_hall_4k.hdr"
hdri_file <- basename(u)

download.file(
  url = u,
  destfile = hdri_file,
  mode = "wb"
)

file_name <- "Rudraprayag-3d-elevation-rivers.png"

rayshader::render_highquality(
  filename = file_name,
  preview = TRUE,
  light = FALSE,
  environment_light = hdri_file,
  intensity_env = 1.25,
  interactive = FALSE,
  width = 3000,
  height = 3000
)
