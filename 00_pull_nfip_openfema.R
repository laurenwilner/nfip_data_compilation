#-----------------
# purpose: pull openfema data on NFIP designations
# author: lauren blair wilner
# last edited: january 2, 2025

#-----------------
# set up
if (!requireNamespace('pacman', quietly = TRUE)){install.packages('pacman')}
pacman::p_load(sf, tigris, MetBrewer, lubridate, arrow, knitr, kableExtra, patchwork, rfema, tidyverse, jsonlite)

raw_data_dir <- "/Users/laurenwilner/Desktop/Desktop/epidemiology_PhD/01_data/raw/"
fema_data_sets <- fema_data_sets()

pal <- met.brewer("Derain", 9)

#-----------------
# pull data
vars <- c("communityIdNumber", "countyCode", "censusYear", "censusGeoid", "geometry")
nfip <- open_fema("NfipCommunityLayerComprehensive")

parse_layer_geometry <- function(geom_str) {
  # Safely parse the geometry string
  geom_list <- tryCatch(eval(parse(text = geom_str)), error = function(e) NULL)
  if (is.null(geom_list)) return(NULL) # Return NULL if parsing fails

  # Validate and extract coordinates
  if (!"coordinates" %in% names(geom_list)) return(NULL)
  
  # Clean coordinates to remove any empty or invalid elements
  cleaned_coords <- lapply(geom_list$coordinates, function(polygon) {
    # Ensure all rings within the polygon are valid
    lapply(polygon, function(ring) {
      if (length(ring) > 0) return(ring) else NULL
    }) |> discard(is.null)
  }) |> discard(is.null)

  # Return MULTIPOLYGON if cleaned coordinates are valid
  if (length(cleaned_coords) > 0) {
    return(st_multipolygon(cleaned_coords))
  } else {
    return(NULL)
  }
}

nfip_sf <- nfip %>%
  mutate(
    geometry = lapply(layerGeometry, parse_layer_geometry), # Parse geometries
    geometry = st_sfc(geometry, crs = 4326) # Combine into an sf geometry column
  ) %>%
  st_as_sf(sf_column_name = "geometry")
  
#-----------------
# map data
nfip_map <- nfip %>%
  st_transform(4326) %>%
  ggplot() +
  geom_sf(aes(fill = NFIP_ZONE), color = "black") +
  scale_fill_manual(values = pal) +
  theme_minimal() +
  theme(legend.position = "bottom") +
  labs(title = "NFIP Designations",
       subtitle = "Community Layer Comprehensive",
       fill = "NFIP Zone") +
  theme(plot.title = element_text(size = 20),
        plot.subtitle = element_text(size = 15))
