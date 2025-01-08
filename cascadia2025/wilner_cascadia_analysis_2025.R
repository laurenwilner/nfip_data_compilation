#-------------------------
# setup
library(pacman)
p_load(tidyverse, ggplot2, broom, tigris, sf, leaflet, patchwork, arrow, cowplot)
source("~/Desktop/Desktop/epidemiology_PhD/00_repos/nfip_data_compilation/cascadia2025/colors.r")
data_dir <- "~/Desktop/Desktop/epidemiology_PhD/01_data/raw/"

read_file <- function(file) {
  read.csv(file)
}
zctas <- zctas(year = 2018, cb = TRUE) %>% 
    select("ZCTA5CE10", "geometry") %>% 
    rename("zcta" = "ZCTA5CE10")
states <- states(year = 2018, cb = TRUE) %>% 
    select("STUSPS", "geometry") %>% 
    rename("state" = "STUSPS")
counties <- counties(year = 2018, cb = TRUE) %>% 
    select("GEOID", "geometry") %>% 
    rename("fips" = "GEOID")
wa_counties <- counties %>% filter(str_starts(fips, "53"))
id_counties <- counties %>% filter(str_starts(fips, "16"))
or_counties <- counties %>% filter(str_starts(fips, "41"))
wa_state <- states %>% filter(state=="WA")
id_state <- states %>% filter(state=="ID")
or_state <- states %>% filter(state=="OR")
contiguous_state_fips <- sprintf("%02d", c(1:56)[-c(2, 15)])

#-------------------------
# read in data

# nfip data
nfip_designations <- read_csv(paste0(data_dir, "nfip/NfipCommunityLayerNoOverlapsSplit.csv"))
nfip_status_book <- read_csv(paste0(data_dir, "nfip/NfipCommunityStatusBook.csv"))
flood_events <- read_csv(paste0(data_dir, "nfip/DisasterDeclarationsSummaries.csv"))
layers <- st_layers(paste0(data_dir, "nfip/cl_2024_v01_OpenFEMA.gdb"))
nfip_gdb_whole <- st_read(paste0(data_dir, "nfip/cl_2024_v01_OpenFEMA.gdb")) %>%
    filter(state_fips %in% contiguous_state_fips)
nfip_gdb_split <- st_read(paste0(data_dir, "nfip/cl_2024_v01_OpenFEMA.gdb"),
    "nfipcommunitylayernooverlapssplit_2023v1") %>%
    filter(state_fips %in% contiguous_state_fips)
nfip_vars <- c("id", "cis_cid", "Shape")
nfip_communities <- nfip_gdb_whole %>% 
    filter(!is.na(cis_cid)) %>% 
    select(all_of(nfip_vars)) %>% 
    group_by(cis_cid) %>%
    filter(id == max(id)) %>%
    ungroup() # pulling max id for now, since i dont know why there are so many entries for each community
status_vars <- c("communityIdNumber", "participatingInNFIP", "classRating",
        "sfhaDiscount", "nonSfhaDiscount", "state", "county")
community_status <- nfip_status_book %>% 
    select(all_of(status_vars)) %>% 
    mutate(classRating = ifelse(is.na(classRating), 0, classRating)) 
community_status_map_df <- community_status %>%
    left_join(nfip_communities, by = c("communityIdNumber" = "cis_cid")) %>% 
    rename(geometry = Shape) %>%
    select(c(geometry, participatingInNFIP, state, county)) %>% 
    st_as_sf() %>%
    st_crop(xmin = -125, xmax = -66.9, ymin = 20, ymax = 49.384358)

# WA
nfhl_wa <- st_read(paste0(data_dir, "nfip/NFHL_53_20241124/NFHL_53_20241124.gdb"), "S_FLD_HAZ_AR")

# ID
nfhl_id <- st_read(paste0(data_dir, "nfip/NFHL_16_20241030/NFHL_16_20241030.gdb"), "S_FLD_HAZ_AR")

# OR 
nfhl_or <- st_read(paste0(data_dir, "nfip/NFHL_41_20241121/NFHL_41_20241121.gdb"), "S_FLD_HAZ_AR")

# US
continental_us <- st_crop(states, xmin = -125, xmax = -66.9, ymin = 20, ymax = 49.384358)
continental_us_counties <- st_crop(counties, xmin = -125, xmax = -66.9, ymin = 20, ymax = 49.384358)

#-------------------------
# nfip processing

# washington 
nfhl_wa <- nfhl_wa %>%
    mutate(sfha = ifelse(FLD_ZONE %in% c("A", "AE", "AO", "AH", "A99", "VE", "V", "V1-30", "V30", "V30-VE"),
            1,
            0)) %>%
    select(sfha, SHAPE) %>%
    rename(geometry = SHAPE) %>% 
    st_as_sf()
sfhas_wa <- nfhl_wa %>% filter(sfha == 1)
sfha_nfip_wa <- ggplot(data = community_status_map_df %>% filter(state == "WA")) +
    geom_sf(aes(fill = factor(participatingInNFIP)), color = NA) +
    geom_sf(data = wa_state, fill = NA, color = "#000000") +
    scale_fill_manual(values = c("1" = small_pal[1], "0" = small_pal[4], "Required" = small_pal[3]),
                      labels = c("1" = "Optional", "0" = "Unavailable", "Required" = "Required")) +
    geom_sf(data = sfhas_wa, aes(fill = "Required"), color = NA) +
    theme_void() +
    theme(panel.grid = element_blank(),
          axis.text.y = element_blank(),
          axis.text.x = element_blank(),
          axis.ticks = element_blank(),
          legend.position = "none",
          plot.title = element_text(hjust = 0.5),
          legend.title = element_blank())

# idaho
nfhl_id <- nfhl_id %>%
    mutate(sfha = ifelse(FLD_ZONE %in% c("A", "AE", "AO", "AH", "A99", "VE", "V", "V1-30", "V30", "V30-VE"),
            1,
            0)) %>%
    select(sfha, SHAPE) %>%
    rename(geometry = SHAPE) %>% 
    st_as_sf()
sfhas_id <- nfhl_id %>% filter(sfha == 1)
sfha_nfip_id <- ggplot(data = community_status_map_df %>% filter(state == "ID")) +
    geom_sf(aes(fill = factor(participatingInNFIP)), color = NA) +
    geom_sf(data = id_state, fill = NA, color = small_pal[5]) +
    scale_fill_manual(values = c("1" = small_pal[1], "0" = small_pal[4], "Required" = small_pal[3]),
                      labels = c("1" = "Optional", "0" = "Unavailable", "Required" = "Required")) +
    geom_sf(data = sfhas_id, aes(fill = "Required"), color = NA) +
    theme_void() +
    theme(panel.grid = element_blank(),
          axis.text.y = element_blank(),
          axis.text.x = element_blank(),
          axis.ticks = element_blank(),
          legend.position = "bottom",
          plot.title = element_text(hjust = 0.5),
          legend.title = element_blank())


# oregon
nfhl_or <- nfhl_or %>%
    mutate(sfha = ifelse(FLD_ZONE %in% c("A", "AE", "AO", "AH", "A99", "VE", "V", "V1-30", "V30", "V30-VE"),
            1,
            0)) %>%
    select(sfha, SHAPE) %>%
    rename(geometry = SHAPE) %>% 
    st_as_sf()
sfhas_or <- nfhl_or %>% filter(sfha == 1)
sfha_nfip_or <- ggplot(data = community_status_map_df %>% filter(state == "OR")) +
    geom_sf(aes(fill = factor(participatingInNFIP)), color = NA) +
    geom_sf(data = or_state, fill = NA, color = small_pal[6]) +
    scale_fill_manual(values = c("1" = small_pal[1], "0" = small_pal[4], "Required" = small_pal[3]),
                      labels = c("1" = "Optional", "0" = "Unavailable", "Required" = "Required")) +
    geom_sf(data = sfhas_or, aes(fill = "Required"), color = NA) +
    theme_void() +
    theme(panel.grid = element_blank(),
          axis.text.y = element_blank(),
          axis.text.x = element_blank(),
          axis.ticks = element_blank(),
          legend.position = "none",
          plot.title = element_text(hjust = 0.5),
          legend.title = element_blank())


sfha_nfip_state <- sfha_nfip_wa + sfha_nfip_id + sfha_nfip_or
pdf("plots/aim1_nfip_state.pdf", width = 4, height = 1.5)
sfha_nfip_state
dev.off()







# Combine all state boundaries into a single dataset
pnw_states <- states %>% filter(state %in% c("WA", "OR", "ID"))

# Combine all SFHA data
sfha_combined <- bind_rows(
  nfhl_wa %>% mutate(state = "WA"),
  nfhl_id %>% mutate(state = "ID"),
  nfhl_or %>% mutate(state = "OR")
) %>%
  filter(sfha == 1)

# Combine NFIP data for the three states
nfip_combined <- community_status_map_df %>% filter(state %in% c("WA", "OR", "ID"))

# Create a unified map
sfha_nfip_pnw <- ggplot() +
  # Plot NFIP participation
  geom_sf(data = nfip_combined, aes(fill = factor(participatingInNFIP)), color = NA) +
  # Plot state boundaries
  geom_sf(data = pnw_states, fill = NA, color = "#000000") +
  # Plot SFHA zones
  geom_sf(data = sfha_combined, aes(fill = "Required"), color = NA, alpha = 0.6) +
  # Define fill colors
  scale_fill_manual(
    values = c("1" = small_pal[1], "0" = small_pal[4], "Required" = small_pal[6]),
    labels = c("1" = "Optional", "0" = "Unavailable", "Required" = "Required")
  ) +
  # Add theme settings
  theme_void() +
  theme(
    panel.grid = element_blank(),
    axis.text.y = element_blank(),
    axis.text.x = element_blank(),
    axis.ticks = element_blank(),
    legend.position = "bottom",
    plot.title = element_text(hjust = 0.5),
    legend.title = element_blank(),
    legend.text = element_text(size = 14)
  ) 

# Save as PDF
pdf("plots/aim1_nfip_pnw.pdf", width = 8, height = 6)
sfha_nfip_pnw
dev.off()

ggsave("plots/aim1_nfip_pnw.png", sfha_nfip_pnw, width = 8, height = 6, dpi = 300)
