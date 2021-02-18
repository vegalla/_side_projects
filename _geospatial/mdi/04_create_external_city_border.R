library(ggplot2)
library(sp)
library(sf)
library(rgdal)
library(rgeos)
library(broom)
library(purrr)

source("02_Code/02_San_Diego/02_Cleaning_coding_and_merging/02_load_and_merge_data.R")

# Load council boundaries as spatial object (using rgdal)
cncl_ogr <- readOGR(get_path("/Shared drives/OES data 2003/Raw_Data/Council_Districts_Map/council_districts_datasd.shp"))

# Plot council boundaries spatial object
plot(cncl_ogr)

# Create external border shape file (using rgeos)
sd_border_ogr <- gBuffer(cncl_ogr, width = 1, quadsegs = 1)

# Plot external border in red
plot(sd_border_ogr, border= "red", add= TRUE) # Convert this to ggplot

# Convert to long-lat
sd_border_ogr <- spTransform(sd_border_ogr, CRS("+proj=longlat +datum=WGS84"))

# Grab points of boundaries as tibble (using broom) ----------------------------
sd_border_pts <- tidy(sd_border_ogr) 

# Plot Border Points
ggplot(sd_border_pts, aes(x= long, y= lat)) +
  geom_point()

# Limit to main external border ------------------------------------------------
sd_border_pts.1 <- sd_border_pts[sd_border_pts$piece == 1,]

# Plot main external border
ggplot(sd_border_pts.1, aes(x= long, y= lat)) +
  geom_point() +
  xlim(-117.3, -116.9) +
  ylim(32.5, 33.13) 

# Limiting sea & country borders; Flag for deletion ----------------------------
sd_border_pts.1 = sd_border_pts.1 %>%
  mutate(border_sea    = ifelse((long < -117.12) & (lat < 32.94), 1, 0),
         border_mexico = ifelse((long < -116.92) & (long > -117.117) & (lat < 32.552), 1, 0),
         border = pmax(border_sea, border_mexico))

# Plot Border Points
ggplot(sd_border_pts.1, aes(x= long, y= lat, color= border)) +
  geom_point() +
  xlim(-117.3, -116.9) +
  ylim(32.5, 33.13)

# Evenly spaced points; Flag irrelevant points for deletion --------------------

# Function for Euclidean distance
euclid_dist <- function(lat_1, lon_1, lat_2, lon_2) sqrt((lat_2 - lat_1)^2 + (lon_2 - lon_1)^2) ## Add conversion to a conventional distance

sd_border_pts.1 <- sd_border_pts.1 %>% 
  mutate(distance= euclid_dist(lat, long, lag(lat), lag(long)))

sd_border_pts.1[1, "distance"] = euclid_dist(sd.border_pts.1[1, "lat"], sd.border_pts.1[1, "long"], sd.border_pts.1[nrow(sd.border_pts.1), "lat"], sd.border_pts.1[nrow(sd.border_pts.1), "long"])

# Function to reset cumulative sum at threshold (using purrr::accumulate)
sum_reset_at <- function(thresh) {function(x) {
    accumulate(x, ~if_else(.x>=thresh, .y, .x+.y))
  }  
}

# Calculate evenly spaced points and flag points between
sd_border_pts.1 = sd_border_pts.1 %>% 
  mutate(thresh_sum = sum_reset_at(0.01)(distance),
         even_pts   = ifelse(thresh_sum < lag(thresh_sum),1, 0)) %>%
  filter(even_pts == 1)

# Plot Border Points
ggplot(sd_border_pts.1, aes(x= long, y= lat, color= border)) +
  geom_point() +
  theme(legend.position= "none") +
  xlim(-117.3, -116.9) +
  ylim(32.5, 33.13)

