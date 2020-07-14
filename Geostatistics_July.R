rm(list=ls(all=TRUE))
getwd()
setwd("C:/Users/HP/Documents/R/PUMA Morphostructure")
library(rgeos)
library(rgdal)
library(gstat)
library(geoR)
library(sgeostat)
library(rgdal)

#https://stackoverflow.com/questions/16607532/error-opening-shp-file-in-r-using-maptools-readshapepoly
#puma<-readOGR("C:/Users/HP/Documents/R/PUMA Morphostructure", "polygoncorrected")
#puma$Nom_Clase <- factor(puma$Nom_Clase)
#names(puma)
#proj4string(puma) <- CRS("+init=epsg:4326")
#proj4string(puma) <- CRS("+proj=longlat +datum=WGS84")
#crs_data = rgdal::make_EPSG()
#View(crs_data)
#https://geocompr.robinlovelace.net/spatial-class.html
#https://r-spatial.github.io/sf/articles/sf3.html
#Proj4js.defs["EPSG:4326"] = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs";
#tmap_mode("view")
#tm_shape(puma)+tm_dots(col = "Nom_Clase")

require(rgdal)
# https://gis.stackexchange.com/questions/151613/reading-feature-class-in-file-geodatabase-using-r
# The input file geodatabase
fgdb <- "C:/Users/HP/Documents/R/PUMA Morphostructure/PUMAfullolygon.gdb"

# List all feature classes in a file geodatabase
subset(ogrDrivers(), grepl("GDB", name))
fc_list <- ogrListLayers(fgdb)
print(fc_list)

# Read the feature class
fc <- readOGR(dsn=fgdb,layer="polyer_chk_1")

# Determine the FC extent, projection, and attribute information
summary(fc)
#https://rdrr.io/cran/sf/man/valid.html
require(sf)
fc <- sf::st_read("C:/Users/HP/Documents/R/PUMA Morphostructure/PUMAfullolygon.gdb", layer = "polyer_chk_1")
st_is_valid(fc, NA_on_exception = TRUE, reason = FALSE)
fc1<-st_make_valid(fc)
st_is_valid(fc1)
# View the feature class
plot(fc1)
tmap_mode("view")
tm_shape(fc1)+ tm_polygons(col="Nom_Clase")
tm_shape(fc1)+ tm_dots(col = "Nom_Clase")

library(maptools)
library(rgeos)
#https://gis.stackexchange.com/questions/43543/how-to-calculate-polygon-centroids-in-r-for-non-contiguous-shapes
sp_cent <- gCentroid(as(fc1, "Spatial"), byid = TRUE)
sf_cent <- st_centroid(fc1)
ggplot() + 
  geom_sf(data = fc1, fill = 'white') +
  geom_sf(data = sp_cent %>% st_as_sf, color = 'blue') + 
  geom_sf(data = sf_cent, color = 'red') 

library(dbscan)
coordinates_sf_cent <- sf_cent  %>% st_coordinates()  # we only need a matrix of the actual coordinates for now
kNNdistplot(coordinates_sf_cent, k = 30)

sf_cent$geographical_cluster <- dbscan(coordinates_sf_cent, eps = 0.01, minPts = 30) %>% 
  pluck('cluster') %>% as.character()

tmap_mode("plot")
sf_cent %>% 
  tm_shape() + tm_text(text = "geographical_cluster", col = "Nom_Clase")


sf_cent_attributes <- sf_cent %>% 
  select(Aream2, Area) %>% 
  st_set_geometry(NULL) %>% 
  scale()

library(factoextra)
fviz_nbclust(sf_cent_attributes, kmeans, method = "wss")
kmeans(sf_cent_attributes, centers = 2, nstart = 50) %>% 
  fviz_cluster(data = sf_cent_attributes)

sf_cent$attributes_cluster <- kmeans(sf_cent_attributes, centers = 3, nstart = 50) %>% 
  pluck('cluster') %>% as.character()
tmap_mode("plot")
sf_cent %>% 
  tm_shape() + tm_text(text = "attributes_cluster")

hex_grid <- fc1 %>%
  st_make_grid(st_bbox(.), square = FALSE, cellsize = 0.00060) %>% 
  st_sf() %>% 
  mutate(hex_id = row_number())

tm_shape(hex_grid) + tm_polygons()

benthic_hex <- st_join(fc1, hex_grid) %>% 
  st_set_geometry(NULL)

pol_ext <- benthic_hex %>% 
  group_by(hex_id) %>% 
  summarise(pol_ext = mean(Aream2)) %>% 
  left_join(hex_grid, .) %>% 
  filter(pol_ext > 0)

tm_shape(pol_ext) + tm_polygons(col = "pol_ext")

library(spdep)

hex_sp <- as(pol_ext, 'Spatial')
hex_neighbors <- poly2nb(hex_sp)
## visually inspect neighbors
plot(hex_neighbors, coordinates(hex_sp))
cluster_data <- pol_ext %>% 
  st_set_geometry(NULL) %>%
  select(pol_ext)

# For each edge between neighbors we calculate the (dis)similarity or 'cost' in 'data space'
hex_edge_costs <- nbcosts(hex_neighbors, cluster_data)
# we add the calculated costs to neighbor list
hex_edge_weights <- nb2listw(hex_neighbors, hex_edge_costs, style="B")
# based on the weighted neighbor list we calculate a minimum spanning tree
hex_mstree <- mstree(hex_edge_weights)
plot(hex_mstree, coordinates(hex_sp))

## LAGGED AVERAGE OVER NEIGHBOURS

hex_sp <- as(pol_ext, 'Spatial')
hex_neighbours <- poly2nb(hex_sp)
plot(hex_neighbours, coordinates(hex_sp))

hex_weights <- nb2listw(hex_neighbours, style="W", zero.policy=TRUE)
lease_lag <- lag.listw(hex_weights, hex_sp$pol_ext)

lease_lag_df <- pol_ext %>%
  add_column(lease_lag = lease_lag)

current.mode <- tmap_mode("plot")
tm_shape(lease_lag_df) +
  tm_polygons(col = "lease_lag")
## moran´s 
ggplot(lease_lag_df) +
  geom_point(aes(x = pol_ext, y = lease_lag))

moran_manual <- lm(lease_lag ~ pol_ext, data=lease_lag_df)
moran_manual
# https://gis.stackexchange.com/questions/99660/interpreting-morans-i-results
moran_local <- localmoran(lease_lag_df$pol_ext, hex_weights)
lease_lag_df$moran <- moran_local[,1]
tmap_mode("view")
tm_shape(lease_lag_df) +
  tm_polygons(col = "moran", palette = "-RdBu", midpoint = NA)
