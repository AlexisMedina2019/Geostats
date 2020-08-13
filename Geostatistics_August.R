rm(list=ls(all=TRUE))
getwd()
setwd("C:/Users/HP/Documents/R/PUMA Morphostructure")
#https://mgimond.github.io/Spatial/spatial-autocorrelation-in-r.html
#https://02522-cua.github.io/lecturenotes/spatial-clustering.html#spatially-constrained-clustering
pumamorphostr=read.csv("PUMA Morphostructure.csv")
benthic=read.csv("BENTHIC_CSV.csv")
pumamorphostr$poligon=factor(pumamorphostr$poligon, levels = c('S','C','N'))
require(wesanderson)
str(benthic)
require(ggplot2)
ggplot(pumamorphostr) +
  aes( y = length, fill = zone) +
  geom_boxplot() +
  scale_y_reverse()+
  geom_hline(yintercept=0, linetype="dashed", 
             color = "red", size=1.2)+
  scale_fill_manual(values = wes_palette(n=3, name="Cavalcanti1"))+
  theme(axis.title.y = element_text(face="bold", colour="black", size=rel(1.2)))+
  theme(axis.text.y = element_text(face="bold", colour="black", size=rel(1.2), angle=90, hjust=0.5))+
  theme_bw()+
  facet_wrap(vars(poligon))+
  theme(strip.text = element_text(face = "bold", size=rel(1)))+
  labs(y = "Slope-breaks distance to Crest-line [m]")+
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank())+
  theme (axis.text.y = element_text(face ="bold", colour="black", size=rel(1.2), angle=90, hjust=0.5))+
  theme (axis.title = element_text(face="bold", colour="black", size=rel(1.2)))

benthic$sector=factor(benthic$sector, levels = c('S','C','N'))
benthic$zone=factor(benthic$zone, levels = c('BR','DT','RF','CG'))

ggplot(benthic) +
  aes(x = sector, fill = Clase, weight = Area) +
  geom_bar() +
  scale_fill_manual(values = wes_palette(n=9, name="Royal1",type = "continuous"))+
  theme_bw() +
  theme(axis.title.x = element_text(face="bold", colour="black", size=rel(1.2)))+
  theme(axis.text.x = element_text(face="bold", colour="black", size=rel(1.2), hjust=0.5))+
  theme(axis.title.y = element_text(face="bold", colour="black", size=rel(1.2)))+
  theme(axis.text.y = element_text(face="bold", colour="black", size=rel(1.2), angle=90, hjust=0.5))+
  theme(strip.text = element_text(face = "bold", size=rel(1)))+
  facet_wrap(vars(zone))

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

#require(rgdal)
# https://gis.stackexchange.com/questions/151613/reading-feature-class-in-file-geodatabase-using-r
# The input file geodatabase
fgdb <- "C:/Users/HP/Documents/R/PUMA Morphostructure/Agosto from QGIS/Geodatabase.gdb"

# List all feature classes in a file geodatabase
subset(ogrDrivers(), grepl("GDB", name))
fc_list <- ogrListLayers(fgdb)
print(fc_list)

# Read the feature class
fc <- readOGR(dsn=fgdb,layer="unica_pol_centrxyz_m2puma")

# Determine the FC extent, projection, and attribute information
summary(fc)
#https://rdrr.io/cran/sf/man/valid.html
require(sf)
fc <- sf::st_read("C:/Users/HP/Documents/R/PUMA Morphostructure/Agosto from QGIS/Geodatabase.gdb", layer = "unica_pol_centrxyz_m2puma")

#st_is_valid(fc, NA_on_exception = TRUE, reason = FALSE)
fc1<-st_make_valid(fc)
st_is_valid(fc1)
fc1$Nom_Clase
table(fc1$Nom_Clase)
levels(fc1$Nom_Clase) <- c('seagrass_comm','corals', 'macroalgae', 'No_data','octocorals', 'octocorals&corals','seagrass&macroalg','sediments','stump&boulders')
table(fc1$Nom_Clase)
# View the feature class
plot(fc1$cover)
plot(fc1$Name)
#plot(fc1,title="",xlab="Longitude",ylab="Latitude",asp=1/cos(45*pi/180))
#require(RGeostats)
require(tmap)
require(maptools)#Tools for Reading and Handling Spatial Objects
#https://www.youtube.com/watch?v=qssJ3DHQe-I
##https://thinking-spatial.org/courses/angewandte_geodatenverarbeitung/kurs06/
tmap_mode("view")
tm_shape(fc1)+ tm_polygons(col="Name")
tm_shape(fc1)+ tm_polygons(col="Nom_Clase")
tm_shape(fc1)+ tm_polygons(col="cover")

tmap_mode("plot")
tm_shape(fc1) + tm_fill(col = "cover", palette = "Oranges", style = "jenks", 
                                 title = "coverage", 
                                 legend.hist = TRUE) + 
  tm_borders(alpha= .2, col = "black") + 
  tm_compass() +
  tm_grid(n.x = 3, n.y = 4,lines = FALSE) +
  tm_scale_bar() +
  tm_credits("Data: CONABIO benthic coverage transf.", size = 0.7, position=c("left","bottom"))+
  tm_layout(title = NA,  legend.outside= FALSE, 
            legend.position = c("right", "center"),
            attr.outside= FALSE,  attr.position= c("RIGHT", "BOTTOM"), 
            legend.frame = FALSE, legend.width = -0.25, legend.hist.width = 0.3, legend.hist.height = 0.24,
            inner.margins = c(0.07, 0.03, 0.01, 0.27),
            frame= TRUE,
            design.mode= FALSE)

library('viridisLite')
vir <- viridis(3)
tm_shape(fc1)+ tm_dots(col = "Nom_Clase", size=0.25)+
  tm_grid(n.x = 3, n.y = 4,lines = FALSE)+ 
  tm_compass() +
  tm_scale_bar ()+
  tm_layout( attr.position= c("RIGHT", "BOTTOM"))
tm_shape(fc1)+ tm_dots(col="cover", size=0.15, palette=vir)+
  tm_grid(n.x = 3, n.y = 4,lines = FALSE)+ 
  tm_compass() +
  tm_scale_bar ()+
  tm_layout( attr.position= c("RIGHT", "BOTTOM"))

ggplot(fc1) +
  aes(x = Name_2, fill = Nom_Clase, weight = aream2 ) +
  geom_bar(width=.7, position=position_dodge2(width=.7)) +
  scale_fill_hue() +
  scale_y_continuous(trans = "log10") +
  theme_minimal()



ggplot(fc1) +
  aes(x = cover, fill = Name, weight = aream2) +
  geom_bar(position = "dodge") +
  scale_fill_hue() +
  scale_y_continuous(trans = "log") +
  theme_minimal()

require(rgeos)
#https://gis.stackexchange.com/questions/43543/how-to-calculate-polygon-centroids-in-r-for-non-contiguous-shapes
sp_cent <- gCentroid(as(fc1, "Spatial"), byid = TRUE)
sf_cent <- st_centroid(fc1)
require(ggplot2)
ggplot() + 
  geom_sf(data = fc1, fill = 'white') +
  geom_sf(data = sp_cent %>% st_as_sf, color = 'blue') + 
  geom_sf(data = sf_cent, color = 'red', size=0.25) 

require(dbscan)
## DBSCAN density-based clustering ALGORITHM of x,y location
coordinates_sf_cent <- sf_cent  %>% st_coordinates()  # we only need a matrix of the actual coordinates for now
coordinates_sf_cent_m <-project(coordinates_sf_cent, "+proj=utm +zone=16 ellps=WGS84")
kNNdistplot(coordinates_sf_cent_m, k = 40)
distances <-kNNdist(coordinates_sf_cent_m, k = 40)
summary(distances)
kNNdistplot(coordinates_sf_cent_m, k = 400)
distances2 <-kNNdist(coordinates_sf_cent_m, k = 400)
summary(distances2)
##k=30
require(purrr)
sf_cent$geographical_cluster <- dbscan(coordinates_sf_cent_m, eps = 150, minPts = 30) %>% 
  pluck('cluster') %>% as.character()
require(RColorBrewer)
require(tmaptools)

tmap_mode("plot")
sf_cent %>% 
  tm_shape() + tm_text(text = "geographical_cluster", col = "geographical_cluster", size= 0.75, palette = vir) + 
  tm_compass() +
  tm_grid(n.x = 3, n.y = 4,lines = FALSE) +
  tm_scale_bar() +
  tm_credits("Data: CONABIO", size = 0.7, position=c("left","bottom"))+
  tm_layout(title = NA,  legend.outside= FALSE, 
            legend.position = c("right", "center"),
            attr.outside= FALSE,  attr.position= c("RIGHT", "BOTTOM"), 
            legend.frame = FALSE, legend.width = -0.25,
            inner.margins = c(0.07, 0.03, 0.01, 0.27),
            frame= TRUE,
            design.mode= FALSE)
##  (0 is the 'cluster' with outliers)
require(dplyr)
### INTENTAR CREAR UNA CLASE NUMERICA PARA DEFINIR
#,Nom_Clase (benthic clases)
#,Name (polygono)
sf_cent_attributes <- sf_cent %>% 
  select(aream2, ID_Clase) %>% 
  st_set_geometry(NULL) %>% 
  scale()
## Area, FID_Polygo,
require(factoextra)
##check the elbow to choose number of clusters 
fviz_nbclust(sf_cent_attributes, kmeans, method = "wss")
## within-cluster sum of square (WSS)
## from the graphic, optimal number results 4
vir2 <- viridis(4)
a<- kmeans(sf_cent_attributes, centers = 4, nstart = 50) %>% 
  fviz_cluster(data = sf_cent_attributes, palette=vir2)
a
kmeans(sf_cent_attributes, centers = 4, nstart = 50) %>% 
  fviz_cluster(data = sf_cent_attributes, palette=vir2)



sf_cent$attributes_cluster <- kmeans(sf_cent_attributes, centers = 4, nstart = 50) %>% 
  pluck('cluster') %>% as.character()
tmap_mode("plot")
sf_cent %>% 
  tm_shape() + tm_text(text = "attributes_cluster", col = "attributes_cluster", size= 0.75, palette = vir2) + 
  tm_compass() +
  tm_scale_bar() +
  tm_credits("Data: Benthic classes", size = 0.7, position=c("left","bottom"))+
  tm_layout(title = NA,  legend.outside= FALSE, 
            legend.position = c("right", "center"),
            attr.outside= FALSE,  attr.position= c("RIGHT", "BOTTOM"), 
            legend.frame = FALSE, legend.width = -0.25,
            legend.text.fontface = 0.4,
            inner.margins = c(0.07, 0.03, 0.01, 0.27),
            frame= TRUE,
            design.mode= FALSE)+
            tm_facets (by="Nom_Clase")
sf_cent %>% 
  tm_shape() + tm_text(text = "attributes_cluster", col = "attributes_cluster", size= 0.75, palette = vir2) + 
  tm_compass() +
  tm_scale_bar() +
  tm_credits("Data: Coverage", size = 0.7, position=c("left","bottom"))+
  tm_layout(title = NA,  legend.outside= FALSE, 
            legend.position = c("right", "center"),
            attr.outside= FALSE,  attr.position= c("RIGHT", "BOTTOM"), 
            legend.frame = FALSE, legend.width = -0.25,
            legend.text.fontface = 0.4,
            inner.margins = c(0.07, 0.03, 0.01, 0.27),
            frame= TRUE,
            design.mode= FALSE)+
            tm_facets (by="cover")
tmap_mode("plot")
sf_cent %>% 
  tm_shape() + tm_text(text = "attributes_cluster", col = "attributes_cluster", size= 0.75, palette = vir2) + 
  tm_compass() +
  tm_scale_bar() +
  tm_credits("Data: Attributes cluster groups", size = 0.7, position=c("left","bottom"))+
  tm_layout(title = NA,  legend.outside= FALSE, 
            legend.position = c("right", "center"),
            attr.outside= FALSE,  attr.position= c("RIGHT", "BOTTOM"), 
            legend.frame = FALSE, legend.width = -0.25,
            inner.margins = c(0.07, 0.03, 0.01, 0.27),
            frame= TRUE,
            design.mode= FALSE)+
            tm_facets (by="attributes_cluster")
  tmap_mode("view")

#The results of the k-means algorithm might be a little more interpretable
#and they clearly show some specific geographic variation 
#as well despite the fact that geographic space hasn't been considered 
#explicitly in the decisions of the algorithm. 
#However, if we were to draw  discrete spatial sub-units withing the structure,
#migration(for example, as result of NE>>SW drifting hypthesis of reef development

###But this wouldn't necessarily be possible yet.
###For this process - often referred to as regionalization 
###- it can be useful to combine both spatial 
###and non-spatial characteristics of the area 

##WHY EXAGONS
##https://pro.arcgis.com/en/pro-app/tool-reference/spatial-statistics/h-whyhexagons.htm
##https://doi.org/10.1016/j.ecolmodel.2007.03.041
#AGGREGATION TO LARGER SPATIAL UNITS
##GLOBAL FULL SPATIAL ANALYSIS 
hex_grid <- fc1 %>%
  st_make_grid(st_bbox(.), square = FALSE, cellsize = 0.000150) %>% 
  st_sf() %>% 
  mutate(hex_id = row_number())

tm_shape(hex_grid) + tm_polygons()
#MATRIX HEXAGONAL
benthic_hex <- st_join(fc1, hex_grid) %>% 
  st_set_geometry(NULL)

pol_ext <- benthic_hex %>% 
  group_by(hex_id) %>% 
  summarise(pol_ext = mean(aream2)) %>% 
  left_join(hex_grid, .) %>% 
  filter(pol_ext > 0)

tm_shape(pol_ext) + tm_polygons(col = "pol_ext")

require(spdep)#Weighting Schemes, Statistics and Models
hex_sp <- as(pol_ext, 'Spatial')
hex_neighbors <- poly2nb(hex_sp, queen = TRUE)
## visually inspect neighbors
##plot(hex_neighbors, coordinates(hex_sp))
cluster_data <- pol_ext %>% 
 st_set_geometry(NULL) %>%
 select(pol_ext)

# For each edge between neighbors we calculate the (dis)similarity or 'cost' in 'data space'
hex_edge_costs <- nbcosts(hex_neighbors, cluster_data)
# we add the calculated costs to neighbor list
hex_edge_weights <- nb2listw(hex_neighbors, hex_edge_costs, style="B")
# based on the weighted neighbor list we calculate a minimum spanning tree
hex_mstree <- mstree(hex_edge_weights)
#plot(hex_mstree, coordinates(hex_sp))

## LAGGED AVERAGE OVER NEIGHBOURS

hex_sp <- as(pol_ext, 'Spatial')
hex_neighbours <- poly2nb(hex_sp)
#plot(hex_neighbours, coordinates(hex_sp))

hex_weights <- nb2listw(hex_neighbours, style="W", zero.policy=TRUE)
summary(hex_weights)



lease_lag <- lag.listw(hex_weights, hex_sp$pol_ext)
require(tibble)
lease_lag_df <- pol_ext %>%
  add_column(lease_lag = lease_lag)

current.mode <- tmap_mode("view")
tm_shape(lease_lag_df) +
  tm_polygons(col = "lease_lag")
## moran?s XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
## 
moran_sp <- moran.test(lease_lag, hex_weights,randomisation=TRUE, alternative="two.sided",
                            na.action=na.exclude)
 moran_sp
 
#Grafica de diagrama de dispersión de Moran
moran.plot(lease_lag, hex_weights, pch=20)
 
 
 
ggplot(lease_lag_df,
       aes(x = pol_ext, y = lease_lag)) +
  geom_point() +
  geom_smooth(method = lm, se = FALSE)

moran_manual <- lm(lease_lag ~ pol_ext, data=lease_lag_df) 
moran_manual
# https://gis.stackexchange.com/questions/99660/interpreting-morans-i-results
moran_local <- localmoran(lease_lag_df$pol_ext, hex_weights)
lease_lag_df$moran <- moran_local[,1]
tmap_mode("view")
tm_shape(lease_lag_df) +
  tm_polygons(col = "moran", palette = "-RdBu", midpoint = NA)







##PARA EL ANALISIS DE UNA PARTE DE LOS POLIGONOS, dejando solo el substrato expuesto
bare_s<-sf::st_read("C:/Users/HP/Documents/R/PUMA Morphostructure/Agosto from QGIS/Geodatabase.gdb", layer = "unica_pol_centrxyz_m2puma")%>% 
   filter(!(cover == "life cover" ))
st_crs(bare_s)
bare_s1<-st_make_valid(bare_s)
## SI SE NECESTARA MAS DE UN FILTRO>> | OBJECTID == 18
hex_bare <- bare_s1 %>%
  st_make_grid(st_bbox(.), square = FALSE, cellsize = 0.000150) %>% 
  st_sf() %>% 
  mutate(hex_id = row_number())

tm_shape(hex_bare) + tm_polygons()

bare_hex <- st_join(bare_s1, hex_bare) %>% 
  st_set_geometry(NULL)

polbare_ext <- bare_hex %>% 
  group_by(hex_id) %>% 
  summarise(pol_ext = mean(aream2)) %>% 
  left_join(hex_bare, .) %>% 
  filter(polbare_ext > 0)

tm_shape(polbare_ext) + tm_polygons(col = "pol_ext")

require(spdep)
hex_bare <- as(polbare_ext, 'Spatial')
hex_neighbors_bare <- poly2nb(hex_bare)
#aqui nos quedamos
#Error: Must subset columns with a valid subscript vector.
#x Subscript has the wrong type `sf<
  #hex_id  : integer
#pol_ext : double
#geometry: sfc_POLYGON
#>`.
#i It must be numeric or character.
#Run `rlang::last_error()` to see where the error occurred.
cluster_databare <- bare_s %>% 
  st_set_geometry(NULL) %>%
  select(all_of(bare_s$ID_Clase))
##XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
# For each edge between neighbors we calculate the (dis)similarity or 'cost' in 'data space'
hex_edge_costs_bare <- nbcosts(hex_neighbors_bare, cluster_databare)
# nbcosts: no-neighbour nodes
#we add the calculated costs to neighbor list
hex_edge_weights_bare <- nb2listw(hex_neighbors_bare, hex_edge_costs_bare, style="B")
# based on the weighted neighbor list we calculate a minimum spanning tree
hex_mstree_bare <- mstree(hex_edge_weights_bare)
plot(hex_mstree_bare, coordinates(hex_bare))
##XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
## LAGGED AVERAGE OVER NEIGHBOURS
plot(hex_neighbors_bare, coordinates(hex_bare))
hex_weights_bare <- nb2listw(hex_neighbors_bare, style="W", zero.policy=TRUE)
hex_weights_bare
lease_lag2 <- lag.listw(hex_weights_bare, hex_bare$pol_ext)
require(tibble)
lease_lag_baredf <- polbare_ext %>%
  add_column(lease_lag = lease_lag2)
#############
current.mode <- tmap_mode("plot")
tm_shape(lease_lag_baredf) +
  tm_polygons(col = "lease_lag")
## moran?s 
ggplot(lease_lag_baredf) +
  geom_point(aes(x = polbare_ext, y = lease_lag2))

moran_manual <- lm(lease_lag_baredf ~ polbare_ext, data=lease_lag)
moran_manual
# https://gis.stackexchange.com/questions/99660/interpreting-morans-i-results
moran_local <- localmoran(lease_lag_df$pol_ext, hex_weights)
lease_lag_df$moran <- moran_local[,1]
tmap_mode("view")
tm_shape(lease_lag_df) +
  tm_polygons(col = "moran", palette = "-RdBu", midpoint = NA)























##PARA EL ANALISIS DE UNA PARTE DE LOS POLIGONOS, dejando solo las comunidades benthicas
life_c<-sf::st_read("C:/Users/HP/Documents/R/PUMA Morphostructure/Agosto from QGIS/Geodatabase.gdb", layer = "unica_pol_centrxyz_m2puma")%>% 
  filter(!(cover == "bare substrate" ))
## SI SE NECESTARA MAS DE UN FILTRO>> | OBJECTID == 18
hex_life <- life_c %>%
  st_make_grid(st_bbox(.), square = FALSE, cellsize = 0.000150) %>% 
  st_sf() %>% 
  mutate(hex_id = row_number())

tm_shape(hex_life) + tm_polygons()

life_hex <- st_join(life_c, hex_life) %>% 
  st_set_geometry(NULL)

polife_ext <- life_hex %>% 
  group_by(hex_id) %>% 
  summarise(polife_ext = mean(aream2)) %>% 
  left_join(hex_life, .) %>% 
  filter(polife_ext > 0)

tm_shape(polife_ext) + tm_polygons(col = "polife_ext")











require(spdep)

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
#hex_weights
lease_lag <- lag.listw(hex_weights, hex_sp$pol_ext)
require(tibble)
lease_lag_df <- pol_ext %>%
  add_column(lease_lag = lease_lag)

current.mode <- tmap_mode("plot")
tm_shape(lease_lag_df) +
  tm_polygons(col = "lease_lag")
## moran?s 
ggplot(lease_lag_df) +
  geom_point(aes(x = pol_ext, y = lease_lag))
#XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
ggplot(lease_lag_df,
       aes(x = polbare_ext, y = lease_lag)) +
  geom_point() +
  geom_smooth(method = lm, se = FALSE)

moran_manual <- lm(lease_lag ~ pol_ext, data=lease_lag_df)
moran_manual
# https://gis.stackexchange.com/questions/99660/interpreting-morans-i-results
moran_local <- localmoran(lease_lag_df$pol_ext, hex_weights)
lease_lag_df$moran <- moran_local[,1]
tmap_mode("view")
tm_shape(lease_lag_df) +
  tm_polygons(col = "moran", palette = "-RdBu", midpoint = NA)

#http://www.bias-project.org.uk/ASDARcourse/unit4_slides.pdf

#http://rgeostats.free.fr/download.php
#file:///C:/Users/HP/Downloads/Handbook%20ofgeostatistics%20marine%20ecology.pdf
#require(RGeostats)

