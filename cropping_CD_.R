library(gdistance)
library(raster)

#LOAD in data
r1 <- "C:/Users/rkopp/Desktop/R_work/Buna/srtm/srtm_44_11.tif"
r <- raster(r1)
plot(r)
plot(gadm36_ETH_2_sp, add = TRUE)

#crop down the DEM raster and border .shpfile

#Woredas
gadm36_ETH_2_sp <- readRDS("C:/Users/rkopp/Desktop/R_work/Buna/gadm36_ETH_2_sp.rds")
keff_cropped <- crop(x = gadm36_ETH_2_sp ,
                       extent(35.42, 36.80, 6.22, 8.10))
plot(keff_cropped)


r_cropped <- crop(x = r ,
                  extent(35.42, 36.80, 6.22, 8.10))


par(mar = c(0, 0, 0, 0))
plot(r_cropped)
plot(keff_cropped, add = TRUE)

#Kabeles--cropping to zoom down to mainly decha
gadm36_ETH_3_sp <- readRDS("C:/Users/rkopp/Desktop/R_work/Buna/gadm36_ETH_3_sp.rds")

Dech_crop <- crop(x = gadm36_ETH_3_sp ,
                  extent(35.49, 36.5, 6.22, 7.56))

par(mar = c(0, 0, 0, 0))
plot(Dech_crop)


r_crop_dech <- crop(x = r ,
                  extent(35.49, 36.5, 6.22, 7.56))


par(mar = c(0, 0, 0, 0))
plot(r_crop_dech)
plot(Dech_crop, add = TRUE)

#Interviews aka "farmers"

farmers <- st_read("C:/Users/rkopp/Desktop/R_work/Cost_Distance_R/Decha_Transect_Points/Decha_Transect_Points.shp")

plot(farmers, col = "black", add = TRUE)



#roads <- st_read("C:/Users/rkopp/Desktop/R_work/Buna/ethiopia-latest-free_OS/gis_osm_roads_free_1.shp")

#plot(roads, add = TRUE)

#-----------------------------------------------------------------------------------
#USING EXAMPLE FROM GDISTANCE VIGNETTE


#calculate the altitudinal differences between cells.

altDiff <- function(x){x[2] - x[1]}
hd <- transition(r_crop_dech, altDiff, 8, symm=FALSE)



#Then use the geoCorrection function to divide by the distance between cells.
slope <- geoCorrection(hd)


adj <- adjacent(r_crop_dech, cells=1:ncell(r_crop_dech), pairs=TRUE, directions=8)
speed <- slope
speed[adj] <- 6 * exp(-3.5 * abs(slope[adj] + 0.05))

Conductance <- geoCorrection(speed)


#example points point 1 = A, point 8 = B

A <- c(36.22489, 7.178127)

B <- c(36.18278, 7.221531) #(point 8)

AtoB <- shortestPath(Conductance, A, B, output="SpatialLines")
BtoA <- shortestPath(Conductance, B, A, output="SpatialLines")

plot(r_crop_dech)
lines(AtoB, col="red", lwd=2)
lines(BtoA, col="blue")
text(A[1] - 10, A[2] - 10, "A")
text(B[1] + 10, B[2] + 10, "B")
plot(Dech_crop, add = TRUE)


