## Bettongia gaimardi species distribution models
## Corey Bradshaw
## June 2021


## remove everything
rm(list = ls())


# load the library
library(backports)
library(biomod2)
library(ggplot2)
library(rgdal)
library(ecospat)
library(usdm)
library(maptools)
library(tmap)
library(sf)
library(raster)
library(eechidna)
library(ncdf4)
library(chron)
library(readr)
library(mgcv)
library(GADMTools)

# load our species data
setwd("~/Bettongs/BGaimardi")

BGaimardi <- read_csv("BGaimardiOccur.csv")
#View(BGaimardi)
head(BGaimardi)

##++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##  STEP 1: FORMATTING THE PRESENCE ABSENCE DATA
##++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# remove extraneous points
data.tmp1 <- subset(BGaimardi, lat < -30) # set to south of -30 degrees - nsw, vic and tas
data.tmp2 <- subset(data.tmp1,  lon > 140) # set to east of 140 degrees - vic and tas
BGaimardi <- data.tmp2

gaimardi <- data.frame(as.numeric(BGaimardi$lon),as.numeric(BGaimardi$lat))
colnames(gaimardi) <- c("longitude","latitude")
plot(gaimardi$longitude, gaimardi$latitude, col = 'red', pch=4, cex=0.5)
range(gaimardi$latitude)
range(gaimardi$longitude)

presences <- data.frame(rep(1,dim(gaimardi)[1]))
colnames(presences) <- c("pres")
coordinates(gaimardi) <- c("longitude","latitude")
proj4string(gaimardi) <- CRS("+proj=longlat +datum=WGS84")
gaim.res <- spTransform(gaimardi, CRS("+proj=lcc +lat_1=-18 +lat_2=-36 +lat_0=0 +lon_0=134 +x_0=0 +y_0=0 +ellps=GRS80 +datum=WGS84 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"))
as(gaim.res, "SpatialPoints")
gaim.proj <- SpatialPointsDataFrame(coords=gaim.res, data=presences)

write.csv(gaim.proj,"GaimardiPresenceOnly_proj.csv")
BGaimardi.proj <- gaim.proj
plot(BGaimardi.proj, pch=3)

##++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##  STEP 2: FORMATTING THE ENVIRONMENTAL DATA
##++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# load the environmental raster layers (could be .img, ArcGIS # rasters or any supported format by the raster package)
# import full rasters

setwd("~/Bettongs/States")

## read original file and transform:
# wamap <- readOGR(dsn="wa.shp")
# plot(wamap)
# wa.proj <- spTransform(wamap, CRS("+proj=lcc +lat_1=-18 +lat_2=-36 +lat_0=0 +lon_0=134 +x_0=0 +y_0=0 +ellps=GRS80 +datum=WGS84 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"))
# writeOGR(wa.proj, ".", "wa.lambert", driver="ESRI Shapefile", )
## read transformed file:
wa.proj <- readOGR(dsn="wa.lambert.shp")
plot(wa.proj)

## read original file and transform:
# samap <- readOGR(dsn="sa.shp")
# plot(samap)
# sa.proj <- spTransform(samap, CRS("+proj=lcc +lat_1=-18 +lat_2=-36 +lat_0=0 +lon_0=134 +x_0=0 +y_0=0 +ellps=GRS80 +datum=WGS84 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"))
# writeOGR(sa.proj, ".", "sa.lambert", driver="ESRI Shapefile", )
## read transformed file:
sa.proj <- readOGR(dsn="sa.lambert.shp")
plot(sa.proj)

## read original file and transform:
# vicmap <- readOGR(dsn="vic.shp")
# plot(vicmap)
# vic.proj <- spTransform(vicmap, CRS("+proj=lcc +lat_1=-18 +lat_2=-36 +lat_0=0 +lon_0=134 +x_0=0 +y_0=0 +ellps=GRS80 +datum=WGS84 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"))
# writeOGR(vic.proj, ".", "vic.lambert", driver="ESRI Shapefile", )
## read transformed file:
vic.proj <- readOGR(dsn="vic.lambert.shp")
plot(vic.proj)

## read original file and transform:
# nswmap <- readOGR(dsn="nsw.shp")
# plot(nswmap)
# nsw.proj <- spTransform(nswmap, CRS("+proj=lcc +lat_1=-18 +lat_2=-36 +lat_0=0 +lon_0=134 +x_0=0 +y_0=0 +ellps=GRS80 +datum=WGS84 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"))
# writeOGR(nsw.proj, ".", "nsw.lambert", driver="ESRI Shapefile", )
## read transformed file:
nsw.proj <- readOGR(dsn="nsw.lambert.shp")
plot(nsw.proj)

## read original file and transform:
#  <- readOGR(dsn="tas.shp")
# plot(tasmap)
# tas.proj <- spTransform(tasmap, CRS("+proj=lcc +lat_1=-18 +lat_2=-36 +lat_0=0 +lon_0=134 +x_0=0 +y_0=0 +ellps=GRS80 +datum=WGS84 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"))
# writeOGR(tas.proj, ".", "tas.lambert", driver="ESRI Shapefile", )
## read transformed file:
tas.proj <- readOGR(dsn="tas.lambert.shp")
plot(tas.proj)

## read original file and transform:
# qldmap <- readOGR(dsn="qld.shp")
# plot(qldmap)
# qld.proj <- spTransform(qldmap, CRS("+proj=lcc +lat_1=-18 +lat_2=-36 +lat_0=0 +lon_0=134 +x_0=0 +y_0=0 +ellps=GRS80 +datum=WGS84 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"))
# writeOGR(qld.proj, ".", "qld.lambert", driver="ESRI Shapefile", )
## read transformed file:
qld.proj <- readOGR(dsn="qld.lambert.shp")
plot(qld.proj)

## read original file and transform:
# ntmap <- readOGR(dsn="nt.shp")
# plot(ntmap)
# nt.proj <- spTransform(ntmap, CRS("+proj=lcc +lat_1=-18 +lat_2=-36 +lat_0=0 +lon_0=134 +x_0=0 +y_0=0 +ellps=GRS80 +datum=WGS84 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"))
# writeOGR(nt.proj, ".", "nt.lambert", driver="ESRI Shapefile", )
## read transformed file:
nt.proj <- readOGR(dsn="nt.lambert.shp")
plot(nt.proj)

## Australia
## read original file and transform:
aus0 <- raster::getData('GADM', country='AUS', level=0) # country outline
aus0.proj <- spTransform(aus0, CRS("+proj=lcc +lat_1=-18 +lat_2=-36 +lat_0=0 +lon_0=134 +x_0=0 +y_0=0 +ellps=GRS80 +datum=WGS84 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"))
plot(aus0.proj)

## read original file and transform:
aus1 <- raster::getData('GADM' , country="AUS", level=1) # country outline with state outlines
aus1.proj <- spTransform(aus1, CRS("+proj=lcc +lat_1=-18 +lat_2=-36 +lat_0=0 +lon_0=134 +x_0=0 +y_0=0 +ellps=GRS80 +datum=WGS84 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"))
plot(aus1.proj)

# mean rainfall 1996-2005 (mm)
setwd("~/Bettongs/ASCII")


rain.asc <- read.asciigrid("m_rainfall.asc", as.image = FALSE, plot.image=T, proj4string = CRS(as.character("+proj=lcc +lat_1=-18 +lat_2=-36 +lat_0=0 +lon_0=134 +x_0=0 +y_0=0 +ellps=GRS80 +datum=WGS84 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs")), colname="RAIN")
rain.rst <- raster(rain.asc, layer=1, values=TRUE)
rain.rst1 <- crop(rain.rst, extent(aus0.proj))
plot(rain.rst1)
points(BGaimardi.proj, pch=3)
ext.dat <- extent(BGaimardi.proj)
plot(rain.rst1, ext=ext.dat)
points(BGaimardi.proj, pch=3)
plot(aus1.proj, add=T)


# mean annual temperature (Worldclim)
temp <- raster::getData('worldclim', var="tmean", res = 2.5)
tempmn <- calc(temp, fun=mean, na.rm=T)
plot(tempmn)
tempmn.real <- calc(tempmn, fun=function(x){x/10})
plot(tempmn.real)
temp.crop <- crop(tempmn.real, y=extent(aus0))
plot(temp.crop)
temp.rst <- projectRaster(temp.crop, crs=CRS("+proj=lcc +lat_1=-18 +lat_2=-36 +lat_0=0 +lon_0=134 +x_0=0 +y_0=0 +ellps=GRS80 +datum=WGS84 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"))
plot(temp.rst)  
ext.dat <- extent(BGaimardi.proj)
plot(temp.rst, ext=ext.dat)
points(BGaimardi.proj, pch=3)
plot(aus1.proj, add=T)
setwd("~/Bettongs/TIFF")
writeRaster(temp.rst, filename="temp.grd", format="raster", overwrite=T)

# temperature seasonality (Worldclim; SD of mean temperature from above)
tstack.crop <- crop(temp, y=extent(aus0))
tempsd <- calc(tstack.crop, fun=sd, na.rm=T)
plot(tempsd)
tempsd.real <- calc(tempsd, fun=function(x){x/10})
plot(tempsd.real)
seas.rst <- projectRaster(tempsd.real, crs=CRS("+proj=lcc +lat_1=-18 +lat_2=-36 +lat_0=0 +lon_0=134 +x_0=0 +y_0=0 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"))
plot(seas.rst)  
ext.dat <- extent(BGaimardi.proj)
plot(seas.rst, ext=ext.dat)
points(BGaimardi.proj, pch=3)
plot(wa.proj, add=T)

writeRaster(seas.rst, filename="temp.grd", format="raster", overwrite=T)

# distance to nearest road
# setwd("~/Desktop/GlobalEcologyLab/Sofie/Ascii")
setwd("~/Bettongs/ASCII")

dist2road.asc <- read.asciigrid("euclideandistanceroads.asc", as.image = FALSE, plot.image=T, proj4string = CRS(as.character("+proj=lcc +lat_1=-18 +lat_2=-36 +lat_0=0 +lon_0=134 +x_0=0 +y_0=0 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs")), colname="D2ROAD")
dist2road.rst <- raster(dist2road.asc,layer=1, values=TRUE)
plot(dist2road.rst)
points(BGaimardi.proj, pch=3)
plot(dist2road.rst, ext=ext.dat)
points(BGaimardi.proj, pch=3)
plot(wa.proj, add=T)

# slope (deg) in 5 km cells
slope.asc <- read.asciigrid("slope_5km.asc", as.image = FALSE, plot.image=T, proj4string = CRS(as.character("+proj=lcc +lat_1=-18 +lat_2=-36 +lat_0=0 +lon_0=134 +x_0=0 +y_0=0 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs")), colname="SLOPE")
slope.rst <- raster(slope.asc,layer=1, values=TRUE)
plot(slope.rst)
points(BGaimardi.proj, pch=3)
plot(slope.rst, ext=ext.dat)
points(BGaimardi.proj, pch=3)
plot(wa.proj, add=T)

# elev (m) DEM
elev.asc <- read.asciigrid("dem_5km.asc", as.image = FALSE, plot.image=T, proj4string = CRS(as.character("+proj=lcc +lat_1=-18 +lat_2=-36 +lat_0=0 +lon_0=134 +x_0=0 +y_0=0 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs")), colname="ELEV")
elev.rst <- raster(elev.asc,layer=1, values=TRUE)
plot(elev.rst)
points(BGaimardi.proj, pch=3)
plot(elev.rst, ext=ext.dat)
points(BGaimardi.proj, pch=3)
plot(wa.proj, add=T)

# changes in vegetation patch size in 75 Australian vegetation communities (as defined by the National Vegetation Information System 4.1)
# underpinning data is a fragmentation measure reflecting  change in proportion of vegetation patches made up of less than 5000 ha
# setwd("~/Desktop/GlobalEcologyLab/Sofie/TIFF")
setwd("~/Bettongs/TIFF")
commchnge.rst <- raster("Comm_Chng.tif",layer=1, values=TRUE)
names(commchnge.rst) <- "COMMCHNGE"

#commchnge.rst <- crop(commchnge.rst, ext.dat.sq) ## check why this doesnt work
plot(commchnge.rst)
points(BGaimardi.proj, pch=3)
plot(commchnge.rst, ext=ext.dat)
points(BGaimardi.proj, pch=3)
plot(wa.proj, add=T)

# Mean percentage sand derived from the Atlas of Australian Soils
# setwd("~/Documents/Papers/Australian.Mammals/Woylies/data/rasters/ASCII")
# setwd("C:/Users/Sofie/Desktop/BTB/ASCII") #sofie
# setwd("U:/Bettongs/ASCII")
# setwd("~/Desktop/GlobalEcologyLab/Sofie/Ascii")
setwd("~/Bettongs/ASCII")
sand.asc <- read.asciigrid("mean_percent_sand.asc", as.image = FALSE, plot.image=T, proj4string = CRS(as.character("+proj=lcc +lat_1=-18 +lat_2=-36 +lat_0=0 +lon_0=134 +x_0=0 +y_0=0 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs")), colname="SAND")
sand.rst <- raster(sand.asc,layer=1, values=TRUE)
plot(sand.rst)
points(BGaimardi.proj, pch=3)
plot(sand.rst, ext=ext.dat)
points(BGaimardi.proj, pch=3)
plot(wa.proj, add=T)

# number of burns (national fire-return frequency 1988-2015)
# setwd("~/Desktop/GlobalEcologyLab/Sofie/Ascii")
setwd("~/Bettongs/ASCII")
burns.asc <- read.asciigrid("no_burns.asc", as.image = FALSE, plot.image=T, proj4string = CRS(as.character("+proj=lcc +lat_1=-18 +lat_2=-36 +lat_0=0 +lon_0=134 +x_0=0 +y_0=0 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs")), colname="BURNS")
burns.rst <- raster(burns.asc,layer=1, values=TRUE)
burns.rst[is.na(burns.rst)] <- 0
burns.rst[is.infinite(burns.rst)] <- 0
plot(burns.rst)
points(BGaimardi.proj, pch=3)
plot(burns.rst, ext=ext.dat)
points(BGaimardi.proj, pch=3)
plot(wa.proj, add=T)

# Normalised Difference Vegetation Index
# https://data.gov.au/dataset/ds-tern-3745d0ae-7e0f-4985-ba83-e647349616c2/distribution/dist-tern-3745d0ae-7e0f-4985-ba83-e647349616c2-0/details?q=

# setwd("~/Desktop/GlobalEcologyLab/Sofie/States")
setwd("~/Bettongs/States")
ndvi1 <- nc_open("C-bawap.D1-20120101.D2-20120131.I-P1M.V-ndvi-1km-1month.P-raw.DC-20120201T000915.DM-20120604T004651.nc")
print(ndvi1)
ndvi1.lon <- ncvar_get(ndvi1, "longitude")
ndvi1.lat <- ncvar_get(ndvi1, "latitude")
length(ndvi1.lon)
length(ndvi1.lat)
t <- ncvar_get(ndvi1, "time")
tunits <- ncatt_get(ndvi1, "time", "units")
nt <- dim(t)
nt
ndvi1.array <- ncvar_get(ndvi1, 'ndvi-1km-1month')
dlname <- ncatt_get(ndvi1, 'ndvi-1km-1month', "long_name")
dunits <- ncatt_get(ndvi1, 'ndvi-1km-1month', "units")
fillvalue <- ncatt_get(ndvi1, 'ndvi-1km-1month', "_FillValue")
dim(ndvi1.array)
nc_close(ndvi1)
#ndvi1.array[ndvi1.array == fillvalue$value] <- NA
#length(na.omit(as.vector(ndvi1.array[, ])))
lonlat <- expand.grid(ndvi1.lon, ndvi1.lat)
ndvi1.vec <- as.vector(ndvi1.array)
length(ndvi1.vec)
ndvi1.df01 <- data.frame(cbind(lonlat, ndvi1.vec))
names(ndvi1.df01) <- c("lon", "lat", "NDVI1")
head(na.omit(ndvi1.df01), 20)
ndvi1.df02 <- na.omit(ndvi1.df01)

ndvi1.rst1 <- raster(ncols=length(ndvi1.lon), nrows=length(ndvi1.lat))
x <- ndvi1.df02$lon
y <- ndvi1.df02$lat
vals <- ndvi1.df02$NDVI1
xy <- cbind(x, y)
p <- data.frame(xy, name=vals)
coordinates(p) <- ~x+y
ndvi1.rst1 <- rasterize(p, ndvi1.rst1, 'name')
plot(ndvi1.rst1, ext=extent(aus0))
ndvi1.crop <- crop(ndvi1.rst1, extent(aus0))
ndvi1.rst <- projectRaster(ndvi1.crop, crs=CRS("+proj=lcc +lat_1=-18 +lat_2=-36 +lat_0=0 +lon_0=134 +x_0=0 +y_0=0 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"))
plot(ndvi1.rst, ext=extent(aus0))
points(BGaimardi.proj, pch=3)
plot(ndvi1.rst, ext=ext.dat)
points(BGaimardi.proj, pch=3)
plot(wa.proj, add=T)


## resolutions
rain.rst1
temp.rst
seas.rst
dist2road.rst
slope.rst
elev.rst
commchnge.rst
sand.rst
burns.rst
ndvi1.rst


## make all lowest resolution

lowres <- ndvi1.rst

auslamb.proj <- "+proj=lcc +lat_1=-18 +lat_2=-36 +lat_0=0 +lon_0=134 +x_0=0 +y_0=0 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"
lowestres <- max(c(res(rain.rst1), res(temp.rst), res(seas.rst), res(dist2road.rst), res(slope.rst), res(elev.rst), res(ndvi1.rst), res(commchnge.rst), res(sand.rst), res(burns.rst)))
rain.lr <- projectRaster(from=rain.rst, res=lowestres, crs=crs(rain.rst), method="bilinear")
temp.lr <- projectRaster(from=temp.rst, res=lowestres, crs=crs(rain.rst), method="bilinear")
seas.lr <- projectRaster(from=seas.rst, res=lowestres, crs=auslamb.proj, method="bilinear")
dist2road.lr <- projectRaster(from=dist2road.rst, res=lowestres, crs=auslamb.proj, method="bilinear")
slope.lr <- projectRaster(from=slope.rst, res=lowestres, crs=auslamb.proj, method="bilinear")
elev.lr <- projectRaster(from=elev.rst, res=lowestres, crs=auslamb.proj, method="bilinear")
ndvi.lr <-  projectRaster(from=ndvi1.rst, res=lowestres, crs=auslamb.proj, method="bilinear")
commchnge.lr <- projectRaster(from=commchnge.rst, res=lowestres, crs=auslamb.proj, method="bilinear")
sand.lr <- projectRaster(from=sand.rst, res=lowestres, crs=auslamb.proj, method="bilinear")
burns.lr <- projectRaster(from=burns.rst, res=lowestres, crs=auslamb.proj, method="bilinear")

## crop to same extent as point data
xydiff.max <- max(c((ext.dat@xmax - ext.dat@xmin), (ext.dat@ymax - ext.dat@ymin)))
x.or.y <- ifelse((xydiff.max == (ext.dat@xmax - ext.dat@xmin))[1], "x", "y")
# square extent area
if (x.or.y == "x") ext.dat.sq <- extent(ext.dat@xmin, ext.dat@xmax, ext.dat@ymin, (ext.dat@ymin + xydiff.max)) else ext.dat.sq <- extent(ext.dat@xmin, (ext.dat@xmin+xydiff.max), ext.dat@ymin, ext.dat@ymax)


## crop all variables to same extent as point data
rain.crop <- crop(rain.lr, ext.dat.sq, snap="near")
names(rain.crop) <- "RAIN"
temp.crop <- crop(temp.lr, ext.dat.sq, snap="near")
names(temp.crop) <- "TEMP"
seas.crop <- crop(seas.lr, ext.dat.sq, snap="near")
names(seas.crop) <- "SEAS"
dist2road.crop <- crop(dist2road.lr, ext.dat.sq, snap="near")
names(dist2road.crop) <- "DIST2ROAD"
slope.crop <- crop(slope.lr, ext.dat.sq, snap="near")
names(slope.crop) <- "SLOPE"
elev.crop <- crop(elev.lr, ext.dat.sq, snap="near")
names(elev.crop) <- "ELEV"
commchnge.crop <- crop(commchnge.lr, ext.dat.sq, snap="near")
names(commchnge.crop) <- "COMMCHNGE"
sand.crop <- crop(sand.lr, ext.dat.sq, snap="near")
names(sand.crop) <- "SAND"
burns.crop <- crop(burns.lr, ext.dat.sq, snap="near")
NAvalue(burns.crop) <- 0
names(burns.crop) <- "BURNS"
ndvi.crop <- crop(ndvi.lr, ext.dat.sq, snap="near")
names(ndvi.crop) <- "NDVI"


rain.resamp <- resample(rain.crop, rain.crop, method="bilinear")
temp.resamp <- resample(temp.crop, rain.crop, method="bilinear")
seas.resamp <- resample(seas.crop, rain.crop, method="bilinear")
dist2road.resamp <- resample(dist2road.crop, rain.crop, method="bilinear")
slope.resamp <- resample(slope.crop, rain.crop, method="bilinear")
elev.resamp <- resample(elev.crop, rain.crop, method="bilinear")
ndvi.resamp <- resample(ndvi.crop, rain.crop, method="bilinear")
commchnge.resamp <- resample(commchnge.crop, rain.crop, method="bilinear")
sand.resamp <- resample(sand.crop, rain.crop, method="bilinear")
burns.resamp <- resample(burns.crop, rain.crop, method="bilinear")

names(rain.crop) <- "RAIN"
names(temp.crop) <- "TEMP"
names(seas.crop) <- "SEAS"
names(dist2road.crop) <- "DIST2ROAD"
names(slope.crop) <- "SLOPE"
names(elev.crop) <- "ELEV"
names(ndvi.crop) <- "NDVI"
names(commchnge.crop) <- "COMMCHNGE"
names(sand.crop) <- "SAND"
names(burns.crop) <- "BURNS"

env.stack <- stack(rain.resamp, temp.resamp, seas.resamp, dist2road.resamp, slope.resamp, elev.resamp, ndvi.resamp, commchnge.resamp, sand.resamp, burns.resamp)
env.stack
plot(env.stack)

# multicollinearity => variance inflation factor
vif(env.stack)

##++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##  STEP 3: BIOMOD2 SPECIES DISTRIBUTION MODELLING
##++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# setwd("~/Desktop/GlobalEcologyLab/Sofie/Run")
setwd("~/Bettongs/BGaimardi/Run")

# studied species
RespName <- 'Bgaimardi'

# the presence/absences data for our species
Resp <- as.numeric(presences$pres)

# the XY coordinates of species data
RespXY <- coordinates(BGaimardi.proj)
plot(RespXY)

BiomodData <- BIOMOD_FormatingData(resp.var = Resp, 
                                   expl.var = env.stack, 
                                   resp.xy = RespXY, 
                                   resp.name = RespName, 
                                   PA.nb.rep = 3, 
                                   PA.nb.absences = 5*length(Resp), 
                                   PA.strategy = 'random')
BiomodData
plot(BiomodData)


# 2. Defining Models Options using default options.
BiomodOption <- BIOMOD_ModelingOptions(
  GLM = gam.control(maxit = 10000))

# 3. Computing the models
# INCLUDING MAXENT without MARS ,'MAXENT.Phillips'
BiomodModelOut <- BIOMOD_Modeling(
  BiomodData,
  models = c('SRE','CTA','RF','FDA','GLM','GBM','GAM','ANN','MARS'),
  models.options = BiomodOption,
  NbRunEval=4,
  DataSplit=80,
  Prevalence=NULL,
  VarImport=10,
  models.eval.meth = c('TSS','ROC'),
  SaveObj = TRUE,
  rescal.all.models = TRUE,
  do.full.models = FALSE,
  modeling.id = paste(RespName,"FirstModeling",sep=""))


# get all model evaluations
BiomodModelEval <- get_evaluations(BiomodModelOut)

# print the dimnames of this object
dimnames(BiomodModelEval)

# model evaluation : by models 
models_scores_graph(BiomodModelOut, by = "models", metrics = c("ROC","TSS"),xlim = c(0.3,1), ylim = c(0.3,1))

# model evaluation : by run (i.e., NbRunEval)
models_scores_graph(BiomodModelOut, by = "cv_run", metrics = c("ROC","TSS"),xlim = c(0.6,1), ylim = c(0.4,1))

# model evaluation : by pseudoabsence dataset (i.e., PA.nb.rep)
models_scores_graph(BiomodModelOut, by = "data_set", metrics = c("ROC","TSS"),xlim = c(0.6,1), ylim = c(0.4,1))

# print the ROC scores of all selected models
ROC.average <- rowMeans(apply(BiomodModelEval["ROC","Testing.data",,,],c(1,2),mean),na.rm = T)
ROC.average
TSS.average <- rowMeans(apply(BiomodModelEval["TSS","Testing.data",,,],c(1,2),mean),na.rm = T)
TSS.average

rev(sort(ROC.average))
rev(sort(TSS.average))

ROC.st <- ROC.average/sum(ROC.average, na.rm=T)
ROC.st
TSS.st <- TSS.average/sum(TSS.average, na.rm=T)
TSS.st

##========================= VARIANCE IMPORTANCE AND RESPONSE CURVES
# print variable importances



# print variable importances
varimp <- get_variables_importance(BiomodModelOut)
varimp

varimp.dims <- dim(varimp)
varimpnorm <- varimpwtROC <- varimpwtTSS <- varimp*0
for (i in 1:varimp.dims[3]) {
  for (j in 1:varimp.dims[4]) {
    sums.method <- as.numeric(apply(varimp[,,i,j], MARGIN=2, max, na.rm=T))
    for (a in 1:varimp.dims[2]) {
      varimpnorm[,a,i,j] <- varimp[,a,i,j] / sums.method[a]
    }
    for (b in 1:varimp.dims[1]) {
      varimpwtROC[b,,i,j] <- varimpnorm[b,,i,j] * ROC.st
      varimpwtTSS[b,,i,j] <- varimpnorm[b,,i,j] * TSS.st
    }
  }
}





Varimp<-get_variables_importance(BiomodModelOut)
mean_Varimp<-apply(Varimp,c(1,2),mean) #average acros pseudo-absence scenario and cross-validation

#barplot of variable importance for RF

library(tidyverse)
RFmeanvarimp<-as.data.frame(mean_Varimp)$RF
names(RFmeanvarimp)<-c("RAIN","TEMP","SEAS","D2RD","SLOPE","ELEV","NDVI","COMCG","SAND","BURN")
barplot(RFmeanvarimp,names=TRUE, xlab=TRUE, col = blues9)

###or 

barplot(as.data.frame(mean_Varimp)$RF,
        names.arg=c("RAIN","TEMP","SEAS","D2RD","SLOPE","ELEV","NDVI","COMCG","SAND","BURN"),
        xlab="Variable",
        col = blues9) 



# response variable for the best model (Random Forest)
Varesp_rf <- BIOMOD_LoadModels(BiomodModelOut, models = 'RF')
rf_eval_strip <- biomod2::response.plot2(models = Varesp_rf, Data = get_formal_data(BiomodModelOut,'expl.var'), 
                                         show.variables = get_formal_data(BiomodModelOut,'expl.var.names'),
                                         do.bivariate= FALSE,fixed.var.metric = 'median', legend = FALSE,
                                         display_title = FALSE, data_species = get_formal_data(BiomodModelOut,'resp.var'))



# print variable importances
varimp <- get_variables_importance(BiomodModelOut)
varimp

varimp.dims <- dim(varimp)
varimpnorm <- varimpwtROC <- varimpwtTSS <- varimp*0
for (i in 1:varimp.dims[3]) {
  for (j in 1:varimp.dims[4]) {
    sums.method <- as.numeric(apply(varimp[,,i,j], MARGIN=2, max, na.rm=T))
    for (a in 1:varimp.dims[2]) {
      varimpnorm[,a,i,j] <- varimp[,a,i,j] / sums.method[a]
    }
    for (b in 1:varimp.dims[1]) {
      varimpwtROC[b,,i,j] <- varimpnorm[b,,i,j] * ROC.st
      varimpwtTSS[b,,i,j] <- varimpnorm[b,,i,j] * TSS.st
    }
  }
}

# weighted averages
wtmeansROC.arr <- wtmeansTSS.arr <- array(data=NA, dim=c(varimp.dims[1], varimp.dims[3], varimp.dims[4]))
for (i in 1:varimp.dims[3]) {
  for (j in 1:varimp.dims[4]) {
    wtmeansROC.arr[,i,j] <- apply(varimpwtROC[,,i,j], MARGIN=1, sum, na.rm=T)
    wtmeansTSS.arr[,i,j] <- apply(varimpwtTSS[,,i,j], MARGIN=1, sum, na.rm=T)
  }
}

var.imp.wt.avg <- rep(0,varimp.dims[1])
for (i in 1:varimp.dims[1]) {
  var.imp.wt.avg[i] <- mean(wtmeansROC.arr[i,,], na.rm=T)
}
varimpwt.table <- data.frame(rownames(varimp), var.imp.wt.avg)
varimpwt.sort <- varimpwt.table[order(varimpwt.table[,2],decreasing=T),1:2]
varimpwt.sort
barplot(varimpwt.sort[,2], 
        names=c("TEMP", "SEAS", "ELEV", "RAIN", "DIST2RD", "SAND", "NDVI", "BURNS", "SLOPE", "COMMCHNG" ), 
        ylab="VI")



##=======================  BEST MODEL (RF) PROJECTION - PRESENT DAY 
# model projection: current
BiomodModelProj<-BIOMOD_Projection(modeling.output = BiomodModelOut, 
                                   new.env = env.stack,proj.name = "current", 
                                   binary.meth = "TSS", output.format = ".img", do.stack = FALSE)

# make some plots sub-selected by str.grep argument
plot(BiomodModelProj, str.grep = 'RF')

# if you want to make custom plots, you can also get the projected map
myCurrentProj <- get_predictions(BiomodModelProj) #we can now manipulate the differents outputs

# averaging results of all RF models
#f1<-stack(myCurrentProj$gaimardi_PA1_RUN1_RF,myCurrentProj$gaimardi_PA1_RUN2_RF,myCurrentProj$gaimardi_PA1_RUN3_RF,myCurrentProj$gaimardi_PA1_RUN4_RF)
#plot(mean(f1))

##++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##  STEP 4: BIOMOD2 SPECIES ENSEMBLE DISTRIBUTION MODELLING
##++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

BiomodEM <- BIOMOD_EnsembleModeling(
  modeling.output = BiomodModelOut,
  chosen.models = 'all',
  em.by='all',
  eval.metric = c('TSS'),
  eval.metric.quality.threshold = c(0.8),
  models.eval.meth = c('TSS','ROC'),
  prob.mean = FALSE,
  prob.cv = T,
  committee.averaging = T,
  prob.mean.weight = T,
  VarImport = 10)

# get evaluation scores
BiomodEnsembleModelEval <- get_evaluations(BiomodEM)
#here EMwmeanByTSS is the averaging method that returns the best results TSS = 0.899 and ROC = 0.991
#we will keep these results for the rest

# variable importance
VarimpEM<-get_variables_importance(BiomodEM)
VarimpEM<-VarimpEM[,,2] #we keep only EMcaByTSS
mean_VarimpEM<-rowMeans(VarimpEM) #average acros Varimp permutations
barplot(mean_VarimpEM,names=c("RAIN","TEMP","SEAS","D2RD","SLOPE","ELEV","NDVI","COMCG","SAND","BURN"),col = blues9, las = 2) #barplot of variable importance for RF

# print variable importances

# print variable importances
varimp <- get_variables_importance(BiomodModelOut)
varimp

varimp.dims <- dim(varimp)
varimpnorm <- varimpwtROC <- varimpwtTSS <- varimp*0
for (i in 1:varimp.dims[3]) {
  for (j in 1:varimp.dims[4]) {
    sums.method <- as.numeric(apply(varimp[,,i,j], MARGIN=2, max, na.rm=T))
    for (a in 1:varimp.dims[2]) {
      varimpnorm[,a,i,j] <- varimp[,a,i,j] / sums.method[a]
    }
    for (b in 1:varimp.dims[1]) {
      varimpwtROC[b,,i,j] <- varimpnorm[b,,i,j] * ROC.st
      varimpwtTSS[b,,i,j] <- varimpnorm[b,,i,j] * TSS.st
    }
  }
}

# weighted averages
wtmeansROC.arr <- wtmeansTSS.arr <- array(data=NA, dim=c(varimp.dims[1], varimp.dims[3], varimp.dims[4]))
for (i in 1:varimp.dims[3]) {
  for (j in 1:varimp.dims[4]) {
    wtmeansROC.arr[,i,j] <- apply(varimpwtROC[,,i,j], MARGIN=1, sum, na.rm=T)
    wtmeansTSS.arr[,i,j] <- apply(varimpwtTSS[,,i,j], MARGIN=1, sum, na.rm=T)
  }
}

var.imp.wt.avg <- rep(0,varimp.dims[1])
for (i in 1:varimp.dims[1]) {
  var.imp.wt.avg[i] <- mean(wtmeansROC.arr[i,,], na.rm=T)
}
varimpwt.table <- data.frame(rownames(varimp), var.imp.wt.avg)
varimpwt.sort <- varimpwt.table[order(varimpwt.table[,2],decreasing=T),1:2]
varimpwt.sort

# response variable for the ensemble model ##############################trying to work this out


Varesp_EM <- BIOMOD_LoadModels(BiomodEM,'EMcaByTSS')

rf_eval_stripEM <- biomod2::response.plot2(models = Varesp_EM, Data = get_formal_data(BiomodModelOut,'expl.var'), 
                                         show.variables = get_formal_data(BiomodModelOut,'expl.var.names'),
                                         do.bivariate= FALSE,fixed.var.metric = 'median', legend = FALSE,
                                         display_title = FALSE, data_species = get_formal_data(BiomodModelOut,'resp.var'))

par(mfrow=c(3,4))
plot(rf_eval_stripEM$RAIN$Bgaimardi_EMcaByTSS_mergedAlgo_mergedRun_mergedData, main = "RAIN", ylab = "", xlab = "",ylim=c(0, 1))
plot(rf_eval_stripEM$TEMP$Bgaimardi_EMcaByTSS_mergedAlgo_mergedRun_mergedData, main = "TEMP", ylab = "", xlab = "",ylim=c(0, 1))
plot(rf_eval_stripEM$SEAS$Bgaimardi_EMcaByTSS_mergedAlgo_mergedRun_mergedData, main = "SEAS", ylab = "", xlab = "",ylim=c(0, 1))
plot(rf_eval_stripEM$DIST2ROAD$Bgaimardi_EMcaByTSS_mergedAlgo_mergedRun_mergedData, main = "DIST2ROAD", ylab = "", xlab = "",ylim=c(0, 1))
plot(rf_eval_stripEM$SLOPE$Bgaimardi_EMcaByTSS_mergedAlgo_mergedRun_mergedData, main = "SLOPE", ylab = "", xlab = "",ylim=c(0, 1))
plot(rf_eval_stripEM$ELEV$Bgaimardi_EMcaByTSS_mergedAlgo_mergedRun_mergedData, main = "ELEV", ylab = "", xlab = "",ylim=c(0, 1))
plot(rf_eval_stripEM$NDVI$Bgaimardi_EMcaByTSS_mergedAlgo_mergedRun_mergedData, main = "NDVI", ylab = "", xlab = "",ylim=c(0, 1))
plot(rf_eval_stripEM$COMMCHNGE$Bgaimardi_EMcaByTSS_mergedAlgo_mergedRun_mergedData, main = "COMMCHNGE", ylab = "", xlab = "",ylim=c(0, 1))
plot(rf_eval_stripEM$SAND$Bgaimardi_EMcaByTSS_mergedAlgo_mergedRun_mergedData, main = "SAND", ylab = "", xlab = "",ylim=c(0, 1))
plot(rf_eval_stripEM$BURNS$Bgaimardi_EMcaByTSS_mergedAlgo_mergedRun_mergedData, main = "BURNS", ylab = "", xlab = "",ylim=c(0, 1))


# Ensemble Projection 
BiomodEnsembleProj <- BIOMOD_EnsembleForecasting(
  EM.output = BiomodEM,
  projection.output = BiomodModelProj,
  binary.meth = 'ROC',
  output.format = ".grd", 
  do.stack = FALSE)

BiomodEnsembleProj
myBiomodEnsembleProj <- get_predictions(BiomodEnsembleProj)#we keep only EMwmeanByTSS
plot(myBiomodEnsembleProj$Bgaimardi_EMcaByTSS_mergedAlgo_mergedRun_mergedData)



# setwd("U:/BTB/Results/SDM/IndividualProjections")
# setwd("~/BGaimardi/Results/SDM/gaimardi/proj_current/individual_projections")
setwd("~/Bettongs/BGaimardi/Run/Bgaimardi/proj_current/individual_projections")
meanbyTSS.dat <- stack(myBiomodEnsembleProj$Bgaimardi_EMwmeanByTSS_mergedAlgo_mergedRun_mergedData)

# meanbyTSS.dat <-get_predictions(BiomodEnsembleProj)
plot(meanbyTSS.dat, extent(ext.dat))

plot(tas.proj, add=T);plot(nsw.proj, add=T);plot(sa.proj, add=T);plot(vic.proj, add=T)
writeRaster(meanbyTSS.dat, "meanPredTAS_VIC.grd", format="raster", overwrite=T)

##++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##  STEP 5: PROJECT TO THE REST OF SOUTHERN AUSTRALIA
##++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# build projection base-layer stack (new data)
plot(rain.rst)
plot(wa.proj, add=T); plot(sa.proj, add=T); plot(vic.proj, add=T); plot(nsw.proj, add=T); plot(tas.proj, add=T); plot(qld.proj, add=T); plot(nt.proj, add=T)
plot(temp.rst)
plot(wa.proj, add=T); plot(sa.proj, add=T); plot(vic.proj, add=T); plot(nsw.proj, add=T); plot(tas.proj, add=T); plot(qld.proj, add=T); plot(nt.proj, add=T)
plot(seas.rst)
plot(wa.proj, add=T); plot(sa.proj, add=T); plot(vic.proj, add=T); plot(nsw.proj, add=T); plot(tas.proj, add=T); plot(qld.proj, add=T); plot(nt.proj, add=T)
plot(dist2road.rst)
plot(wa.proj, add=T); plot(sa.proj, add=T); plot(vic.proj, add=T); plot(nsw.proj, add=T); plot(tas.proj, add=T); plot(qld.proj, add=T); plot(nt.proj, add=T)
plot(slope.rst)
plot(wa.proj, add=T); plot(sa.proj, add=T); plot(vic.proj, add=T); plot(nsw.proj, add=T); plot(tas.proj, add=T); plot(qld.proj, add=T); plot(nt.proj, add=T)
plot(elev.rst)
plot(wa.proj, add=T); plot(sa.proj, add=T); plot(vic.proj, add=T); plot(nsw.proj, add=T); plot(tas.proj, add=T); plot(qld.proj, add=T); plot(nt.proj, add=T)
plot(ndvi1.rst)
plot(wa.proj, add=T); plot(sa.proj, add=T); plot(vic.proj, add=T); plot(nsw.proj, add=T); plot(tas.proj, add=T); plot(qld.proj, add=T); plot(nt.proj, add=T)
plot(commchnge.rst)
plot(wa.proj, add=T); plot(sa.proj, add=T); plot(vic.proj, add=T); plot(nsw.proj, add=T); plot(tas.proj, add=T); plot(qld.proj, add=T); plot(nt.proj, add=T)


plot(sand.rst)
plot(wa.proj, add=T); plot(sa.proj, add=T); plot(vic.proj, add=T); plot(nsw.proj, add=T); plot(tas.proj, add=T); plot(qld.proj, add=T); plot(nt.proj, add=T)
plot(burns.rst)
plot(wa.proj, add=T); plot(sa.proj, add=T); plot(vic.proj, add=T); plot(nsw.proj, add=T); plot(tas.proj, add=T); plot(qld.proj, add=T); plot(nt.proj, add=T)

# set extent for southern australia
proj.aus.xmin <- extent(wa.proj)[1]
proj.aus.xmax <- extent(nsw.proj)[2]
proj.aus.ymin <- extent(tas.proj)[3]
#proj.aus.ymax <- extent(sa.proj)[4]
proj.aus.ymax <- proj.aus.ymin + (proj.aus.xmax - proj.aus.xmin) # makes projection area square (setting Δx to be same as Δy)

proj.ext.aus <- extent(c(proj.aus.xmin, proj.aus.xmax, proj.aus.ymin, proj.aus.ymax))

# check
plot(rain.rst, ext=proj.ext.aus)
plot(wa.proj, add=T); plot(sa.proj, add=T); plot(vic.proj, add=T); plot(nsw.proj, add=T); plot(tas.proj, add=T); plot(qld.proj, add=T); plot(nt.proj, add=T)
plot(temp.rst, ext=proj.ext.aus)
plot(wa.proj, add=T); plot(sa.proj, add=T); plot(vic.proj, add=T); plot(nsw.proj, add=T); plot(tas.proj, add=T); plot(qld.proj, add=T); plot(nt.proj, add=T)
plot(seas.rst, ext=proj.ext.aus)
plot(wa.proj, add=T); plot(sa.proj, add=T); plot(vic.proj, add=T); plot(nsw.proj, add=T); plot(tas.proj, add=T); plot(qld.proj, add=T); plot(nt.proj, add=T)
plot(dist2road.rst, ext=proj.ext.aus)
plot(wa.proj, add=T); plot(sa.proj, add=T); plot(vic.proj, add=T); plot(nsw.proj, add=T); plot(tas.proj, add=T); plot(qld.proj, add=T); plot(nt.proj, add=T)
plot(slope.rst, ext=proj.ext.aus)
plot(wa.proj, add=T); plot(sa.proj, add=T); plot(vic.proj, add=T); plot(nsw.proj, add=T); plot(tas.proj, add=T); plot(qld.proj, add=T); plot(nt.proj, add=T)
plot(elev.rst, ext=proj.ext.aus)
plot(wa.proj, add=T); plot(sa.proj, add=T); plot(vic.proj, add=T); plot(nsw.proj, add=T); plot(tas.proj, add=T); plot(qld.proj, add=T); plot(nt.proj, add=T)
plot(ndvi1.rst, ext=proj.ext.aus)
plot(wa.proj, add=T); plot(sa.proj, add=T); plot(vic.proj, add=T); plot(nsw.proj, add=T); plot(tas.proj, add=T); plot(qld.proj, add=T); plot(nt.proj, add=T)
plot(commchnge.rst, ext=proj.ext.aus)
plot(wa.proj, add=T); plot(sa.proj, add=T); plot(vic.proj, add=T); plot(nsw.proj, add=T); plot(tas.proj, add=T); plot(qld.proj, add=T); plot(nt.proj, add=T)
plot(sand.rst, ext=proj.ext.aus)
plot(wa.proj, add=T); plot(sa.proj, add=T); plot(vic.proj, add=T); plot(nsw.proj, add=T); plot(tas.proj, add=T); plot(qld.proj, add=T); plot(nt.proj, add=T)
plot(burns.rst, ext=proj.ext.aus)
plot(wa.proj, add=T); plot(sa.proj, add=T); plot(vic.proj, add=T); plot(nsw.proj, add=T); plot(tas.proj, add=T); plot(qld.proj, add=T); plot(nt.proj, add=T)

## crop to same extent as southern aus extent
aus.rain.crop <- crop(rain.lr, extent(proj.ext.aus), snap="near")
names(aus.rain.crop) <- "RAIN"
aus.temp.crop <- crop(temp.lr, extent(proj.ext.aus), snap="near")
names(aus.temp.crop) <- "TEMP"
aus.seas.crop <- crop(seas.lr, extent(proj.ext.aus), snap="near")
names(aus.seas.crop) <- "SEAS"
aus.dist2road.crop <- crop(dist2road.lr, extent(proj.ext.aus), snap="near")
names(aus.dist2road.crop) <- "DIST2ROAD"
aus.slope.crop <- crop(slope.lr, extent(proj.ext.aus), snap="near")
names(aus.slope.crop) <- "SLOPE"
aus.elev.crop <- crop(elev.lr, extent(proj.ext.aus), snap="near")
names(aus.elev.crop) <- "ELEV"
aus.commchnge.crop <- crop(commchnge.lr, extent(proj.ext.aus), snap="near")
names(aus.commchnge.crop) <- "COMMCHNGE"
aus.sand.crop <- crop(sand.lr, extent(proj.ext.aus), snap="near")
names(aus.sand.crop) <- "SAND"
aus.burns.crop <- crop(burns.lr, extent(proj.ext.aus), snap="near")
names(aus.burns.crop) <- "BURNS"
aus.ndvi.crop <- crop(ndvi1.rst, extent(proj.ext.aus), snap="near")
names(aus.ndvi.crop) <- "NDVI"

aus.rain.resamp <- resample(aus.rain.crop, aus.rain.crop, method="bilinear")
aus.temp.resamp <- resample(aus.temp.crop, aus.rain.crop, method="bilinear")
aus.seas.resamp <- resample(aus.seas.crop, aus.rain.crop, method="bilinear")
aus.dist2road.resamp <- resample(aus.dist2road.crop, aus.rain.crop, method="bilinear")
aus.slope.resamp <- resample(aus.slope.crop, aus.rain.crop, method="bilinear")
aus.elev.resamp <- resample(aus.elev.crop, aus.rain.crop, method="bilinear")
aus.ndvi.resamp <- resample(aus.ndvi.crop, aus.rain.crop, method="bilinear")
aus.commchnge.resamp <- resample(aus.commchnge.crop, aus.rain.crop, method="bilinear")
aus.sand.resamp <- resample(aus.sand.crop, aus.rain.crop, method="bilinear")
aus.burns.resamp <- resample(aus.burns.crop, aus.rain.crop, method="bilinear")

names(aus.rain.resamp) <- "RAIN"
names(aus.temp.resamp) <- "TEMP"
names(aus.seas.resamp) <- "SEAS"
names(aus.dist2road.resamp) <- "DIST2ROAD"
names(aus.slope.resamp) <- "SLOPE"
names(aus.elev.resamp) <- "ELEV"
names(aus.ndvi.resamp) <- "NDVI"
names(aus.commchnge.resamp) <- "COMMCHNGE"
names(aus.sand.resamp) <- "SAND"
names(aus.burns.resamp) <- "BURNS"

#setwd("~/Desktop/GlobalEcologyLab/Sofie/Derived_Layers")
setwd("~/Bettongs/BGaimardi/Derived_Layers")

writeRaster(aus.rain.resamp, filename="aus.rain.resamp.grd", format="raster", overwrite=T)
writeRaster(aus.temp.resamp, filename="aus.temp.resamp.grd", format="raster", overwrite=T)
writeRaster(aus.seas.resamp, filename="aus.seas.resamp.grd", format="raster", overwrite=T)
writeRaster(aus.dist2road.resamp, filename="aus.dist2road.resamp.grd", format="raster", overwrite=T)
writeRaster(aus.elev.resamp, filename="aus.elev.resamp.grd", format="raster", overwrite=T)
writeRaster(aus.slope.resamp, filename="aus.slope.resamp.grd", format="raster", overwrite=T)
writeRaster(aus.ndvi.resamp, filename="aus.ndvi.resamp.grd", format="raster", overwrite=T)
writeRaster(aus.commchnge.resamp, filename="aus.commchnge.resamp.grd", format="raster", overwrite=T)
writeRaster(aus.sand.resamp, filename="aus.sand.resamp.grd", format="raster", overwrite=T)
writeRaster(aus.burns.resamp, filename="aus.burns.resamp.grd", format="raster", overwrite=T)


aus.env.stack <- stack(aus.rain.resamp, aus.temp.resamp, aus.seas.resamp, aus.dist2road.resamp, aus.slope.resamp, aus.elev.resamp, aus.ndvi.resamp, aus.commchnge.resamp, aus.sand.resamp, aus.burns.resamp)
aus.env.stack
plot(aus.env.stack)


# model projection: current
#setwd("~/Documents/Papers/Australian.Mammals/Woylies/results/SDM")
#setwd("C:/Users/Sofie/Desktop/BTB/Results/SDM") #sofie's
# setwd("U:/BTB/Results/SDM")
# setwd("~/BGaimardi/Results/SDM")
setwd("~/Bettongs/BGaimardi/Run")


BiomodModelProjAus <- BIOMOD_Projection(modeling.output = BiomodModelOut, 
                                        new.env = aus.env.stack, 
                                        proj.name = "allsaus", 
                                        binary.meth = "TSS", 
                                        output.format = ".img", do.stack = FALSE)
#plot(BiomodModelProjAus) #don't do this


##++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##  STEP 6: BIOMOD2 SPECIES ENSEMBLE DISTRIBUTION MODELLING
##++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


BiomodEMAus <- BIOMOD_EnsembleModeling(
  modeling.output = BiomodModelOut,
  chosen.models = 'all',
  em.by='all',
  eval.metric = c('TSS'),
  eval.metric.quality.threshold = c(0.8),
  models.eval.meth = c('KAPPA','TSS','ROC'),
  prob.mean = FALSE,
  prob.cv = T,
  committee.averaging = T,
  prob.mean.weight = T,
  VarImport = 0)


# get evaluation scores
BiomodEnsembleModelEvalAus <- get_evaluations(BiomodEMAus)
########################ensembleproj

# Ensemble Projection 
BiomodEnsembleProjAus <- BIOMOD_EnsembleForecasting(
  EM.output = BiomodEMAus,
  projection.output = BiomodModelProjAus,
  binary.meth = 'TSS',
  output.format = ".grd", 
  do.stack = FALSE)


BiomodEnsembleProjAus

plot(BiomodEnsembleProjAus)
BiomodEnsembleProjAus@models.projected

# load specific map from drive
# setwd("~/Documents/Papers/Australian.Mammals/Woylies/results/SDM/woylie/proj_allsaus/individual_projections/")
# setwd("C:/Users/Sofie/Desktop/BTB/Results/SDM/IndividualProjections") #sofie's
# setwd("U:/BTB/Results/SDM/IndividualProjections")
# setwd("~/BTB/Results/SDM/IndividualProjections")
# setwd("~/BGaimardi/Results/SDM/gaimardi/proj_allsaus/individual_projections")
# setwd("~/Bettongs/BGaimardi/Run/Bgaimardi/models/BgaimardiFirstModeling")
setwd("~/Bettongs/BGaimardi/Run/Bgaimardi/proj_allsaus/individual_projections")

meanbyTSS <- stack("Bgaimardi_EMcaByTSS_mergedAlgo_mergedRun_mergedData")
plot(meanbyTSS, extent(proj.ext.aus))
plot(wa.proj, add=T); plot(sa.proj, add=T); plot(vic.proj, add=T); plot(nsw.proj, add=T); plot(tas.proj, add=T); plot(qld.proj, add=T); plot(nt.proj, add=T)

# setwd("~/Documents/Papers/Australian.Mammals/Woylies/results/SDM")
# setwd("U:/BTB/Results/SDM")
# setwd("~/BGaimardi/Results/SDM")


writeRaster(meanbyTSS, filename="BgaimardimeanPredSAUS.grd", format="raster", overwrite=T)
setwd("~/Bettongs/States")
SAusExtent<-raster("SAusExt.tif")
setwd("~/Bettongs/BGaimardi/Run/Bgaimardi/proj_allsaus/individual_projections")
BgaimAus<-raster('BgaimardimeanPredSAUS.grd')
BgaimardiSAus<-crop(BgaimAus,SAusExtent)
BgaimardiSAusFinal<-BgaimardiSAus/1000

plot(BgaimardiSAusFinal, options(scipen=5))
plot(wa.proj, add=T); plot(sa.proj, add=T); plot(vic.proj, add=T); plot(nsw.proj, add=T); plot(tas.proj, add=T); plot(qld.proj, add=T); plot(nt.proj, add=T)


