# Install packages
install.packages("ncdf4")
install.packages("raster")
install.packages("rgdal")

# Libraries
library(ncdf4)
library(raster)
library(rgdal)

# Reprojection
fpath <- 'YOUR PATH'
fname <- paste0(fpath,'YOUR_FILE_NAME.nc')
nc <- nc_open(fname)
SM <- ncvar_get(nc,varid = 'SM')
SM <- apply(t(SM),2,rev)
SM_raster <- raster(SM)
crs(SM_raster) <- CRS('+proj=cea +lon_0=0 +lat_ts=30 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs')
Ext<- project(cbind(c(-180,180),c(-85.044,85.044)),proj='+proj=cea +lon_0=0 +lat_ts=30 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs',inv=F)
extent(SM_raster) <- c(Ext[1,1], Ext[2,1], Ext[1,2], Ext[2,2])
r <- writeRaster(SM_raster, filename="your_path_and_filename.tif", format="GTiff", overwrite=TRUE)

# Obtain lat/lon
fpath <- 'YOUR PATH'
fname <- paste0(fpath,'YOUR_FILE_NAME.nc')
nc <- nc_open(fname)
lat <- ncvar_get(nc,varid = 'lat')
lon <- ncvar_get(nc,varid = 'lon')