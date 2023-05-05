rm(list = ls())

# 1. Save your homework as a rmarkdown or word with a name of Homework_04
# 2. Submit your homework through your own github
# 3. Due Date: May 10, 2023

# Our lessons primarily depend on doubs river datasets, which are embedded in the package of vegan. The source of the Doubs river is from the 946-a.s.l Mouthe in the western Jura mountains, and its mouth is at 175-a.s.l Verdun-sur-le-Doubs. It is a total of 453 km length in eastern France, straying into western Switzerland. Please complete the tasks and write the operation in details step by step: 

# - Obtaining a vector of the river and saving it as the area of interest (AOI)
# - Finding and downloading the remote sensing image containing ndvi
# - Using AOI to crop the image and plot the cropped image

### Section I

# 1. Utilizing openstreesmap plugin in QGIS to find and download the map of the Doubs river (map: https://www.openstreetmap.org/relation/156145#map=9/47.1085/6.1057&layers=H)
# Steps: QGIS - vector - quickOSM - key: waterway; value: river; in Doubs

# 2. Downloading the map as a geojson format, which is becoming popular for encoding a variety of geographic data structures.
# Steps: Double click - export - save features as GeoJSON format - 'doubs_points' & 'doubs_lines' layers

### Section II

# 1. You’d better to check the structure of the regions object by plotting the simple feature collection and dissolve the internal boundaries of the region if the geojson file contains. 
# Steps: Vector - Geoprocessing Tools - Dissolve - Select elements of doubs - Run

# 2. After that, please set the area that the river undergoes as the area of interest (AOI).
# Steps: Vector - Research Tools - Select by location... - Dissolved lines of doubs - Run

### Section III 

# 1. Create an account of one of these geospatial official websites, such as USGS, EARTHDATA, Copernicus data space, and login in for check the images that the river covers. 
# Steps: copernicus, https://dataspace.copernicus.eu/browser/?zoom=9&lat=47.10565&lng=6.2677&themeId=DEFAULT-THEME&visualizationUrl=https%3A%2F%2Fsh.dataspace.copernicus.eu%2Fogc%2Fwms%2F28b654e7-8912-4e59-9e58-85b58d768b3a&datasetId=S2_L2A_CDAS&fromTime=2023-04-19T00%3A00%3A00.000Z&toTime=2023-04-19T23%3A59%3A59.999Z&layerId=3_NDVI&demSource3D=%22MAPZEN%22&cloudCoverage=10 / USGS, https://earthexplorer.usgs.gov/

# 2. Filter the records to keep only Landsat images with LandCloudCover lower than 10%, and try to find what products levels. Please note whether the records contain sr_ndvi. 
# Steps: copernicus: Sentinel-2 L2A - NVDI(Default Zoom:9) - cloud 10%, choose one date
# Steps: USGS: world features - doubs - cloud range (0-10%) - datasets (Landsat 8-9 OLI/TIRS C2 L2/L1) - results

# 3. The normalized difference vegetation index, NDVI, provides a rough estimate of the abundance of healthy vegetation and provides a means of monitoring changes in vegetation over time. You can download through manual methods directly from the geospatial websites or by other means, including QGIS and R package.
# Steps: download picture as .tiff

### Section IV

# 1. Load the NDVI raster file and check whether its crs is identical to AOI. If not, transform the crs of AOI vector into the another which is the same as that of NDVI. 
# 2. After that, you can crop and mask the NDVI for a small area, which just covers the AOI. 
# 3. Plot the cropped NDVI. 
# Steps: Using R package (raster) load and crop
library('raster')
ndvi_raster <- raster("merged_NDVI.tif") # Load raster file

library('sf')
aoi_vector <- st_read("AOI.shp") # load river line which was saved as shapefile in QGIS

# Judge if crs are identical between raster and river
# If not, transform the crs of AOI vector into the another which is the same as that of NDVI
st_crs(ndvi_raster) == st_crs(aoi_vector) # TRUE
# aoi_transformed <- st_transform(aoi_vector, st_crs(ndvi_raster))

# Crop and mask the NDVI for a small area, which just covers the AOI
ndvi_cropped <- crop(ndvi_raster, aoi_vector)
ndvi_masked <- mask(ndvi_cropped, aoi_vector)

writeRaster(ndvi_masked, filename = 'doubs_masked.tif', overwrite = TRUE) # save as tiff and load in QGIS
