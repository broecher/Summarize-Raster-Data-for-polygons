# This script processes the raster data and produces polygon
# summaries and saves them as csv files. For a large area such this can be a slow process.

# Websites the data came from:
# Land Cover: http://nassgeodata.gmu.edu/CropScape/      ***get an appropriate UTM version, Don't use the degrees minutes projection, 


# SET VARIABLES
##########################
#  Set the path and name for the polgon shape:
countiesShapePath      <-  "M:/GFP/Wildlife/BigGameProgram/Deer/Statewide/Winter Severity Index/GIS/Shape"
countiesShapeName <-  "Deer_UnitsWithOutBH1" # ommit the .shp

# Set the field name that contains the county or unit names you want used in the output.
# For example the column containing "Pennington" or "02C":
countyNameFieldVar <- "UNITNO"

# Set the output file location.
ouputFile <- "M:/GFP/Wildlife/BigGameProgram/Terrestrial/Land Cover Changes/Summary Data/df Deer Units.csv"
##########################

require(rgdal)
require(raster)

countiesSHP <- readOGR(countiesShapePath, layer = countiesShapeName)
countiesSHP$countyNum <- seq(1:nrow(countiesSHP)) # this creates a unique number for all counties/units

cropScapePath      <-  "M:/GFP/Wildlife/BigGameProgram/Terrestrial/Land Cover Changes/USDA CropScape Landcover Rasters/" # with slash at end
# List of rasters, follow the same naming format in future years so that the year can be extracted based on its position in the names.
cropScapeRasterList <-  c("CDL_2014_clip_SD.tif")
df <- NULL

for (r in 1:length(cropScapeRasterList)){
cropScapeRaster = raster(paste0(cropScapePath, cropScapeRasterList[r]))

# Convert counties to same projection as the cropScape, we do this for each raster incase there are any downloaded as a different projection.
countiesSHP <- spTransform(countiesSHP ,CRS=crs(cropScapeRaster))

# THe print statements are so that you know what is going on while you wait... and wait.
print("")
print("Starting county conversion to raster")
print(cropScapeRasterList[r])
print(Sys.time())

# Convert counties Shape into raster. 
countyRaster <- rasterize(countiesSHP,cropScapeRaster,countiesSHP$countyNum)
print("Starting crosstab")
print(Sys.time())

# A crosstab of the crop raster and county raster is produced (aka contingency table).
# Crosstab is a big table (matrix) with counties as col names and crops as row names
crossOut <- crosstab(cropScapeRaster,countyRaster, progress="text")

# The cell size is obtained from each raster (res = cellsize) for this sq miles calculation. The old rasters have different cell sizes.
crossOut <- crossOut * res(cropScapeRaster)[1] * res(cropScapeRaster)[2] * 3.86102e-7 # from cells to sq meters to sq miles
# crosstab is flattened (thats when you turn a crosstab matrix into a table)
flatOut <- as.data.frame(crossOut)
# THe name of the raster is added so we know what year the data is from.
flatOut$Raster <- cropScapeRasterList[r]
colnames(flatOut) <- c("Value","CountyCode","sqMiles","Raster")
# The data is added to our df
df <- rbind(df,flatOut)

# Incase script crashes, will have what is completed so far:
write.csv(df,ouputFile , row.names=F)
# This is an attempt to control memory issues. Not sure if it helps much.
rm(cropScapeRaster )
rm(countyRaster )
} # close r (raster) loop

# This produces a table telling us the character names for the numeric county numbers.
countyKey <- as.data.frame(cbind(as.numeric(countiesSHP$countyNum), as.vector(countiesSHP[[countyNameFieldVar]])))
colnames(countyKey) <- c("CountyCode","County")
df <- merge(df,countyKey)
# This loads a table telling us the character names for the numeric crop numbers.
valuesKey <- read.csv("M:/GFP/Wildlife/BigGameProgram/Terrestrial/Land Cover Changes/Summary Data/valueKey.csv", stringsAsFactors=F)
df <- merge(df,valuesKey)

# 5,8 is the position of the year in the raster file names.
df$Year <- substr(df$Raster,5,8) 

write.csv(df,ouputFile , row.names=F)

