# Load R libraries for the project
if(!require(rgdal)){
  install.packages("rgdal")
  library(rgdal)
}

if(!require(raster)){
  install.packages("raster")
  library(raster)
}

if(!require(caret)){
  install.packages("caret")
  library(caret)
}


#set subset Image file location
path <- "./SubsetImagery/"
setwd(path)

#import each tif file into a spatial grid dataframe
new_B1 <- readGDAL("band1.tif")
new_B2 <- readGDAL("band2.tif")
new_B3 <- readGDAL("band3.tif")
new_B4 <- readGDAL("band4.tif")
new_B5 <- readGDAL("band5.tif")
new_B6 <- readGDAL("band6.tif")
new_B7 <- readGDAL("band7.tif")


# display a single band
plot(new_B5,col = gray(0:100 / 100))

# Take each individual band (spatial grid dataframe) and create a raster stack for analysis
# Create a raster stack image of all 7 bands of the subsetted image
FullImage <- stack(raster(new_B1), raster(new_B2), raster(new_B3)
                   , raster(new_B4), raster(new_B5), raster(new_B6), 
                   raster(new_B7))

# update the names of the raster stack to a simpler B1 - B7
names(FullImage) <- paste0("B", c(1:7))
names(FullImage)

# Now that the image is a raster stack we can plot band combinations using the Red Green and Blue monitor color guns
# These band combos can be used to see trends in the image data 
# True color plot of Martha's Vineyard Image
dev.off()  # reset R Studio Graphic device
plotRGB(FullImage * (FullImage >= 0)
        , r = 4, g = 3, b = 2
        , stretch ='lin')

# False color Color Infrared plot of Martha's Vineyard Image
plotRGB(FullImage * (FullImage >= 0)
        , r = 5, g = 4, b = 3
        , scale = 2
        , stretch ='lin')




# Set up access to the ESRI file geodatabase and the training areas layer located in the project folder"
fgdb <- "../GISData.gdb"


# After the training areas were captured using air photo interpretation the vector polygons (training areas)
# were stored in the file geodatabase used in previous steps.  The file which contained all of the training areas
#for each landcover type was called MVTraining Areas.  This code imports it into a Spatial Polygons dataframe
TrainingPixels <- readOGR(dsn=fgdb, "MVTrainingAreas")
class(TrainingPixels)
# CovCode is the landcover type attribute for each training polygon.  Each polygon contains a pure landcover type




# Each polygon in Training Pixels contains a set of pixels with "pure" landcover based on aerial photo interpretation
# The CovCode field is an numeric code that identifies the landcover type .

# The 9 classes were stored as the following numbers in the  CovCode field:
# 1 - Impervious 
# 2 - Developed / Grassland  
# 3 - Deciduous Forest  
# 4 - Conferous Forest  
# 5 - Scrub/Shrub  
# 6 - Palustrine Wetland  
# 7 - Estuarine Wetland  
# 8 - Bare Ground  
# 9 - Water 


# Extract pixel values across all 7 bands of image from within landcover polygons in the Training pixels layer.  
# These pixel values serve as training information for the model.  Each record is a single pixel with it's band 
# values (1-7) and landcover type ID 

responseCol <- "CovCode"

dfTraining = data.frame(matrix(vector(), nrow = 0, ncol = length(names(FullImage)) + 1))   
for (i in 1:length(unique(TrainingPixels[[responseCol]]))){
  category <- unique(TrainingPixels[[responseCol]])[i]
  categorymap <- TrainingPixels[TrainingPixels[[responseCol]] == category,]
  dataSet <- raster::extract(FullImage, categorymap)
  dataSet <- dataSet[!unlist(lapply(dataSet, is.null))]
  dataSet <- lapply(dataSet, function(x){cbind(x, class = as.numeric(rep(category, nrow(x))))})
  df <- do.call("rbind", dataSet)
  dfTraining <- rbind(dfTraining, df)
}

# print the first few records
head(dfTraining)

# It's expected that pixels with like landcover should show similar responses across all 7 bands.  The code below plots 
# each landcover class and it's associated pixels across all 7 bands.  The plots show consistency or inconsistency in pixel
# respoonse for each landcover type.  It identifies potential problems the model may have if there is no consensus in pixel
# response for a landcover type and could show where training areas need to be updated or reconsidered.

###############################################################################
#Analyze and plot training set pixels.

if(!require(ggplot2)){
  install.packages("ggplot2")
  library(ggplot2)
}

if(!require(dplyr)){
  install.packages("dplyr")
  library(dplyr)
}

if(!require(tidyr)){
  install.packages("tidyr")
  library(tidyr)
}


# add an ID to each pixel in the training 
# dataframe for plotting

# add an ID to dfTraining for plotting
ID = seq(1:nrow(dfTraining))
dfTraining <- cbind(dfTraining, ID)

# load simple lookup table created in excel for the landcover classes and merge
#df training to show the class names in the final plot
lookup <- read.csv(file="../lookup.csv", header=TRUE)
TrainingSignatures <- merge(dfTraining, lookup, by.x = "class", by.y="LCID")

ncol(lookup)

TrainingSignatures %>% 
  gather(col, val, -c(classname, ID, class)) %>%  # from `tidyr`
  ggplot(aes(col, val, color = ID, group = ID)) + 
  geom_line() +
  facet_wrap(~classname)


########################################################

# Model Creation

# apply randomforest from caret to training set.  Using all 7 bands 
if(!require(caret)){
  install.packages("caret")
  library(caret)
}
set.seed(99)
rfmod1_7 <- train(as.factor(class)~ B1 + B2 + B3 + B4 + B5 + B6 + B7, method = "rf", data = dfTraining)
rfmod1_7

plot(rfmod1_7)


# Once the model is complete we can use it to classify all pixels in the image into a raster 
LC_Pred_MV <- predict(FullImage, model = rfmod1_7)

# create a consistent color ramp for each landcover and use it to plot the classified image

colors <- c(rgb(100, 100, 100, maxColorValue=255),  # Impervious
            rgb(211, 255, 190, maxColorValue=255),  # Grassland
            rgb(85, 255, 0, maxColorValue=255),  # Deciduous
            rgb(76, 115, 0, maxColorValue=255),  # Coniferous
            rgb(255, 170, 0, maxColorValue=255),  # Scrub/Shub
            rgb(0, 112, 255, maxColorValue=255),  # Palustrine Wetland
            rgb(255, 0, 197, maxColorValue=255),  # Estuarine Wetland
            rgb(255, 255, 204, maxColorValue=255), # Bare Ground
            rgb(0, 38, 115, maxColorValue=255))  # Water

# plot the image
plot(LC_Pred_MV, col=colors)


# import the feature class MVSubsetBND for subsetting the final classified image
# the Ground truth image is slightly smaller and the images must be exactly the same size for 
# the accuracy assessment
MVSubsetBND <- readOGR(dsn=fgdb, layer="MV_Subset")

# Subset the final Classified image to match the extent of the ground truth data
FinalClassImg <- crop(LC_Pred_MV, MVSubsetBND)

dev.off()  # reset R Studio Graphic device
plot(FinalClassImg, col=colors, legend = FALSE)



# Accuracy Assessment - Ground Truth image
# import the ground truth image for actual interpreted landcover in the test area
GrndTruthImg <- readGDAL("../GroundTruth/GroundTruth_clip3.tif")

# plot the Ground Truth Image
plot(GrndTruthImg, col = colors)


# Generate a proportions for each classified landcover for use
# in a proportional Stratified Random Sample for Accuracy Assessment

#Determine how many pixel of each landcover in my classified image
imgVector <- as.vector(FinalClassImg$layer)
totByLandCover <- as.vector(table(imgVector))

# Get the total pixels in the image
totalPixels <- length(imgVector)

# Get the percentage of all pixels in each landcover class
pixProps <- totByLandCover / totalPixels

# Choose 5000 total samples
TotalSamplePixels <- 5000
# calculate teh total sample pixels to request for each landcover type
PropofTotalsample <- pixProps * TotalSamplePixels

# Convert to integer to lose the decimal (partial sample)
SampleTotals <- as.integer(PropofTotalsample)

#### predicitons

# Code for pulling a proportional stratified random sample from final classified image
# This code grabs samples from each class (landcover) proportional to the number of total
# pixels classified in the final image.

# Write a function to pull n samples from category c so I can pass a 
# vector of n (total number of samples) and a vector of categories

sampleNfromC = function(r,N,C){
  d=subset(
    data.frame(
      sampleStratified(r==C,N, sp = T)),
    layer==1)
  d$layer=C
  d}


# Create the two vectors that will be passed to the function above on with landcover codes and the other
# based on proportions of classified landcover pixels * 5000 requested sample size.
landcovers <- as.vector(c(1,2,3,4,5,6,7,8,9))
TotalSamples <- as.numeric(SampleTotals)  # from earlier proportion calculation

# pull the samples for each category into a dataframe 
samp = do.call(rbind, lapply(1:9, function(i){sampleNfromC(FinalClassImg, TotalSamples[i],landcovers[i])}))
head(samp)  # layer is the landcover code. Cell is the cell number

# convert samp to a spatial points dataframe 
coords <- samp[, c("x", "y")]
data <- samp
crs <- CRS("+init=epsg:32619")

spdf <- SpatialPointsDataFrame(coords = coords,
                               data = data,
                               proj4string = crs)

# plot the samnple locations over the Final Classified Image.  Each point location is part of the 
# Accuracy assessment 
dev.off()
plot(FinalClassImg, col = colors, legend = FALSE)
points(spdf, pch = "+")

# convert the samples with predicted landcover to a vector.  these are the predicted landcover value at the 
# 5000 points shown in the plot
predictions = as.vector(as.integer((spdf$layer)))
head(predictions)

#### Convert Ground truth image to a raster from a spatial grid dataframe
rast <- raster(GrndTruthImg)

# use sample locations (spdf) to pull Ground truth landcover for comparison/  The results are the sampe 5000 
# pixel locations but the landcover is the actual landcover from the Ground Truth image
GrndTruth <- raster::extract(rast, spdf)
head(GrndTruth)

# Convert the ground truth samples with actual landcover to a vector
Truth <- as.vector(as.integer((GrndTruth)))

# confusion matrix. Compare the GroundTruth values and predictions at each sample location
confusionMatrix(table(Truth,predictions))


