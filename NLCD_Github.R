############################ 
#PURPOSE: Clip NLCD to Treasure Valley 
#INPUT: NLCD 
#OUTPUT: 
#DEVELOPED: 5-8-2020
#CONTACT: sarahhalperin@u.boisestate.edu
#---------------------------------#

# PACKAGES NEEDED
library(raster) # read and edit rasters
library(rgdal)  # Add this package to read .tif files
library(dplyr) # !! Remove dplyr in order to run clump detach(name="package:dplyr", unload=TRUE)
library(sp)
library(rgdal)
library(sf)
library(terra)

# ----------------------------------------------
# FILE PATHS:

# ----------------------------------------------
# Set location general folder location for the input NLCD folder
#I downloaded these manually into a folder. 
NLCD<-"/Volumes/JodiBrandt/Common/SarahHalperin/Data/LandUse/"

# Bring in desired rasters 
version<-"rawNLCD" #desired raster folder
Input_Folder<-paste0(NLCD,version) #generate folder path
Output_Folder<-"/Volumes/JodiBrandt/Common/SarahHalperin/Data/LandUse/TV_NLCD/"
#---------------------------------------------------#
# ----------------------------------------------
# READ INPUT FILES: 
# ----------------------------------------------

#loop through a folder and then merge all datasets.
Input_Folder_List<-list.files(Input_Folder, pattern=".img$", full.names = TRUE) 

NLCD<-lapply(Input_Folder_List,function(i){
  terra::rast(i) #bring in all files in input folder
})

#--------------------------------------#
StudyArea.sp<-readOGR("/Volumes/JodiBrandt/Common/SarahHalperin/Data/StudyArea/StudyArea_wgs84.shp")


#NLCD_2001<-NLCD[[1]]
NLCD_2016<-NLCD[[2]]

#----------------------------------------------
#Check Projection 
#------------------------------------------------
StudyArea.sp<-spTransform(StudyArea.sp, crs(NLCD_2016)) #reproject to NLCD layer

#-----------------------------------------
#Clip NLCD to Treasure Valley 
#-------------------------------------------
StudyArea.Vec<-terra::vect(StudyArea.sp)

#NLCD_2001
#TV.NLCD_2001<-terra::crop(NLCD_2001, StudyArea.Vec) 
#TV.NLCD_2001.m<-terra::mask(TV.NLCD_2001, StudyArea.Vec)
#values(TV.NLCD_2001.m)[values(TV.NLCD_2001.m) <= 0] = NA

writeRaster(TV.NLCD_2001.m,file=paste0(Output_Folder, "TV.NLCD_2001.tif"),overwrite=TRUE)




#NLCD_2016

TV.NLCD_2016<-terra::crop(NLCD_2016, StudyArea.Vec) 
TV.NLCD_2016.m<-terra::mask(TV.NLCD_2016, StudyArea.Vec)
#values(TV.NLCD_2016.m)[values(TV.NLCD_2016.m) <= 0] = NA

writeRaster(TV.NLCD_2016.m,file=paste0(Output_Folder, "TV.NLCD_2016.tif"),overwrite=TRUE)

#-----------------------------#
#NLCD Layers weighted 2001, 2016, 2030, 2050 
#------------------------------#
#Determine weighted average for development classes. Need to collapse development classes as future projections only have one development class. Going to weight average based on 2016 proportions of developed open space, low, medium, high. 

NLCD_folder<-"/Volumes/JodiBrandt/Common/SarahHalperin/Data/LandUse/TV_NLCD/"
year2016<-"TV.NLCD_2016.tif"
#year2001<-"TV.NLCD_2001.tif"

NLCD_2016<-raster(paste0(NLCD_folder, year2016))

NLCD_2016.df<-freq(NLCD_2016)
NLCD_2016.df<-data.frame(NLCD_2016.df)
NLCD_2016.df<-na.omit(NLCD_2016.df)

NLCD_2016.open<-NLCD_2016.df %>%
  filter(value %in% 21)
NLCD_2016.low<-NLCD_2016.df %>%
  filter(value %in% 22)
NLCD_2016.med<-NLCD_2016.df %>%
  filter(value %in% 23)
NLCD_2016.high<-NLCD_2016.df %>%
  filter(value %in% 24)

NLCD_2016.dev<-NLCD_2016.df %>%
  filter(value %in% c(21,22,23,24))

#determine weights
dev.all<-sum(NLCD_2016.dev)

sum(NLCD_2016.open$count)/dev.all #0.52
sum(NLCD_2016.low$count)/dev.all  #0.34
sum(NLCD_2016.med$count)/dev.all #0.12
sum(NLCD_2016.high$count)/dev.all #0.02
#--------------------------------------------------#
NLCD_folder<-"/Volumes/JodiBrandt/Common/SarahHalperin/Data/LandUse/TV_NLCD/"
year2016<-"TV.NLCD_2016.tif"
#year2001<-"TV.NLCD_2001.tif"

#Reclassify NLCD to match the collapsed development classes 

#2001
#NLCD_2001<-terra::rast(paste0(NLCD_folder, year2001))
#NLCD_2001.w<-NLCD_2001
#values(NLCD_2001.w)[values(NLCD_2001.w) == 21] <- 25
#values(NLCD_2001.w)[values(NLCD_2001.w) == 22] <- 25
#values(NLCD_2001.w)[values(NLCD_2001.w) == 23] <- 25
#values(NLCD_2001.w)[values(NLCD_2001.w) == 24] <- 25
#NLCD_2016.w[]= as.integer(NLCD_2016.w[]) 
#dataType(NLCD_2016.w)="INT4U"

#2016
NLCD_2016<-terra::rast(paste0(NLCD_folder, year2016))
NLCD_2016.w<-NLCD_2016
values(NLCD_2016.w)[values(NLCD_2016.w) == 21] <- 25
values(NLCD_2016.w)[values(NLCD_2016.w) == 22] <- 25
values(NLCD_2016.w)[values(NLCD_2016.w) == 23] <- 25
values(NLCD_2016.w)[values(NLCD_2016.w) == 24] <- 25
#NLCD_2016.w[]= as.integer(NLCD_2016.w[]) 
#dataType(NLCD_2016.w)="INT4U"


#write rasters
#writeRaster(NLCD_2001.w,file="/Volumes/JodiBrandt/Common/SarahHalperin/Data/LandUse/TV_NLCD/NLCD_Weighted/TV.NLCD_2001w.tif", datatype="INT4S", overwrite=TRUE)

writeRaster(NLCD_2016.w,file="/Volumes/JodiBrandt/Common/SarahHalperin/Data/LandUse/TV_NLCD/NLCD_Weighted/TV.NLCD_2016w.tif", datatype="INT4S", overwrite=TRUE)


#-----------------------------------------------#
#Land use 2030 and 2050 

#2030
NLCD_2030<-terra::rast("/Volumes/JodiBrandt/Common/SarahHalperin/Treasure Valley Land Use Projections/Final Rasters/Preds_Pop1.5_Dens0/Pred2030_Pop1.5_Dens0.tif")


values(NLCD_2030)[values(NLCD_2030) == 2]<-NA 
values(NLCD_2030)[values(NLCD_2030) == 1]<-25 
NLCD_2030<-terra::project(NLCD_2030, NLCD_2016.w) 
NLCD_2030<-trunc(NLCD_2030)
values(NLCD_2030)[values(NLCD_2030) == 24]<-25 

NLCD_2030.m<-terra::merge(NLCD_2030, NLCD_2016.w)
#NLCD_2030.m[]= as.integer(NLCD_2030.m[])
#dataType(NLCD_2030.m)="INT4U"
#finding areas that are in 2016 but not in 2030 that are developed. What does this mean? Likely because these models came out before 2016


writeRaster(NLCD_2030.m,file="/Volumes/JodiBrandt/Common/SarahHalperin/Data/LandUse/TV_NLCD/NLCD_Weighted/TV.NLCD_2030.tif", datatype="INT4S", overwrite=TRUE)

#2050
NLCD_2050<-terra::rast("/Volumes/JodiBrandt/Common/SarahHalperin/Treasure Valley Land Use Projections/Final Rasters/Preds_Pop1.5_Dens0/Pred2050_Pop1.5_Dens0.tif")

values(NLCD_2050)[values(NLCD_2050) == 2]<-NA 
values(NLCD_2050)[values(NLCD_2050) == 1]<-25 
NLCD_2050<-terra::project(NLCD_2050, NLCD_2016.w) 
NLCD_2050<-trunc(NLCD_2050)
values(NLCD_2050)[values(NLCD_2050) == 24]<-25 

NLCD_2050.m<-terra::merge(NLCD_2050, NLCD_2016.w)
#NLCD_2050.m[]= as.integer(NLCD_2050.m[])
#dataType(NLCD_2050.m)="INT4U"


writeRaster(NLCD_2050.m,file="/Volumes/JodiBrandt/Common/SarahHalperin/Data/LandUse/TV_NLCD/NLCD_Weighted/TV.NLCD_2050.tif", datatype="INT4S", overwrite=TRUE)
#----------------------------------------------#
#Weighted NLCD Layers in Nad83 for Water Quality Model 
NLCD_folder<-"/Volumes/JodiBrandt/Common/SarahHalperin/Data/LandUse/TV_NLCD/NLCD_Weighted/"
year2016.w<-"TV.NLCD_2016w.tif"
#year2001.w<-"TV.NLCD_2001w.tif"
year2030.w<-"TV.NLCD_2030.tif"
year2050.w<-"TV.NLCD_2050.tif"

#Read in files
NLCD_2016.w<-terra::rast(paste0(NLCD_folder,year2016.w))
NLCD_2001.w<-terra::rast(paste0(NLCD_folder,year2001.w))
NLCD_2030<-terra::rast(paste0(NLCD_folder,year2030.w))
NLCD_2050<-terra::rast(paste0(NLCD_folder,year2050.w)) 


#Reproject 
DEM<-terra::rast("/Volumes/JodiBrandt/Common/SarahHalperin/Data/WaterQuality/DEM/NED_10mFill.tif") #only layer I have in NAD83. Not sure why won't let just assign NAD83 using ESPG. 

NLCD_2001.Nad83<-terra::project(NLCD_2001.w, crs(DEM), method="near")
NLCD_2016.Nad83<-terra::project(NLCD_2016.w, crs(DEM), method="near")
NLCD_2030.Nad83<-terra::project(NLCD_2030, crs(DEM), method="near")
NLCD_2050.Nad83<-terra::project(NLCD_2050, crs(DEM), method="near")




#Write Rasters 
writeRaster(NLCD_2001.Nad83,file="/Volumes/JodiBrandt/Common/SarahHalperin/Data/LandUse/TV_NLCD/NLCD_NAD83/TV.NLCD_2001Nad83.tif", datatype="INT4S", overwrite=TRUE)
writeRaster(NLCD_2016.Nad83,file="/Volumes/JodiBrandt/Common/SarahHalperin/Data/LandUse/TV_NLCD/NLCD_NAD83/TV.NLCD_2016Nad83.tif", datatype="INT4S", overwrite=TRUE)
writeRaster(NLCD_2030.Nad83,file="/Volumes/JodiBrandt/Common/SarahHalperin/Data/LandUse/TV_NLCD/NLCD_NAD83/TV.NLCD_2030Nad83.tif", datatype="INT4S", overwrite=TRUE)
writeRaster(NLCD_2050.Nad83,file="/Volumes/JodiBrandt/Common/SarahHalperin/Data/LandUse/TV_NLCD/NLCD_NAD83/TV.NLCD_2050Nad83.tif", datatype='INT4S', overwrite=TRUE)

  
#-------------------------------------------------------------------#
#Create Threat Layers for Habitat Quality Model. Need Ag land and Urban 

#First need to bring in raw NLCD data from above for 2001 and 2016

#create buffer around study area 
StudyArea.vec.buffer<-buffer(StudyArea.Vec, width=10000) #10km buffer greater than all threat max distances 

#2001
#TV.NLCD_2001<-terra::crop(NLCD_2001, StudyArea.vec.buffer) #bring in raw rasters above
#TV.NLCD_2001.m<-terra::mask(TV.NLCD_2001, StudyArea.vec.buffer)

#TV.NLCD_2001.w<-TV.NLCD_2001.m
#TV.NLCD_2001.w[TV.NLCD_2001.w == 21] <-25
#TV.NLCD_2001.w[TV.NLCD_2001.w == 22] <-25
#TV.NLCD_2001.w[TV.NLCD_2001.w== 23] <-25
#TV.NLCD_2001.w[TV.NLCD_2001.w == 24] <-25

#writeRaster(TV.NLCD_2001.w, file="/Volumes/JodiBrandt/Common/SarahHalperin/Data/HabitatQuality_Data/NLCD/TV.NLCD_2001B.tif", datatype='INT4S', overwrite=TRUE)

#Ag
#TV.NLCD_2001.ag<-TV.NLCD_2001.m
#TV.NLCD_2001.ag[TV.NLCD_2001.ag == 82] <-1
#TV.NLCD_2001.ag[TV.NLCD_2001.ag == 81] <-1
#TV.NLCD_2001.ag[TV.NLCD_2001.ag >1] <-0

#writeRaster(TV.NLCD_2001.ag, file="/Volumes/JodiBrandt/Common/SarahHalperin/Data/HabitatQuality_Data/Threats_20012016/agri_c.tif", datatype='INT4S', overwrite=TRUE)

#Urban 
#TV.NLCD_2001.urb<-TV.NLCD_2001.m
#TV.NLCD_2001.urb[TV.NLCD_2001.urb == 21] <-1
#TV.NLCD_2001.urb[TV.NLCD_2001.urb == 22] <-1
#TV.NLCD_2001.urb[TV.NLCD_2001.urb == 23] <-1
#TV.NLCD_2001.urb[TV.NLCD_2001.urb == 24] <-1
#TV.NLCD_2001.urb[TV.NLCD_2001.urb >1] <-0

#writeRaster(TV.NLCD_2001.urb, file="/Volumes/JodiBrandt/Common/SarahHalperin/Data/HabitatQuality_Data/Threats/Threats_2001/urban_c.tif", datatype='INT4S', overwrite=TRUE)

#2016 
TV.NLCD_2016<-terra::crop(NLCD_2016, StudyArea.vec.buffer) 
TV.NLCD_2016.m<-terra::mask(TV.NLCD_2016, StudyArea.vec.buffer)

TV.NLCD_2016.w<-TV.NLCD_2016.m
TV.NLCD_2016.w[TV.NLCD_2016.w == 21] <-25
TV.NLCD_2016.w[TV.NLCD_2016.w == 22] <-25
TV.NLCD_2016.w[TV.NLCD_2016.w== 23] <-25
TV.NLCD_2016.w[TV.NLCD_2016.w == 24] <-25

writeRaster(TV.NLCD_2016.w, file="/Volumes/JodiBrandt/Common/SarahHalperin/Data/HabitatQuality_Data/NLCD/TV.NLCD_2016B.tif", datatype='INT4S', overwrite=TRUE)


TV.NLCD_2016.ag<-TV.NLCD_2016.m
TV.NLCD_2016.ag[TV.NLCD_2016.ag == 82] <-1
TV.NLCD_2016.ag[TV.NLCD_2016.ag == 81] <-1
TV.NLCD_2016.ag[TV.NLCD_2016.ag >1] <-0

writeRaster(TV.NLCD_2016.ag, file="/Volumes/JodiBrandt/Common/SarahHalperin/Data/HabitatQuality_Data/Threats_20012016/agri_f.tif", datatype='INT4S', overwrite=TRUE)


#Urban 
TV.NLCD_2016.urb<-TV.NLCD_2016.m
TV.NLCD_2016.urb[TV.NLCD_2016.urb == 21] <-1
TV.NLCD_2016.urb[TV.NLCD_2016.urb == 22] <-1
TV.NLCD_2016.urb[TV.NLCD_2016.urb == 23] <-1
TV.NLCD_2016.urb[TV.NLCD_2016.urb == 24] <-1
TV.NLCD_2016.urb[TV.NLCD_2016.urb >1] <-0

writeRaster(TV.NLCD_2016.urb, file="/Volumes/JodiBrandt/Common/SarahHalperin/Data/HabitatQuality_Data/Threats/Threats_2016/urban_f.tif", datatype='INT4S', overwrite=TRUE)

#2030 
# Need to merge first with 2016 for US to get full buffer covered in 2030 
#TV.NLCD_2016<-terra::crop(NLCD_2016, StudyArea.vec.buffer) 
#TV.NLCD_2016.m<-terra::mask(TV.NLCD_2016, StudyArea.vec.buffer)


TV.NLCD_2030.w<-terra::merge(NLCD_2030, TV.NLCD_2016.w)


writeRaster(TV.NLCD_2030.w, file="/Volumes/JodiBrandt/Common/SarahHalperin/Data/HabitatQuality_Data/NLCD/TV.NLCD_2030B.tif", datatype='INT4S', overwrite=TRUE)

#Ag
TV.NLCD_2030.m<-terra::merge(NLCD_2030, TV.NLCD_2016.m) 

TV.NLCD_2030.ag<-TV.NLCD_2030.m
TV.NLCD_2030.ag[TV.NLCD_2030.ag == 82] <-1
TV.NLCD_2030.ag[TV.NLCD_2030.ag == 81] <-1
TV.NLCD_2030.ag[TV.NLCD_2030.ag >1] <-0

writeRaster(TV.NLCD_2030.ag, file="/Volumes/JodiBrandt/Common/SarahHalperin/Data/HabitatQuality_Data/Threats/Threats_2030/agri_c.tif", datatype='INT4S', overwrite=TRUE)

#Urban 
TV.NLCD_2030.urb<-TV.NLCD_2030.w
TV.NLCD_2030.urb[TV.NLCD_2030.urb == 25]<-1
TV.NLCD_2030.urb[TV.NLCD_2030.urb >1] <-0

writeRaster(TV.NLCD_2030.urb, file="/Volumes/JodiBrandt/Common/SarahHalperin/Data/HabitatQuality_Data/Threats/Threats_2030/urban_c.tif", datatype='INT4S', overwrite=TRUE)

#2050
TV.NLCD_2050.w<-terra::merge(NLCD_2050, TV.NLCD_2016.w)

writeRaster(TV.NLCD_2050.w, file="/Volumes/JodiBrandt/Common/SarahHalperin/Data/HabitatQuality_Data/NLCD/TV.NLCD_2050B.tif", datatype='INT4S', overwrite=TRUE)

#Ag
TV.NLCD_2050.m<-terra::merge(NLCD_2050, TV.NLCD_2016.m)



TV.NLCD_2050.m[TV.NLCD_2050.m == 82] <-1
TV.NLCD_2050.m[TV.NLCD_2050.m == 81] <-1
TV.NLCD_2050.m[TV.NLCD_2050.m >1] <-0

writeRaster(TV.NLCD_2050.ag, file="/Volumes/JodiBrandt/Common/SarahHalperin/Data/HabitatQuality_Data/Threats_20302050/agri_f.tif", datatype='INT4S', overwrite=TRUE)


#Urban 
TV.NLCD_2050.urb<-TV.NLCD_2050.w
TV.NLCD_2050.urb[TV.NLCD_2050.urb == 25]<-1
TV.NLCD_2050.urb[TV.NLCD_2050.urb >1] <-0

writeRaster(TV.NLCD_2050.urb, file="/Volumes/JodiBrandt/Common/SarahHalperin/Data/HabitatQuality_Data/Threats/Threats_2050/urban_f.tif", datatype='INT4S', overwrite=TRUE)
