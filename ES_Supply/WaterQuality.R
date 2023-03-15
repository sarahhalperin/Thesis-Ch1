############################ 
#PURPOSE: Water Quality - aggregate precipitation data  
#INPUT: NOAA precipitation data
#OUTPUT: aggregated to annual precipitation 
#DEVELOPED: 10-6-2020
#CONTACT: sarahhalperin@u.boisestate.edu
#---------------------------------# 
library(dplyr)
library(rgdal)
library(sp)
library(raster)
library(sf)
library(rgeos)
library(terra)

#Daily Precipitation data 2011 from NOAA 

#units are standard -> inches
#https://www1.ncdc.noaa.gov/pub/data/cdo/documentation/GHCND_documentation.pdf


#----------------------------------------------------#
#Not sure how to use here when data is not in the Git repository 
#Precip_2011<-read.csv("Q:/SarahHalperin/Data/WaterQuality/DailyPrecipitation2011.csv")

#AnnualPrecip<-aggregate(PRCP~LATITUDE + LONGITUDE, data=Precip_2011, FUN= sum ) #seems to only have data from one station in Idaho city 
#---------------------------------------------------------#
#Clip PRISM Data
#--------------------------------------------------------#
#PRISM2011_all<-raster("Q:/SarahHalperin/Data/WaterQuality/PRISM/PRISM_ppt_stable_4kmM3_2011_asc.asc")

#StudyArea<-readOGR("Q:/SarahHalperin/Data/StudyArea/StudyArea_wgs84.shp")
#NLCD_2011<-raster("Q:/SarahHalperin/Data/LandUse/TV_NLCD/TV.NLCD_2011.tif")

#StudyArea.reproj<-spTransform(StudyArea, crs(NLCD_2011))
#StudyArea.buffer<-buffer(StudyArea.reproj, width=5000)

#StudyArea.b.reproj<-spTransform(StudyArea.buffer, crs(PRISM2011_all))
#identicalCRS(StudyArea.b.reproj, PRISM2011_all)



#PRISM2011_all.TV<-crop(PRISM2011_all, extent(StudyArea.b.reproj)) 
#PRISM2011_all.TV<-raster::mask(PRISM2011_all.TV, StudyArea.b.reproj)

#Output_Folder<-"Q:/SarahHalperin/Data/WaterQuality/PRISM/"

#writeRaster(PRISM2011_all.TV,file=paste0(Output_Folder, "TV.PRISM2011buffer"), format="GTiff", overwrite=TRUE)

#-------------------------------------------------------------------#
#Clip PRISM Data for Month of June 
#PRISM2011_June<-raster("Q:/SarahHalperin/Data/WaterQuality/PRISM/PRISM_ppt_stable_4kmM3_201106_asc.asc")

#StudyArea<-readOGR("Q:/SarahHalperin/Data/StudyArea/StudyArea_wgs84.shp")
#NLCD_2011<-raster("Q:/SarahHalperin/Data/LandUse/TV_NLCD/TV.NLCD_2011.tif")

#StudyArea.reproj<-spTransform(StudyArea, crs(NLCD_2011))
#StudyArea.buffer<-buffer(StudyArea.reproj, width=5000)

#StudyArea.b.reproj<-spTransform(StudyArea.buffer, crs(PRISM2011_June))
#identicalCRS(StudyArea.b.reproj, PRISM2011_June)



#PRISM2011_June.TV<-crop(PRISM2011_June, extent(StudyArea.b.reproj)) 
#PRISM2011_June.TV<-raster::mask(PRISM2011_June.TV, StudyArea.b.reproj)

#Output_Folder<-"Q:/SarahHalperin/Data/WaterQuality/PRISM/"

#writeRaster(PRISM2011_June.TV,file=paste0(Output_Folder, "TV.PRISM2011Junebuffer"), format="GTiff", overwrite=TRUE) #Reprojected to NAD_83 in Arc. TV2011Jbproj1.tif

#---------------------------------------------------------#
#Clip WorldClim monthly Data
#https://www.worldclim.org/data/monthlywth.html
#--------------------------------------------------------#
#WorldClim_2000<-raster("Q:/SarahHalperin/Data/WaterQuality/WorldClim/wc2.1_30s_prec_06.tif") #testing the average from 1970-2000. 06 is for month of june

#StudyArea<-readOGR("Q:/SarahHalperin/Data/StudyArea/StudyArea_wgs84.shp")
#NLCD_2011<-raster("Q:/SarahHalperin/Data/LandUse/TV_NLCD/TV.NLCD_2011.tif")

#StudyArea.reproj<-spTransform(StudyArea, crs(NLCD_2011))
#StudyArea.buffer<-buffer(StudyArea.reproj, width=5000)

#StudyArea.b.reproj<-spTransform(StudyArea.buffer, crs(WorldClim_2000))
#identicalCRS(StudyArea.b.reproj, WorldClim_2000)



#WorldClim.TV<-crop(WorldClim_2000, extent(StudyArea.b.reproj)) 
#WorldClim.TV<-raster::mask(WorldClim_2000.TV, StudyArea.b.reproj)

#Output_Folder<-"Q:/SarahHalperin/Data/WaterQuality/WorldClim/"

#writeRaster(WorldClim.TV,file=paste0(Output_Folder, "WorldClim06_TV"), format="GTiff", overwrite=TRUE) 

#---------------------------------------#
#WorldClim 1970-2000 average data: Clip and Avergae 
#-----------------------------------------#
Input_Folder<-"/Volumes/JodiBrandt/Common/SarahHalperin/Data/WaterQuality/WorldClim/WorldClim19702000/"

Input_Folder_List<-list.files(Input_Folder, pattern=".tif$", full.names = TRUE)

StudyArea<-readOGR("/Volumes/JodiBrandt/Common/SarahHalperin/Data/StudyArea/StudyArea_nad83.shp")
StudyArea.buffer<-buffer(StudyArea, width=5000)

WC<-lapply(Input_Folder_List,function(i){
  raster(i) #bring in all files in input folder 
})

StudyArea.b.reproj<-spTransform(StudyArea.buffer, crs(WC[[1]]))
identicalCRS(StudyArea.b.reproj, WC[[1]])

#WC_test<-projectRaster(WC[[1]], crs=crs(StudyArea), method="ngb")


WC.TV<-lapply(WC, function(i){
  crop(i, extent(StudyArea.b.reproj)) 
})

WC.tv<-lapply(WC.TV, function(i){
  raster::mask(i, StudyArea.b.reproj)
})
  

WCstack<-stack(WC.tv) #stack to then take average of the 12 months of precip data
WC.avg<-calc(WCstack, fun = mean) #take average

WC.avg.83<-projectRaster(WC.avg, crs=crs(StudyArea), method="ngb") #reproject to Nad83 

Output_Folder<-Input_Folder

writeRaster(WC.avg.83,file=paste0(Output_Folder, "WorldClim19702000_tv_avg"), format="GTiff", overwrite=TRUE) 

#-----------------------------------------------------_#
#DEM GEE 
#Folder_DEM<-"/Volumes/JodiBrandt/Common/SarahHalperin/Data/WaterQuality/DEM/GEE/"
#Input_Folder_List<-list.files(Folder_DEM, pattern=".tif$", full.names = TRUE)

#DEM.gee<-lapply(Input_Folder_List,function(i){
  #terra::rast(i) #bring in all files in input folder
#})

#DEM.1<-DEM.gee[[1]]
#DEM.2<-DEM.gee[[2]]
#DEM.3<-DEM.gee[[3]]
#DEM.4<-DEM.gee[[4]]
#DEM.5<-DEM.gee[[5]]
#DEM.6<-DEM.gee[[6]]
#DEM.7<-DEM.gee[[7]]
#DEM.8<-DEM.gee[[8]]
#DEM.9<-DEM.gee[[9]]
#DEM.10<-DEM.gee[[10]]
#DEM.11<-DEM.gee[[11]]
#DEM.12<-DEM.gee[[12]]
#DEM.13<-DEM.gee[[13]]
#DEM.14<-DEM.gee[[14]]

#DEM.R<-terra::merge(DEM.1, DEM.2, DEM.3, DEM.4, DEM.5, DEM.6, DEM.7, DEM.8, DEM.9, DEM.10, DEM.11, DEM.12, DEM.13, DEM.14)
#----------------------------------------------------#
#NED IU 
Folder_DEM<-"/Volumes/JodiBrandt/Common/SarahHalperin/Data/WaterQuality/DEM/NED_IU/"
Input_Folder_List<-list.files(Folder_DEM, pattern=".tif$", full.names = TRUE)

DEM.ned<-lapply(Input_Folder_List,function(i){
terra::rast(i) #bring in all files in input folder
})

DEM.1<-DEM.ned[[1]]
DEM.2<-DEM.ned[[2]]
DEM.3<-DEM.ned[[3]]
DEM.4<-DEM.ned[[4]]
DEM.5<-DEM.ned[[5]]
DEM.6<-DEM.ned[[6]]

DEM.R<-terra::merge(DEM.1, DEM.2, DEM.3, DEM.4, DEM.5, DEM.6)

Folder<-"/Volumes/JodiBrandt/Common/SarahHalperin/"
NED<-terra::rast(paste0(Folder,"Data/WaterQuality/DEM/NED_10m.tif" )) 

DEM.R.p<-terra::project(DEM.R, crs(NED))

writeRaster(DEM.R.p, file=paste0(Folder,"Data/WaterQuality/DEM/NED_IUDEM.tif"), overwrite=TRUE)


library(raster)
DEM.R.R<-terra::rast(paste0(Folder, "Data/WaterQuality/DEM/NED_IUDEM.tif"))


#Bring in StudyArea 
StudyArea.sp<-readOGR("/Volumes/JodiBrandt/Common/SarahHalperin/Data/StudyArea/StudyArea_nad83.shp")
StudyArea.Vec<-terra::vect(StudyArea.sp)

StudyArea.vec.buffer<-buffer(StudyArea.Vec, width=100000) #want to run InVEST with DEM larger than study area 
#crop
DEM.TV<-terra::crop(DEM.R.R, StudyArea.vec.buffer) 
DEM.TV.m<-terra::mask(DEM.R.R, StudyArea.vec.buffer)

writeRaster(DEM.TV.m, file=paste0(Folder,"Data/WaterQuality/DEM/NED_IUDEMTV.tif"), overwrite=TRUE)

#Fill DEM raster (always crashes)
#https://www.rdocumentation.org/packages/DMMF/versions/0.5.1.2/topics/SinkFill

#DEM.TV.R<-raster(DEM.TV.m)
#library(DMMF)
#DEM.fill<-SinkFill(DEM.TV.R, min_angle=0.00001)
#-------------------------------------------------------#
#Elevatr package NOT WORKING WHEN RUN IN INVEST 
install.packages("elevatr")
library(elevatr)

StudyArea.sp<-readOGR("/Volumes/JodiBrandt/Common/SarahHalperin/Data/StudyArea/StudyArea_nad83.shp")


#get elevation raster, automaticaclly mosaiced and projected 
DEM<-get_elev_raster(locations=StudyArea.sp, src="srtm15plus", z=12, expand=5000)
DEM.t<-terra::rast(DEM)

NED<-terra::rast(paste0(Folder,"Data/WaterQuality/DEM/NED_10m.tif" )) 

DEM.t<-terra::project(DEM.t, crs(NED))

writeRaster(DEM.t, file=paste0(Folder,"Data/WaterQuality/DEM/SRTM_DEM.tif"), overwrite=TRUE)
#used arc gis fill tool and ran in inVEST and does not work 
#------------------------------------------------------------------------#
#Sum Nitrogen n_export rasters

N2001<-raster("Q:/SarahHalperin/InVEST/WaterQuality/NLCD2001/02/n_export_2001.tif")
N2016<-raster("Q:/SarahHalperin/InVEST/WaterQuality/NLCD2016/n_export_01.tif")

N2001sum<-cellStats(N2001,'sum')
N2016sum<-cellStats(N2016,'sum')

#-----------------------------------------------#
#Karimi et al 2021 using equation (1-NDR)*load to turn NDR InVEST outputs into an ecoystem serivce. 


library(raster)
Folder<-"/Volumes/JodiBrandt/Common/SarahHalperin/"

#2016
NDR_n<-raster(paste0(Folder, "InVEST/WaterQuality/NLCD2016w/intermediate_outputs/ndr_n_01.tif"))
load_n<-raster(paste0(Folder, "InVEST/WaterQuality/NLCD2016w/intermediate_outputs/load_n_01.tif"))

NDR_p<-raster(paste0(Folder, "InVEST/WaterQuality/NLCD2016w/intermediate_outputs/ndr_p_01.tif"))
load_p<-raster(paste0(Folder, "InVEST/WaterQuality/NLCD2016w/intermediate_outputs/load_p_01.tif"))

Retention_n<-(1-NDR_n)*load_n
Retention_p<-(1-NDR_p)*load_p

writeRaster(Retention_n,file=paste0(Folder, "InVEST/WaterQuality/NLCD2016w/Retention2016_n"), format="GTiff", overwrite=TRUE)

writeRaster(Retention_p,file=paste0(Folder, "InVEST/WaterQuality/NLCD2016w/Retention2016_p"), format="GTiff", overwrite=TRUE)

#2001
NDR_n<-raster(paste0(Folder, "InVEST/WaterQuality/NLCD2001w/intermediate_outputs/ndr_n_01.tif"))
load_n<-raster(paste0(Folder, "InVEST/WaterQuality/NLCD2001w/intermediate_outputs/load_n_01.tif"))

NDR_p<-raster(paste0(Folder, "InVEST/WaterQuality/NLCD2001w/intermediate_outputs/ndr_p_01.tif"))
load_p<-raster(paste0(Folder, "InVEST/WaterQuality/NLCD2001w/intermediate_outputs/load_p_01.tif"))

Retention_n<-(1-NDR_n)*load_n
Retention_p<-(1-NDR_p)*load_p

writeRaster(Retention_n,file=paste0(Folder, "InVEST/WaterQuality/NLCD2001w/Retention2001_n"), format="GTiff", overwrite=TRUE)

writeRaster(Retention_p,file=paste0(Folder, "InVEST/WaterQuality/NLCD2001w/Retention2001_p"), format="GTiff", overwrite=TRUE)

#2030 
NDR_n<-raster(paste0(Folder, "InVEST/WaterQuality/NLCD2030/intermediate_outputs/ndr_n_01.tif"))
load_n<-raster(paste0(Folder, "InVEST/WaterQuality/NLCD2030/intermediate_outputs/load_n_01.tif"))

NDR_p<-raster(paste0(Folder, "InVEST/WaterQuality/NLCD2030/intermediate_outputs/ndr_p_01.tif"))
load_p<-raster(paste0(Folder, "InVEST/WaterQuality/NLCD2030/intermediate_outputs/load_p_01.tif"))

Retention_n<-(1-NDR_n)*load_n
Retention_p<-(1-NDR_p)*load_p

writeRaster(Retention_n,file=paste0(Folder, "InVEST/WaterQuality/NLCD2030/Retention2030_n"), format="GTiff", overwrite=TRUE)

writeRaster(Retention_p,file=paste0(Folder, "InVEST/WaterQuality/NLCD2030/Retention2030_p"), format="GTiff", overwrite=TRUE)
#2050 
NDR_n<-raster(paste0(Folder, "InVEST/WaterQuality/NLCD2050/intermediate_outputs/ndr_n_01.tif"))
load_n<-raster(paste0(Folder, "InVEST/WaterQuality/NLCD2050/intermediate_outputs/load_n_01.tif"))

NDR_p<-raster(paste0(Folder, "InVEST/WaterQuality/NLCD2050/intermediate_outputs/ndr_p_01.tif"))
load_p<-raster(paste0(Folder, "InVEST/WaterQuality/NLCD2050/intermediate_outputs/load_p_01.tif"))

Retention_n<-(1-NDR_n)*load_n
Retention_p<-(1-NDR_p)*load_p

writeRaster(Retention_n,file=paste0(Folder, "InVEST/WaterQuality/NLCD2050/Retention2050_n"), format="GTiff", overwrite=TRUE)

writeRaster(Retention_p,file=paste0(Folder, "InVEST/WaterQuality/NLCD2050/Retention2050_p"), format="GTiff", overwrite=TRUE)

#----------------------#
#Chaplin Paper Nitrogen Retention = Nitrogen Load- Nitrogen export and natures contribution = nitrogen retention/nitrogen load 
Folder<-"/Volumes/JodiBrandt/Common/SarahHalperin/"

#2001
load_n.c<-raster(paste0(Folder, "InVEST/WaterQuality/NLCD2001w/intermediate_outputs/load_n_30m.tif"))
export_n.c<-raster(paste0(Folder, "InVEST/WaterQuality/NLCD2001w/n_export_30m.tif"))

Retention.c<-load_n.c-export_n.c
NaturesContr<-Retention.c/load_n.c


writeRaster(Retention.c,file=paste0(Folder, "InVEST/WaterQuality/NLCD2001w/Retention2001_chaplin30m.tif"), overwrite=TRUE)
writeRaster(NaturesContr,file=paste0(Folder, "InVEST/WaterQuality/NLCD2001w/NaturesC2001_chaplin30m.tif"), overwrite=TRUE)



#2016
load_n.c<-raster(paste0(Folder, "InVEST/WaterQuality/NLCD2016w/intermediate_outputs/load_n_30m.tif"))
export_n.c<-raster(paste0(Folder, "InVEST/WaterQuality/NLCD2016w/n_export_30m.tif"))

Retention.c<-load_n.c-export_n.c
NaturesContr<-Retention.c/load_n.c

writeRaster(Retention.c,file=paste0(Folder, "InVEST/WaterQuality/NLCD2016w/Retention2016_chaplin30m"), format="GTiff", overwrite=TRUE)
writeRaster(NaturesContr,file=paste0(Folder, "InVEST/WaterQuality/NLCD2016w/NaturesC2016_chaplin30m"), format="GTiff", overwrite=TRUE)

#2030 
load_n.c<-raster(paste0(Folder, "InVEST/WaterQuality/NLCD2030/intermediate_outputs/load_n_30m.tif"))
export_n.c<-raster(paste0(Folder, "InVEST/WaterQuality/NLCD2030/n_export_30m.tif"))

Retention.c<-load_n.c-export_n.c
NaturesContr<-Retention.c/load_n.c

writeRaster(Retention.c,file=paste0(Folder, "InVEST/WaterQuality/NLCD2030/Retention2030_chaplin30m"), format="GTiff", overwrite=TRUE)
writeRaster(NaturesContr,file=paste0(Folder, "InVEST/WaterQuality/NLCD2030/NaturesC2030_chaplin30m"), format="GTiff", overwrite=TRUE)

#2050 
load_n.c<-raster(paste0(Folder, "InVEST/WaterQuality/NLCD2050/intermediate_outputs/load_n_30m.tif"))
export_n.c<-raster(paste0(Folder, "InVEST/WaterQuality/NLCD2050/n_export_30m.tif"))

Retention.c<-load_n.c-export_n.c
NaturesContr<-Retention.c/load_n.c

writeRaster(Retention.c,file=paste0(Folder, "InVEST/WaterQuality/NLCD2050/Retention2050_chaplin30m"), format="GTiff", overwrite=TRUE)
writeRaster(NaturesContr,file=paste0(Folder, "InVEST/WaterQuality/NLCD2050/NaturesC2050_chaplin30m"), format="GTiff", overwrite=TRUE)



