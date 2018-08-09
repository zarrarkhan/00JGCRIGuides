
#______________________
# Introduction
#______________________

rm(list=ls()) # Clear all old variables
graphics.off() # Turn of Graphics

# Load Libraries
packagesX     <-c("ggplot2","RColorBrewer","reshape2","magrittr","plyr","dplyr","tools","scales","rgcam","rgdal","rgeos","raster","tmap","animation","tis","tibble","classInt","sp","geosphere","Hmisc","gridExtra","grid")
for(i in packagesX){if(i %in% rownames(installed.packages()) == FALSE){install.packages(i)}
  library(i,character.only = TRUE)}

#_______________________
# Working Direcotries and Data
#______________________


wd0<<-getwd();wd0
wdsp<<-"D:/00SpatialData/"
wdfigsOut<-paste(wd0,"/fig_outputs",sep="");wdfigsOut;
deleteOldFigs<-1
if(deleteOldFigs==1){if(dir.exists(wdfigsOut)){unlink(wdfigsOut,recursive=T)}}
if(!dir.exists(wdfigsOut)){dir.create(wdfigsOut)}  # Output Directory
# Supporting shapefiles directories (Admin/Basin)

#______________________
# Functions Themes
#______________________
# ggplot2 Theme
z_theme <<- theme_bw() + 
  theme(
    text =                element_text(family = NULL, face = "plain",colour = "black", size = 24 ,hjust = 0.5, vjust = 0.5, angle = 0, lineheight = 0.9)
    , axis.text.x =       element_text(size=24)
    , axis.text.y =       element_text(size=24)
    ,axis.title.x =       element_text(vjust = -1, margin=margin(t=1,unit="line"))
    ,axis.title.y =       element_text(angle = 90, vjust = 2, margin=margin(r=1,unit="line"))
    ,legend.key =         element_blank()
    ,legend.key.size =    unit(1.5, 'lines')
    ,legend.text =        element_text(size = rel(1.0), colour = "black")
    ,legend.title =       element_text(size = rel(1.2), face = NULL, hjust = 0, colour = "black")
    ,strip.background =   element_rect(fill = NA, colour = "black")
    ,plot.margin =        unit(c(1, 1, 1, 1), "lines")
    ,plot.title=          element_text(face="bold", hjust=0,size=40,margin = margin(b=20))
  )

#______________________
# Read in Spatial Data
#______________________
#GADM 
gadm36L1<<-readOGR(paste(dirname(dirname(wd0)),"/gadm",sep=""),"gadm36_1",use_iconv=T,encoding='UTF-8')
#natural earth
ne10mAdmin0<<-readOGR(paste(dirname(dirname(wd0)),"/naturalEarth/ne_10m_admin_0_countries_lakes",sep=""),"ne_10m_admin_0_countries_lakes",use_iconv=T,encoding='UTF-8')
shp_PNNL235CLM5ArcMin_multi<<-readOGR(paste(wdsp,"/boundaries_PNNLChrisVernon/shp",sep=""),"Global235_CLM_final_5arcmin_multipart",use_iconv=T,encoding='UTF-8')
shp_PNNL32Reg<<-readOGR(paste(wdsp,"/boundaries_PNNLChrisVernon/shp",sep=""),"region32_0p5deg",use_iconv=T,encoding='UTF-8')

LookupTable_PNNL32Region<<-data.frame(reg32_id=c(0:32),
                                      GCAM_region=c("0","USA","Africa_Eastern","Africa_Northern","Africa_Southern",
                                                    "Africa_Western","Australia_NZ","Brazil","Canada",
                                                    "Central America and Caribbean","Central Asia","China","EU-12",
                                                    "EU-15","Europe_Eastern","Europe_Non_EU","European Free Trade Association",
                                                    "India","Indonesia","Japan","Mexico",
                                                    "Middle East","Pakistan","Russia","South Africa",
                                                    "South America Northern","South America_Southern","South Asia","South Korea",
                                                    "Southeast Asia","Taiwan","Argentina","Colombia"))

shp_PNNL32Reg@data <-join(shp_PNNL32Reg@data,LookupTable_PNNL32Region,by=c("reg32_id")); head(shp_PNNL32Reg@data)



#______________________
# Set projection layer
#______________________

#projX<<-proj4string(gadm36L1) # Setting to HydroBASINS layer
projX<<-proj4string(ne10mAdmin0) # Setting to HydroBASINS layer
gadm36L1<<-spTransform(gadm36L1, CRS(projX))
shp_PNNL235CLM5ArcMin_multi<<-spTransform(shp_PNNL235CLM5ArcMin_multi, CRS(projX))
shp_PNNL32Reg<<-spTransform(shp_PNNL32Reg, CRS(projX))

#______________________
# Create Basic Shapes to cut along
#______________________


region_i<-"Argentina"

shp1<-ne10mAdmin0
shp2<-gadm36L1
shpGCAMBasin<-shp_PNNL235CLM5ArcMin_multi
shpGCAMReg<-shp_PNNL32Reg


# GCAM LAC Regions
LAC_reg<-c("Argentina","Brazil","Central America and Caribbean","Colombia","Mexico","South America Northern","South America_Southern")
shp_PNNL_LAC<-shp_PNNL32Reg[shp_PNNL32Reg$GCAM_region %in% LAC_reg,]
shp_PNNL_LAC@data<-droplevels(shp_PNNL_LAC@data)

m0GCAMLAC<-qtm(shp_PNNL_LAC,fill="GCAM_region")+tm_layout(legend.show=T,frame=F,title="",title.size=2,title.position=c("right","top"),legend.outside = T);
m0GCAMLAC

shp_PNNL_LACBasins<-raster::crop(shp_PNNL235Basins,shp_PNNL_LAC)
shp_PNNL_LACBasins@data<-droplevels(shp_PNNL_LACBasins@data);
head(shp_PNNL_LACBasins)
m0GCAMLACBasins<-qtm(shp_PNNL_LACBasins,fill="basin_name",text="basin_id")+tm_layout(legend.show=T,frame=F,title="",title.size=2,title.position=c("right","top"),legend.outside=T,legend.outside.position = "right", legend.text.size = 1)
m0GCAMLACBasins

m3<-qtm(shpGCAMBasin,text="basin_id",fill="basin_id")+tm_layout(legend.show=F,frame=F);m3

shpa1<-shp1  # natural Earth Provinces
shpa1<-shpa1[shpa1$ADMIN==region_i,]
shpa1@data<-droplevels(shpa1@data);head(shpa1)
m1<-qtm(shpa1,text="ADMIN",fill="ADMIN")+tm_layout(legend.show=F,frame=F);m1

shpa2<-shp2  # natural Earth Provinces
shpa2<-shpa2[shpa2$NAME_0==region_i,]
shpa2@data<-droplevels(shpa2@data);head(shpa2)
m2<-qtm(shpa2,text="NAME_1",fill="NAME_1")+tm_layout(legend.show=F,frame=F);m2

#larger bounding box
b1<-as.data.frame(bbox(shpa1))   # Get Bounding box
expandbyPercent<-2; b1$min;b1$max
b1$min[1]<-if(b1$min[1]<0){(1+expandbyPercent/100)*b1$min[1]}else{(1-expandbyPercent/100)*b1$min[1]};
b1$min[2]<-if(b1$min[2]<0){(1+expandbyPercent/100)*b1$min[2]}else{(1-expandbyPercent/100)*b1$min[2]};
b1$max[1]<-if(b1$max[1]<0){(1-expandbyPercent/100)*b1$max[1]}else{(1+expandbyPercent/100)*b1$max[1]};
b1$max[2]<-if(b1$max[2]<0){(1-expandbyPercent/100)*b1$max[2]}else{(1+expandbyPercent/100)*b1$max[2]};
b1$min;b1$max;
b1<-as(extent(as.vector(t(b1))), "SpatialPolygons")
proj4string(b1)<-CRS(projX) # ASSIGN COORDINATE SYSTEM

# GADM Boundaries Bounding Box
shpb1<-raster::crop(shp1,b1)
shpb1@data<-droplevels(shpb1@data)

tm1<-system.time({
m1a<-qtm(shpb1,text="ADMIN",fill="ADMIN")+tm_layout(legend.show=F,frame=F,title="",title.size=2,title.position=c("right","top"))
m1a
})
tm1[[3]]; m1a

# GADM Boundaries Bounding Box
shpb2<-raster::crop(shp2,b1)
shpb2@data<-droplevels(shpb1@data)

tm2<-system.time({
  m2a<-qtm(shpb2,text="NAME_0",fill="NAME_0")+tm_layout(legend.show=F,frame=F,title="",title.size=2,title.position=c("right","top"))
m2a
  })
tm2[[3]];m2a



