
#______________________
# Introduction
#______________________

# Testing for three methods of spatial aggregation
# i) Using Spatial Point Data Frames
# ii) Cropping a raster with a polygon using the raster area weights
# iii) Cropping a polygon grid and using the subsequent polygon area weights
# Conclusion: For depth(mm) need to use method iii and for volume(km3) need to use method ii

#https://gis.stackexchange.com/questions/235175/raster-extract-function-area-weighted-values
#https://stackoverflow.com/questions/26620373/spatialpolygons-creating-a-set-of-polygons-in-r-from-coordinates

#sink(file = "SelectedFigs/latex_CompareStates_km3.txt", append = FALSE, type = c("output"),
#     split = FALSE)
#latex(l1xa, cdec=c(0,2,2,2,4), na.blank=TRUE,
#      booktabs=TRUE, table.env=FALSE, center="none", file="", title="")
#sink()

#pdf(file="wdfigsOut/CompareBasins_TRWRRunoffkm3LogLog.pdf", height=10, width=10)
#dev.off()

#----------------------
# A.1 Clear Variables
#----------------------

rm(list=ls()) # Clear all old variables
graphics.off()

#______________________
# Install libraries
#______________________

packagesX<-c("ggplot2","RColorBrewer","reshape2","magrittr","plyr","dplyr","tools","scales","rgcam","rgdal",
             "rgeos","raster","tmap","animation","tis","tibble","classInt","sp","geosphere","Hmisc")
for(i in packagesX){if(i %in% rownames(installed.packages()) == FALSE) {install.packages(i)};library(i,character.only = TRUE)}

#______________________
# Working Direcotries and Data
#______________________


wd0<<-getwd();wd0
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
gadm36L1<<-readOGR(paste(wd0,"/gadm",sep=""),"gadm36_1")

#______________________
# Set projection layer
#______________________

projX<<-proj4string(gadm36L1) # Setting to HydroBASINS layer

#______________________
# Create Basic Shapes to cut along
#______________________

region_i<-"Argentina"

shpa1<-gadm36L1  # natural Earth Provinces
shpa1<-shpa1[shpa1$NAME_0==region_i,]
shpa1@data<-droplevels(shpa1@data);head(shpa1)
m<-qtm(shpa1,text="NAME_1",fill="NAME_1")+tm_layout(legend.show=F,frame=F)
pdf(file=paste(wdfigsOut,"/m1_region.pdf",sep=""), height=10, width=8)
m
dev.off()

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

#______________________
# Read in Raster Data
#______________________

# Read in Data Files (Tethys example file, Chosoe year X2005)
df<- read.csv(paste(getwd(),"/wdirr.csv",sep=""), stringsAsFactors = F)
colnames(df)[which(names(df) == "latitude")] <- "lat"
colnames(df)[which(names(df) == "longitude")] <- "lon"
df<-df%>%dplyr::select(X..ID,lat,lon,X2005)
head(df)

# CREATE RASTER FILE FOR DATA
r<-df%>%dplyr::select(lat,lon,X2005)
coordinates(r)=~lon+lat
gridded(r)<-T
r<-raster(r)
projection(r)<-projX

rcrop<-raster::intersect(r,b1)
rcropP<-rasterToPolygons(rcrop)
m<-tm_shape(rcrop)+tm_raster(col="X2005",style="kmeans",n=10)+tm_legend(outside = TRUE, text.size = .8)+
  tm_shape(rcropP)+tm_borders("gray40",lwd=0.2, lty=1)+tm_dots()+
  tm_shape(shpa1)+tm_borders("black",lwd=2, lty=1)+tm_fill("gray",alpha=0.1);m

pdf(file=paste(wdfigsOut,"/m2_polygonRaster.pdf",sep=""), height=10, width=8)
m
dev.off()

m<-tm_shape(rcrop)+tm_raster(col="X2005",style="kmeans",n=10)+tm_legend(outside = TRUE, text.size = .8)+
  tm_shape(rcropP)+tm_borders("gray40",lwd=0.2, lty=1)+
  tm_shape(shpa1)+tm_borders("black",lwd=2, lty=1)+tm_fill("gray",alpha=0.1);m

pdf(file=paste(wdfigsOut,"/m2a_polygonRasterNoDots.pdf",sep=""), height=10, width=8)
m
dev.off()

#______________________
# INITIAL ANALYSIS FOR REGION sum (Vol)  & mean (depth mm)
#______________________

# METHOD 1 -- SPATIAL POINTS DATA FRAME
tm<-system.time({  # system time
df1<-df
# Convert to Spatial Point Data Frames
df1 = SpatialPointsDataFrame(
  SpatialPoints(coords=(cbind(df1$lon,df1$lat))),
  data=df1
)
proj4string(df1)<-projX
df1;head(df1)
# Admin Boundary
dfp<-raster::intersect(df1,shpa1);plot(dfp)  # Crop to Layer shpa
gridded(dfp)<-TRUE  # Create Gridded Raster Data
## SUM
dxp<-as.data.frame(dfp@data)
dxp<- dxp %>% subset(select=c("X2005","NAME_1")) %>%
  group_by(NAME_1) %>% 
  summarise_all(funs(round(sum(.,na.rm=T),2))) %>% as.data.frame
head(dxp)
sum_m1<-dxp
## MEAN
dxp<-as.data.frame(dfp@data)
dxp<- dxp %>% subset(select=c("X2005","NAME_1")) %>%
  group_by(NAME_1) %>% 
  summarise_all(funs(round(mean(.,na.rm=T),2))) %>% as.data.frame
head(dxp)
mean_m1<-dxp
}) # close system time
names(sum_m1)[names(sum_m1)=="X2005"]<-"X2005m1"
names(mean_m1)[names(mean_m1)=="X2005"]<-"X2005m1"
sum_m1$timeSecm1<-tm[[3]]; head(sum_m1)
mean_m1$timeSecm1<-tm[[3]]; head(mean_m1)

# METHOD 2 -- RATSER EXTRACT (Weighted by RASTER AREA) FOR SUMS
tm<-system.time({  # system time
w <- raster::extract(r,shpa1, method="simple",weights=T, normalizeWeights=F)
head(w)
dfx<-data.frame()
for (i in seq(w)){
  x<-as.data.frame(w[[i]])
  x$ID<-shpa1@data$NAME_1[[i]]
  x$WeightedValue<-x$value*x$weight
  #assign(paste0("df", i), x)
  dfx<-rbind.data.frame(dfx,x)
}
names(dfx)[names(dfx)=="ID"]<-"NAME_1"
names(dfx)[names(dfx)=="WeightedValue"]<-"X2005m2"
head(dfx)
## SUM
dxp<- dfx %>% subset(select=c("X2005m2","NAME_1")) %>%
  group_by(NAME_1) %>% 
  summarise_all(funs(round(sum(.,na.rm=T),2))) %>% as.data.frame
head(dxp)
sum_m2<-dxp
}) # close system time
sum_m2$timeSecm2<-tm[[3]]; head(sum_m2)

# METHOD 3 -- RATSER to Polygon (Weighted by cropped raster polygon) for Means
tm<-system.time({  # system time
  x<-raster::intersect(shpa1,rcropP)
  plot(x);x
  x@data<-x@data%>%dplyr::select(NAME_1,X2005)
  x@data$area<-area(x); head(x@data)
  s1<-shpa1
  s1$subRegAreaSum<-area(shpa1);head(s1)
  s1<-s1@data%>%dplyr::select(NAME_1,subRegAreaSum);head(s1)
  x@data<-join(x@data,s1,by="NAME_1");head(x)
  x@data$areaPrcnt<-x@data$area/x@data$subRegAreaSum; 
  x@data$X2005m3<-x@data$X2005*x@data$areaPrcnt;head(x)
  ## MEAN
  dxp<- x@data %>% subset(select=c("X2005m3","NAME_1")) %>%
    group_by(NAME_1) %>% 
    summarise_all(funs(round(sum(.,na.rm=T),2))) %>% as.data.frame
  head(dxp)
  mean_m3<-dxp
}) # close system time
mean_m3$timeSecm3<-tm[[3]]; head(mean_m3)

# COMPARE SUMS-------------
comb<-join(sum_m1,sum_m2,by="NAME_1");comb<-join(comb,mean_m3,by="NAME_1"); head (comb)
combval<-melt(comb%>%dplyr::select(NAME_1,X2005m1,X2005m2), id.vars=c("NAME_1"), value.name="X2005",variable.name="Method");

p<-ggplot(combval,aes(x=NAME_1,y=X2005,fill=Method))+z_theme+geom_bar(stat="identity",position = position_dodge(width = 1))+ theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
p<-p+ggtitle("Compare Sums")+ylab("Value (2005 demands");p
pdf(file=paste(wdfigsOut,"/p_compareSums.pdf",sep=""), height=12, width=15)
p
dev.off()

combtime<-melt(comb%>%dplyr::select(NAME_1,timeSecm1,timeSecm2,timeSecm3), id.vars=c("NAME_1"), value.name="X2005",variable.name="Method");
p<-ggplot(combtime,aes(x=NAME_1,y=X2005,fill=Method))+z_theme+geom_bar(stat="identity",position = position_dodge(width = 1))+ theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
p<-p+ggtitle("Compare Time")+ylab("sec");p
pdf(file=paste(wdfigsOut,"/p_compareTimes.pdf",sep=""), height=12, width=15)
p
dev.off()

diff<-comb%>%dplyr::select(NAME_1,X2005m1,X2005m2); head(diff)
diff$diffm1m2<-round((diff$X2005m2-diff$X2005m1),1); head(diff)
diff$diffPrcntm1m2<-round((diff$X2005m2-diff$X2005m1)*100/diff$X2005m1,1); head(diff)
p<-ggplot(diff,aes(x=NAME_1,y=diffPrcntm1m2,fill="red"))+z_theme+geom_bar(stat="identity",position = position_dodge(width = 1))+ theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
p<-p+ggtitle("% Difference between m1 & m2 for sums")+ylab("%");p
pdf(file=paste(wdfigsOut,"/p_compareSumsm1m2Prcnt.pdf",sep=""), height=12, width=15)
p
dev.off()
diffSum<-diff

shpa.x<-shpa1
shpa.x@data<-join(shpa.x@data,diff,by=c("NAME_1")) %>% 
  subset(select=c("X2005m1","X2005m2","NAME_1"))
tm1<-qtm(shpa.x,fill="X2005m1",text="NAME_1")+tm_layout(frame=F);
tm2<-qtm(shpa.x,fill="X2005m2",text="NAME_1")+tm_layout(frame=F);
pdf(file=paste(wdfigsOut,"/m_sumsm1m2.pdf",sep=""), height=10, width=13)
tmap_arrange(tm1, tm2,asp = NA,ncol=2)
dev.off()

shpa.x<-shpa1
shpa.x@data<-join(shpa.x@data,diff,by=c("NAME_1")) %>%
  subset(select=c("diffm1m2","NAME_1"));head(shpa.x) 
tmdiffSum<-qtm(shpa.x,fill="diffm1m2",text="NAME_1")+tm_borders("gray40",lwd=0.2, lty=1)+tm_layout(frame=F);
pdf(file=paste(wdfigsOut,"/m_sumsDiffm1m2.pdf",sep=""), height=10, width=8)
tmdiffSum
dev.off()

shpa.x<-shpa1
shpa.x@data<-join(shpa.x@data,diff,by=c("NAME_1")) %>%
  subset(select=c("diffPrcntm1m2","NAME_1"));head(shpa.x) 
tmdiffSum<-qtm(shpa.x,fill="diffPrcntm1m2",text="NAME_1")+tm_borders("gray40",lwd=0.2, lty=1)+tm_layout(frame=F);
pdf(file=paste(wdfigsOut,"/m_sumsDiffPrcntm1m2.pdf",sep=""), height=10, width=8)
tmdiffSum
dev.off()

# COMPARE MEANS-------------
comb<-join(mean_m1,mean_m3, by="NAME_1"); head (comb)
combval<-melt(comb%>%dplyr::select(NAME_1,X2005m1,X2005m3), id.vars=c("NAME_1"), value.name="X2005",variable.name="Method");
p<-ggplot(combval,aes(x=NAME_1,y=X2005,fill=Method))+z_theme+geom_bar(stat="identity",position = position_dodge(width = 1))+ theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
p<-p+ggtitle("Compare means")+ylab("Value (2005 demands");p
pdf(file=paste(wdfigsOut,"/p_compareMeans.pdf",sep=""), height=12, width=15)
p
dev.off()

diff<-comb%>%dplyr::select(NAME_1,X2005m1,X2005m3); head(diff)
diff$diffm1m3<-round((diff$X2005m3-diff$X2005m1),1); head(diff)
diff$diffPrcntm1m3<-round((diff$X2005m3-diff$X2005m1)*100/diff$X2005m1,1); head(diff)
ggplot(diff,aes(x=NAME_1,y=diffPrcntm1m3,fill="red"))+geom_bar(stat="identity",position = position_dodge(width = 1))+ theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
p<-p+ggtitle("% Difference between m1 & m2 for Means")+ylab("%");p
pdf(file=paste(wdfigsOut,"/p_compareMeansm1m2Prcnt.pdf",sep=""), height=12, width=15)
p
dev.off()
diffMean<-diff

shpa.x<-shpa1
shpa.x@data<-join(shpa.x@data,diff,by=c("NAME_1")) %>% 
  subset(select=c("X2005m1","X2005m3","NAME_1"))
tm1<-qtm(shpa.x,fill="X2005m1",text="NAME_1");
tm3<-qtm(shpa.x,fill="X2005m3",text="NAME_1");
tmap_arrange(tm1,tm3,asp = NA,ncol=2)

shpa.x<-shpa1
shpa.x@data<-join(shpa.x@data,diff,by=c("NAME_1")) %>% 
  subset(select=c("diffPrcntm1m3","NAME_1"));head(shpa.x)
tmdiffPrcntMean<-qtm(shpa.x,fill="diffPrcntm1m3",text="NAME_1")+tm_borders("gray40",lwd=0.2, lty=1)+tm_layout(frame=F);
pdf(file=paste(wdfigsOut,"/m_meansDiffPrcntm1m2.pdf",sep=""), height=10, width=8)
tmdiffPrcntMean
dev.off()

shpa.x<-shpa1
shpa.x@data<-join(shpa.x@data,diff,by=c("NAME_1")) %>% 
  subset(select=c("diffm1m3","NAME_1"));head(shpa.x)
tmdiffMean<-qtm(shpa.x,fill="diffm1m3",text="NAME_1")+tm_borders("gray40",lwd=0.2, lty=1)+tm_layout(frame=F);
pdf(file=paste(wdfigsOut,"/m_meansDiffm1m2.pdf",sep=""), height=10, width=8)
tmdiffMean
dev.off()

#shpa.x<-shpa1
#shpa.x@data<-join(shpa.x@data,diff,by=c("NAME_1")) %>% 
#  subset(select=c("diffm1m2","diffm1m3","NAME_1"));head(shpa.x)
#scaleData<-as.numeric(as.vector(as.matrix((shpa.x@data%>%dplyr::select(-NAME_1)))),na.rm=T);scaleData;
#scaleData<-scaleData[!is.na(scaleData)];scaleData
#breakx<-classIntervals(scaleData, n=10, style = "kmeans")[2]$brks
#tm1<-tm_shape(shpa.x) + tm_fill("diffm1m2",text="NAME_1",style="fixed",breaks=breakx,legend.show=T,showNA=F)+
#  tm_borders("gray40",lwd=0.2, lty=1)+tm_layout(frame=F);



#------------------------------------------------------------
#-------------------------------------------------------------
# Zoom Subregion
diff[order(diff$diffm1m2),];diff[order(diff$diffm1m3),]
zoomR<-"Formosa"

# Zoom
dev.off()
b1<-as.data.frame(bbox(shpa1[shpa1$NAME_1==zoomR,]))   # Get Bounding box
expandbyPercent<-2; b1$min;b1$max
b1$min[1]<-if(b1$min[1]<0){(1+expandbyPercent/100)*b1$min[1]}else{(1-expandbyPercent/100)*b1$min[1]};
b1$min[2]<-if(b1$min[2]<0){(1+expandbyPercent/100)*b1$min[2]}else{(1-expandbyPercent/100)*b1$min[2]};
b1$max[1]<-if(b1$max[1]<0){(1-expandbyPercent/100)*b1$max[1]}else{(1+expandbyPercent/100)*b1$max[1]};
b1$max[2]<-if(b1$max[2]<0){(1-expandbyPercent/100)*b1$max[2]}else{(1+expandbyPercent/100)*b1$max[2]};
b1$min;b1$max;
b1<-as(extent(as.vector(t(b1))), "SpatialPolygons")
proj4string(b1)<-CRS(projX) # ASSIGN COORDINATE SYSTEM
emx<-raster::crop(shpa1,b1)
rem<-raster::intersect(r,emx);
remG<-rasterToPolygons(rem)
tm<-tm_shape(rem)+tm_raster(col="X2005",style="kmeans",n=10)+tm_legend(outside = TRUE, text.size = .8)+
  tm_shape(remG)+tm_borders("gray40",lwd=0.2, lty=1)+tm_dots()+
  tm_shape(emx)+tm_borders("black",lwd=3, lty=1)+tm_fill("gray",alpha=0.1)+
  tm_text("NAME_1",scale=1.2,auto.placement=F, col="black")
tm


#______________________
# COMPARE AGAIN USING NEW BOUNDS
# raster is now rem, shapefile is now emx, rcropP is remG
#______________________
dev.off()

shpa1<-emx
r<-rem
rcropP<-remG


# METHOD 1 -- SPATIAL POINTS DATA FRAME
tm<-system.time({  # system time
  df1<-df
  # Convert to Spatial Point Data Frames
  df1 = SpatialPointsDataFrame(
    SpatialPoints(coords=(cbind(df1$lon,df1$lat))),
    data=df1
  )
  proj4string(df1)<-projX
  df1;head(df1)
  # Admin Boundary
  dfp<-raster::intersect(df1,shpa1);plot(dfp)  # Crop to Layer shpa
  gridded(dfp)<-TRUE  # Create Gridded Raster Data
  data_m1<-dfp
  ## SUM
  dxp<-as.data.frame(dfp@data)
  dxp<- dxp %>% subset(select=c("X2005","NAME_1")) %>%
    group_by(NAME_1) %>% 
    summarise_all(funs(round(sum(.,na.rm=T),2))) %>% as.data.frame
  head(dxp)
  sum_m1<-dxp
  ## MEAN
  dxp<-as.data.frame(dfp@data)
  dxp<- dxp %>% subset(select=c("X2005","NAME_1")) %>%
    group_by(NAME_1) %>% 
    summarise_all(funs(round(mean(.,na.rm=T),2))) %>% as.data.frame
  head(dxp)
  mean_m1<-dxp
}) # close system time
names(sum_m1)[names(sum_m1)=="X2005"]<-"X2005m1"
names(mean_m1)[names(mean_m1)=="X2005"]<-"X2005m1"
sum_m1$timeSecm1<-tm[[3]]; head(sum_m1)
mean_m1$timeSecm1<-tm[[3]]; head(mean_m1)

# METHOD 2 -- RATSER EXTRACT (Weighted by RASTER AREA) FOR SUMS
tm<-system.time({  # system time
  w <- raster::extract(r,shpa1, method="simple",weights=T, normalizeWeights=F)
  head(w)
  dfx<-data.frame()
  for (i in seq(w)){
    x<-as.data.frame(w[[i]])
    x$ID<-shpa1@data$NAME_1[[i]]
    x$WeightedValue<-x$value*x$weight
    #assign(paste0("df", i), x)
    dfx<-rbind.data.frame(dfx,x)
  }
  names(dfx)[names(dfx)=="ID"]<-"NAME_1"
  names(dfx)[names(dfx)=="WeightedValue"]<-"X2005m2"
  head(dfx)
  data_m2<-dfx
  ## SUM
  dxp<- dfx %>% subset(select=c("X2005m2","NAME_1")) %>%
    group_by(NAME_1) %>% 
    summarise_all(funs(round(sum(.,na.rm=T),2))) %>% as.data.frame
  head(dxp)
  sum_m2<-dxp
}) # close system time
sum_m2$timeSecm2<-tm[[3]]; head(sum_m2)

# METHOD 3 -- RATSER to Polygon (Weighted by cropped raster polygon) for Means
tm<-system.time({  # system time
  x<-raster::intersect(shpa1,rcropP)
  plot(x);x
  map_m3<-x
  x@data<-x@data%>%dplyr::select(NAME_1,X2005)
  x@data$area<-area(x); head(x@data)
  s1<-shpa1
  s1$subRegAreaSum<-area(shpa1);head(s1)
  s1<-s1@data%>%dplyr::select(NAME_1,subRegAreaSum);head(s1)
  x@data<-join(x@data,s1,by="NAME_1");head(x)
  x@data$areaPrcnt<-x@data$area/x@data$subRegAreaSum; 
  x@data$X2005m3<-x@data$X2005*x@data$areaPrcnt;head(x)
  data_m3<-x@data
  ## MEAN
  dxp<- x@data %>% subset(select=c("X2005m3","NAME_1")) %>%
    group_by(NAME_1) %>% 
    summarise_all(funs(round(sum(.,na.rm=T),2))) %>% as.data.frame
  head(dxp)
  mean_m3<-dxp
}) # close system time
mean_m3$timeSecm3<-tm[[3]]; head(mean_m3)

#COMPARE DATA
data_m1<-data_m1@data[data_m1@data$NAME_1==zoomR,]%>%dplyr::select(NAME_1,X2005);data_m1
data_m2<-data_m2[data_m2$NAME_1==zoomR,];data_m2
data_m3<-data_m3[data_m3$NAME_1==zoomR,];data_m3
nrow(data_m1);nrow(data_m2);nrow(data_m3)
sum(data_m1$X2005);sum(data_m2$X2005m2);
mean(data_m1$X2005);sum(data_m3$X2005m3)

#COMPARE METHOD 3 map_m3
tm1<-tm_shape(rem)+tm_raster(col="X2005",style="kmeans",n=10)+tm_legend(title="M1",outside = TRUE, text.size = .8)+
  tm_shape(remG)+tm_borders("gray40",lwd=0.2, lty=1)+tm_dots()+
  tm_shape(emx)+tm_borders("black",lwd=0.5, lty=1)+tm_fill("gray",alpha=0.1)+
  tm_text("NAME_1",scale=1.2,auto.placement=F, col="black");tm1
tm3<-tm_shape(map_m3)+tm_fill("X2005",style="kmeans",n=10)+tm_dots()+tm_legend(title="M3",outside = TRUE, text.size = .8)+
  tm_borders("gray40",lwd=0.2, lty=1)+tm_layout(frame=F);tm3
pdf(file=paste(wdfigsOut,"/mZ_m1m3.pdf",sep=""), height=10, width=13)
tmap_arrange(tm1, tm3,asp = NA,ncol=2)
dev.off()



# COMPARE SUMS-------------
comb<-join(sum_m1,sum_m2,by="NAME_1");comb<-join(comb,mean_m3,by="NAME_1"); head (comb)
combval<-melt(comb%>%dplyr::select(NAME_1,X2005m1,X2005m2), id.vars=c("NAME_1"), value.name="X2005",variable.name="Method");

p<-ggplot(combval,aes(x=NAME_1,y=X2005,fill=Method))+z_theme+geom_bar(stat="identity",position = position_dodge(width = 1))+ theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
p<-p+ggtitle("Compare Sums")+ylab("Value (2005 demands");p
pdf(file=paste(wdfigsOut,"/pZ_compareSums.pdf",sep=""), height=12, width=15)
p
dev.off()

combtime<-melt(comb%>%dplyr::select(NAME_1,timeSecm1,timeSecm2,timeSecm3), id.vars=c("NAME_1"), value.name="X2005",variable.name="Method");
p<-ggplot(combtime,aes(x=NAME_1,y=X2005,fill=Method))+z_theme+geom_bar(stat="identity",position = position_dodge(width = 1))+ theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
p<-p+ggtitle("Compare Time")+ylab("sec");p
pdf(file=paste(wdfigsOut,"/pZ_compareTimes.pdf",sep=""), height=12, width=15)
p
dev.off()

diff<-comb%>%dplyr::select(NAME_1,X2005m1,X2005m2); head(diff)
diff$diffm1m2<-round((diff$X2005m2-diff$X2005m1),1); head(diff)
diff$diffPrcntm1m2<-round((diff$X2005m2-diff$X2005m1)*100/diff$X2005m1,1); head(diff)
p<-ggplot(diff,aes(x=NAME_1,y=diffPrcntm1m2,fill="red"))+z_theme+geom_bar(stat="identity",position = position_dodge(width = 1))+ theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
p<-p+ggtitle("% Difference between m1 & m2 for sums")+ylab("%");p
pdf(file=paste(wdfigsOut,"/pZ_compareSumsm1m2Prcnt.pdf",sep=""), height=12, width=15)
p
dev.off()
diff
diffSumZ<-diff

shpa.x<-shpa1
shpa.x@data<-join(shpa.x@data,diff,by=c("NAME_1")) %>% 
  subset(select=c("X2005m1","X2005m2","NAME_1"))
tm1<-qtm(shpa.x,fill="X2005m1",text="NAME_1")+tm_layout(frame=F);
tm2<-qtm(shpa.x,fill="X2005m2",text="NAME_1")+tm_layout(frame=F);
pdf(file=paste(wdfigsOut,"/mZ_sumsm1m2.pdf",sep=""), height=10, width=13)
tmap_arrange(tm1, tm2,asp = NA,ncol=2)
dev.off()

shpa.x<-shpa1
shpa.x@data<-join(shpa.x@data,diff,by=c("NAME_1")) %>%
  subset(select=c("diffm1m2","NAME_1"));head(shpa.x) 
tmdiffSum<-qtm(shpa.x,fill="diffm1m2",text="NAME_1")+tm_borders("gray40",lwd=0.2, lty=1)+tm_layout(frame=F);
pdf(file=paste(wdfigsOut,"/mZ_sumsDiffm1m2.pdf",sep=""), height=10, width=8)
tmdiffSum
dev.off()

shpa.x<-shpa1
shpa.x@data<-join(shpa.x@data,diff,by=c("NAME_1")) %>%
  subset(select=c("diffPrcntm1m2","NAME_1"));head(shpa.x) 
tmdiffSum<-qtm(shpa.x,fill="diffPrcntm1m2",text="NAME_1")+tm_borders("gray40",lwd=0.2, lty=1)+tm_layout(frame=F);
pdf(file=paste(wdfigsOut,"/mZ_sumsDiffPrcntm1m2.pdf",sep=""), height=10, width=8)
tmdiffSum
dev.off()

# COMPARE MEANS-------------
comb<-join(mean_m1,mean_m3, by="NAME_1"); head (comb)
combval<-melt(comb%>%dplyr::select(NAME_1,X2005m1,X2005m3), id.vars=c("NAME_1"), value.name="X2005",variable.name="Method");
p<-ggplot(combval,aes(x=NAME_1,y=X2005,fill=Method))+z_theme+geom_bar(stat="identity",position = position_dodge(width = 1))+ theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
p<-p+ggtitle("Compare means")+ylab("Value (2005 demands");p
pdf(file=paste(wdfigsOut,"/pZ_compareMeans.pdf",sep=""), height=12, width=15)
p
dev.off()

diff<-comb%>%dplyr::select(NAME_1,X2005m1,X2005m3); head(diff)
diff$diffm1m3<-round((diff$X2005m3-diff$X2005m1),1); head(diff)
diff$diffPrcntm1m3<-round((diff$X2005m3-diff$X2005m1)*100/diff$X2005m1,1); head(diff)
ggplot(diff,aes(x=NAME_1,y=diffPrcntm1m3,fill="red"))+geom_bar(stat="identity",position = position_dodge(width = 1))+ theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
p<-p+ggtitle("% Difference between m1 & m2 for Means")+ylab("%");p
pdf(file=paste(wdfigsOut,"/pZ_compareMeansm1m2Prcnt.pdf",sep=""), height=12, width=15)
p
dev.off()
diff
diffMeanZ<-diff

shpa.x<-shpa1
shpa.x@data<-join(shpa.x@data,diff,by=c("NAME_1")) %>% 
  subset(select=c("X2005m1","X2005m3","NAME_1"))
tm1<-qtm(shpa.x,fill="X2005m1",text="NAME_1");
tm3<-qtm(shpa.x,fill="X2005m3",text="NAME_1");
tmap_arrange(tm1,tm3,asp = NA,ncol=2)

shpa.x<-shpa1
shpa.x@data<-join(shpa.x@data,diff,by=c("NAME_1")) %>% 
  subset(select=c("diffPrcntm1m3","NAME_1"));head(shpa.x)
tmdiffPrcntMean<-qtm(shpa.x,fill="diffPrcntm1m3",text="NAME_1")+tm_borders("gray40",lwd=0.2, lty=1)+tm_layout(frame=F);
pdf(file=paste(wdfigsOut,"/mZ_meansDiffPrcntm1m2.pdf",sep=""), height=10, width=8)
tmdiffPrcntMean
dev.off()

shpa.x<-shpa1
shpa.x@data<-join(shpa.x@data,diff,by=c("NAME_1")) %>% 
  subset(select=c("diffm1m3","NAME_1"));head(shpa.x)
tmdiffMean<-qtm(shpa.x,fill="diffm1m3",text="NAME_1")+tm_borders("gray40",lwd=0.2, lty=1)+tm_layout(frame=F);
pdf(file=paste(wdfigsOut,"/mZ_meansDiffm1m2.pdf",sep=""), height=10, width=8)
tmdiffMean
dev.off()

diffSum;diffMean;diffSumZ;diffMeanZ

sink(file = paste(wdfigsOut,"/latex_diffSum.txt",sep=""), append = FALSE, type = c("output"),split = FALSE)
latex(diffSum, cdec=c(0,2,2,2,4), na.blank=TRUE,booktabs=TRUE, table.env=FALSE, center="none", file="", title="")
sink()

sink(file = paste(wdfigsOut,"/latex_diffMean.txt",sep=""), append = FALSE, type = c("output"),split = FALSE)
latex(diffMean, cdec=c(0,2,2,2,4), na.blank=TRUE,booktabs=TRUE, table.env=FALSE, center="none", file="", title="")
sink()

sink(file = paste(wdfigsOut,"/latex_diffSumZ.txt",sep=""), append = FALSE, type = c("output"),split = FALSE)
latex(diffSumZ, cdec=c(0,2,2,2,4), na.blank=TRUE,booktabs=TRUE, table.env=FALSE, center="none", file="", title="")
sink()

sink(file = paste(wdfigsOut,"/latex_diffMeanZ.txt",sep=""), append = FALSE, type = c("output"),split = FALSE)
latex(diffMeanZ, cdec=c(0,2,2,2,4), na.blank=TRUE,booktabs=TRUE, table.env=FALSE, center="none", file="", title="")
sink()


#shpa.x<-shpa1
#shpa.x@data<-join(shpa.x@data,diff,by=c("NAME_1")) %>% 
#  subset(select=c("diffm1m2","diffm1m3","NAME_1"));head(shpa.x)
#scaleData<-as.numeric(as.vector(as.matrix((shpa.x@data%>%dplyr::select(-NAME_1)))),na.rm=T);scaleData;
#scaleData<-scaleData[!is.na(scaleData)];scaleData
#breakx<-classIntervals(scaleData, n=10, style = "kmeans")[2]$brks
#tm1<-tm_shape(shpa.x) + tm_fill("diffm1m2",text="NAME_1",style="fixed",breaks=breakx,legend.show=T,showNA=F)+
#  tm_borders("gray40",lwd=0.2, lty=1)+tm_layout(frame=F);
