#LIBRARY

library(dplyr)
library(kohonen)
library(wesanderson)
library(RColorBrewer)
library(data.table)
library(factoextra)





# IMPORT DATA
DATI <- read.csv("FINAL3.csv", sep = ";", dec = ",")
#DATI<-DATI[,1:154]
rownames(DATI) <- DATI[,1]

summary(DATI)
#DATI[,154]<-as.numeric(DATI[,154])

final<-as.matrix(DATI[,2:47])

#final<-as.numeric(final)

for (i in 1:dim(final)[1]) {
  for (j in 1:dim(final)[2]) {
    
    ifelse(final[i,45]==0,0,final[i,j]<-final[i,j]/final[i,45]) 
    
  }
  
}



final<-scale(final)
final<-final[,1:45]



# Create the SOM Grid - you generally have to specify the size of the 
# training grid prior to training the SOM. Hexagonal and Circular 
# topologies are possible
# x*y deve essere la dimensione del numero di righe del datasets
#som_grid1 <- somgrid(3, 4, topo="hexagonal")
#som_grid2<- somgrid(3, 5, topo="hexagonal")
#som_grid3<- somgrid(5, 6, topo="hexagonal")

# Finally, train the SOM, options for the number of iterations,
# the learning rates, and the neighbourhood are available

coolBlueHotRed <- function(n, alpha = 1) {rainbow(n, end=4/6, alpha=alpha)[n:1]}
#heatcols <- heat.colors(6)

#set.seed(1234)

#137 regioni 68 settori nodes= 5*sqr (rows)= 5*sqr(137)=5*11,7=58

set.seed(12)
som_model <- som(final, 
                 somgrid(3,3,topo="hexagonal"), 
                 rlen=1000, 
                 alpha=c(0.05,0.01), 
                 mode="batch", 
                 maxNA.fraction=.5,
                 dist.fcts = "euclidean"
)

plotHeatMap (som_model,DATI, variable=47)

plot(som_model, type="changes")

#U_MATRIX 2016
plot(som_model, type="dist.neighbours", main = "", shape = "straight", palette.name=coolBlueHotRed)

#cds16 <- as.data.frame(som_model16$codes)
#fviz_nbclust(cds16, kmeans, method = "wss") #7 cluster


par(mfrow=c(1,2)) 
dev.off()
#rownames(dati13_16_som_final)<-region[,2] 

##############
#plot names of regions
plot(som_model, type="mapping", main = "",shape="straight"
     , labels=rownames(DATI), cex=0.45)
plot(som_model, type="dist.neighbours", main = "", shape = "straight", palette.name=coolBlueHotRed)


plot(som_model, type="counts", main="", shape = "straight", palette.name = coolBlueHotRed)     
plot(som_model, type="codes", main = "Weigh Vector 2013-16", shape = "straight", palette.name=coolBlueHotRed)
dev.off()  
#mypal<- wes_palette(9, name = "Zissou1", type = "continuous")

###################################
#plot 12 different colours
mypal<- (c("#A6CEE3", "#1F78B4", "#B2DF8A" ,"#33A02C" ,"#FB9A99" ,"#E31A1C" ,"#FDBF6F" ,"#FF7F00" ,"#CAB2D6"))

plot(som_model, type="mapping", 
     #labels=rownames(SOM13_16_final), 
     cex=0.45,
     #palette.name= mypal,
     bgcol = mypal,
     shape="straight",
     #labels=rownames (SOM13_16_final)
     #border ="darkgray"
     main=""
)
###########################################################
plotHeatMap <- function(som_model, data, variable=0){    
  # Plot a heatmap for any variable from the data set "data".
  # If variable is 0, an interactive window will be provided to choose the variable.
  # If not, the variable in "variable" will be plotted.
  
  require(dummies)
  require(kohonen)
  #source('coolBlueHotRed.R')
  
  interactive <- TRUE
  
  while (interactive == TRUE){
    
    if (variable == 0){
      #show interactive window.
      color_by_var <- select.list(names(data), multiple=FALSE,
                                  graphics=TRUE, 
                                  title="Choose variable to color map by.")
      # check for user finished.
      if (color_by_var == ""){ # if user presses Cancel - we quit function        
        return(TRUE)
      }
      interactive <- TRUE
      color_variable <- data.frame(data[, color_by_var])
      
    } else {
      color_variable <- data.frame(data[, variable])
      color_by_var <- names(data)[variable]
      interactive <- FALSE
    }
    
    #if the variable chosen is a string or factor - 
    #Get the levels and ask the user to choose which one they'd like.
    
    if (class(color_variable[,1]) %in% c("character", "factor", "logical")){
      #want to spread this out into dummy factors - but colour by one of those.
      temp_data <- dummy.data.frame(color_variable, sep="_")
      chosen_factor <- select.list(names(temp_data), 
                                   multiple=FALSE,
                                   graphics=TRUE, 
                                   title="Choose level of variable for colouring")
      color_variable <- temp_data[, chosen_factor]
      rm(temp_data, chosen_factor)
      color_by <- color_variable
    } else {      
      #impute the missing values with the mean.
      color_variable[is.na(color_variable[,1]),1] <- mean(color_variable[,1], na.rm=TRUE)
      #color_by <- capVector(color_variable[,1])
      #color_by <- scale(color_by)  
      color_by <- color_variable[,1]
    }
    unit_colors <- aggregate(color_by, by=list(som_model$unit.classif), FUN=mean, simplify=TRUE)
    plot(som_model, type = "property", shape = "straight", property=unit_colors[,2], main=color_by_var, palette.name=coolBlueHotRed)    
  }
}

library (dummies)

for (i in 2:47) {
  pdf(paste("rplot_", i,"BIS.pdf", sep="")) 
  plotHeatMap(som_model,final, variable=i)
  dev.off()
  print(i)
}

plotHeatMap (som_model,DATI)



# Extract the codebooks from SOM
codes <- as.data.frame(som_model$codes)


library(Rfast)
Rfast::nth(codes[,1], 1, descending = T)

apply(codes, 1, function(x) sort(x, decreasing=TRUE)[1])

a<-vector()
b<-vector()
d<-vector()
e<-vector()


iterations = 9
variables = 4

output <- matrix(ncol=variables, nrow=iterations)

for (i in 1:iterations){
  output[i,1] <- colnames(sort(codes[i,], decreasing = TRUE)[1])
  output[i,2]  <- colnames(sort(codes[i,], decreasing = TRUE)[2])
  output[i,3] <- colnames(sort(codes[i,], decreasing = FALSE)[1])
  output[i,4]  <- colnames(sort(codes[i,], decreasing = FALSE)[2])
  
}

colnames(sort(codes[2,], decreasing = TRUE)[2])

som_model$unit.classif

countrySOM<-cbind(as.data.frame(DATI$Country, stringsAsFactors=FALSE),som_model$unit.classif)



output<- as.data.frame(som_model$unit.classif, row.names = rownames(DATI))
write.csv(output, "country_cluster.csv")
write.csv(codes, "codes.csv")


qplot( x=rawnames , y=value , data=data , geom=c("boxplot","jitter") , fill=names)


cds07 <- as.data.frame(som_model07$codes, row.names = c("1","2","3","4","5","6","7","8","9","10","11","12"))


set.seed(123)

# function to compute total within-cluster sum of square 
wss <- function(k) {
  kmeans(df, k, nstart = 50)$tot.withinss
}

set.seed(123)

fviz_nbclust(cds16, kmeans, method = "wss")
fviz_nbclust(cds07, kmeans, method = "wss")

km16 <- kmeans(cds16, centers = 12, nstart = 50)
str(km16)
fviz_cluster(km16, data = cds16)
print (km16)

km07 <- kmeans(cds07, centers = 6, nstart = 50)
str(km07)
fviz_cluster(km07, data = cds07)
print (km07)

# Plot the SOM codes map with 3 clusters as background
#myPal1=colorRampPalette(c("black","orange","red","green"))
#MyPal5 <- c("yellow", "blue", "red", "violet",'yellowgreen', 'turquoise3', "azure")
mypal<- wes_palette(12, name = "Zissou1", type = "continuous")


dev.off()  
par(mar = c(0,5,0,2))
par(mfrow=c(1,2))

#A Kmeans
plot(som_model16, 
     type="mapping", 
     labels=rownames(SOM13_16_final),
     cex=0.45,
     #palette.name= coolBlueHotRed,
     bgcol = mypal[km16$cluster], 
     main = "",
     shape="straight",
     border ="darkgray"
)
add.cluster.boundaries(som_model16, km16$cluster)

#B umatrix
plot(som_model16, type="dist.neighbours", main = "", shape = "straight", palette.name=coolBlueHotRed)
add.cluster.boundaries(som_model16, km16$cluster)

#pretty_palette <- c("#1f77b4", '#ff7f0e', '#2ca02c', '#d62728', '#9467bd', '#8c564b', '#e377c2')

#A kmeans
plot(som_model07, 
     type="mapping", 
     labels=rownames(SOM02_07_final),
     cex=0.45,
     #palette.name= myPal1,
     bgcol = pal[km07$cluster], 
     main = "",
     shape="straight",
     border ="gray"
)
add.cluster.boundaries(som_model07, km07$cluster)

#B umatrix
plot(som_model07, type="dist.neighbours", main = "", shape = "straight", palette.name=coolBlueHotRed)
add.cluster.boundaries(som_model07, km07$cluster)

#####assegna cluster kmeans a cluster som
#A<-km16$cluster[som_model16$unit.classif] 

#prepara files con clusters
ass16= cbind (som_model16$unit.classif,cds16 [som_model16$unit.classif,])
rownames(ass16)<-rownames(SOM13_16_final)
names(ass16)[1]<-paste("SOM")

ass07= cbind (som_model07$unit.classif, cds07 [som_model07$unit.classif,])
rownames(ass07)<-rownames(SOM02_07_final)
names(ass07) [1] <- paste("SOM")

#salva files con clusters
write.csv(ass16, file = "cluster16.csv")
write.csv(ass07, file = "cluster07.csv")
dev.off()  


##############################################################
#legend("right",
#      x=7,
#     y=4,
#    cex=1.5,
#   title="Cluster",
#  legend = c(1:nCls),
# fill= MyPal3[c(1:nCls)]
#)


#H cluster boundaries
som.hc <- cutree(hclust(object.distances(som_model15, "codes")), 4)
add.cluster.boundaries(som_model15, som.hc)

#???
coolBlueHotRed <- function(n, alpha = 1) {rainbow(n, end=4/6, alpha=alpha)[n:1]}

#par(mfrow=c(1,2))
#CPLANES
#for (j in dim(dati13_15_som_final)[2]){
plot(som_model07, type = "property",  property = getCodes(som_model16)[,27], shape="straight",
     main=colnames(getCodes(som_model07))[27], 
     palette.name=coolBlueHotRed)

boxplot(cds07$D35)
#}
dev.off()  



#training plot
plot(som_model15, type="changes", shape = "straight", main= "Training 2015")



#####
plot(som_model16, type = "property",  property = , shape="straight",
     main=colnames(getCodes(som_model07))[27], 
     palette.name=coolBlueHotRed)

#########################FINE##########################################


# per 2008
som_model08_1 <- som(dati08_som_final, # cambia per 2015
                     grid=som_grid1, 
                     rlen=1000, 
                     alpha=c(0.05,0.01), 
                     keep.data = TRUE ,
                     maxNA.fraction=.5,
                     mode="batch")
som_model08_2 <- som(dati08_som_final, # cambia per 2015
                     grid=som_grid2, 
                     rlen=1000, 
                     alpha=c(0.05,0.01), 
                     keep.data = TRUE )
som_model08_3 <- som(dati08_som_final, # cambia per 2015
                     grid=som_grid3, 
                     rlen=1000, 
                     alpha=c(0.05,0.01), 
                     keep.data = TRUE )

# per 2015
som_model15_1 <- som(dati15_som_final, # cambia per 2015
                     somgrid(5,7,topo="hexagonal"), 
                     rlen=1000, 
                     alpha=c(0.05,0.01), 
                     keep.data = TRUE ,
                     maxNA.fraction=.5,
                     mode="batch",
)
som_model15_2 <- som(dati15_som_final, # cambia per 2015
                     grid=som_grid2, 
                     rlen=1000, 
                     alpha=c(0.05,0.01), 
                     keep.data = TRUE )
som_model15_3 <- som(dati15_som_final, # cambia per 2015
                     grid=som_grid3, 
                     rlen=1000, 
                     alpha=c(0.05,0.01), 
                     keep.data = TRUE )


################### trovo settori interessanti
FindOutliers <- function(data) {
  lowerq = quantile(data)[2]
  upperq = quantile(data)[4]
  iqr = upperq - lowerq #Or use IQR(data)
  # we identify extreme outliers
  extreme.threshold.upper = (iqr * 1.5) + upperq
  extreme.threshold.lower = lowerq - (iqr * 1.5)
  result <- which(data > extreme.threshold.upper | data < extreme.threshold.lower)
}

library (plyr)
out07 <- lapply(cds07, FindOutliers)
lengths(out07)
out07 <- ldply (out07, data.frame)
colnames(out07) <- c("ind","SOM")
write.csv(out07, file = "out_set07.csv")


out15 <- lapply(cds15, FindOutliers)
lengths(out15)
out15 <- ldply (out15, data.frame)
colnames(out15) <- c("ind","SOM")
write.csv(out15, file = "out_set15.csv")


plot(som_model07, type = "property",  property = getCodes(som_model07)[,24], shape="straight",
     main=colnames(getCodes(som_model07))[24],
     palette.name=coolBlueHotRed)

par(mfrow=c(1,1))
boxplot(cds15)
boxplot(cds07)

# use the function to identify outliers
#temp07 <- FindOutliers(cds07$C10)
#train <- train [-temp,]
#boxplot(train$T1, main="T1", boxwex=0.1)


plot(as.numeric(cds15[1,]))
is.numeric(cds)

### MAP MAP MAP MAP MAP MAP MAP MAP MAP MAP MAP MAP
#####################################################

library(dplyr)
library(eurostat)
library(sf)
library(tmap)
library (classInt)
library(backports)

sp_data <- eurostat::get_eurostat("tgs00026",
                                  time_format = "raw",
                                  stringsAsFactors = FALSE) %>% 
  # subset to have only a single row per geo
  dplyr::filter(time == 2010, nchar(geo) == 4) %>% 
  # categorise
  dplyr::mutate(income = cut_to_classes(values, n = 5))

geodata <- get_eurostat_geospatial(output_class = "sf",
                                   resolution = "60",
                                   nuts_level = 2,
                                   year = 2013)


geodatauk <- geodata[which(geodata$CNTR_CODE=="UK"), ]
geodatait <- geodata[which(geodata$CNTR_CODE=="IT"), ]
geodatauk <- geodata[which(geodata$CNTR_CODE=="FR"), ]
geodatait <- geodata[which(geodata$CNTR_CODE=="ES"), ]
geodatait <- geodata[which(geodata$CNTR_CODE=="DE"), ]

#geodatauk$geo<-gsub("UKI3", "UKI1", geodatauk$geo)
#geodatauk$geo<-gsub("UKI4", "UKI1", geodatauk$geo)
#geodatauk$geo<-gsub("UKI5", "UKI2", geodatauk$geo)
#geodatauk$geo<-gsub("UKI6", "UKI2", geodatauk$geo)
#geodatauk$geo<-gsub("UKI7", "UKI2", geodatauk$geo)

#map_data <- inner_join(geodatauk, robot_wideuk)
#map1 <- tmap::tm_shape(geodatauk) +
# tmap::tm_fill("lightgrey") +
#tmap::tm_shape(map_data) +
# tmap::tm_grid() +
# tmap::tm_polygons("stock", title = "Robot stock per 100000 in.",  
#    palette = "Oranges")
#print(map1) 

######################
geodata_5 <- geodata[which(geodata$CNTR_CODE=="UK" | geodata$CNTR_CODE=="ES"| geodata$CNTR_CODE=="IT"| geodata$CNTR_CODE=="FR"| geodata$CNTR_CODE=="DE"), ]

geodata_5$geo<-gsub("UKI3", "UKI1", geodata_5$geo)
geodata_5$geo<-gsub("UKI4", "UKI1", geodata_5$geo)
geodata_5$geo<-gsub("UKI5", "UKI2", geodata_5$geo)
geodata_5$geo<-gsub("UKI6", "UKI2", geodata_5$geo)
geodata_5$geo<-gsub("UKI7", "UKI2", geodata_5$geo)

geodata_5<- geodata_5[ !(geodata_5$id == "FRA1"),]
geodata_5<- geodata_5[ !(geodata_5$id == "FRA2"),]
geodata_5<- geodata_5[ !(geodata_5$id == "FRA3"),]
geodata_5<- geodata_5[ !(geodata_5$id == "FRA4"),]
geodata_5<- geodata_5[ !(geodata_5$id == "FRA5"),]

##########################################

cluster16<-read.csv("cluster16.csv", header=TRUE, sep=",", dec=",")
#cluster16$Kmeans<-as.factor(cluster16$Kmeans)
cluster16$SOM<-as.factor(cluster16$SOM)
colnames(cluster16)[1]<-"geo"
cluster16[order(cluster16$SOM),]

cluster07<-read.csv("cluster07.csv", header=TRUE, sep=",", dec=",")
cluster07$SOM<-as.factor(cluster07$SOM)
colnames(cluster07)[1]<-"geo"
cluster07[order(cluster07$SOM),]


#colors packages
library (RColorBrewer)
library(randomcoloR)
n <- 12
palette <- distinctColorPalette(12)


library(wesanderson)
palette1<-wes_palette("GrandBudapest2", 12)

palette2<- topo.colors(12)
########

map_data1 <- inner_join(geodata_5,cluster16 )
map_data2 <- inner_join(geodata_5,cluster07 )

map1 <- tmap::tm_shape(geodata_5) +
  tmap::tm_fill("lightgrey") +
  tmap::tm_shape(map_data1) +
  tmap::tm_grid() +
  tmap::tm_polygons("SOM",palette="Paired") +
  
  tm_layout(#"Industrial Structures 2013-2016",
    #legend.title.size = 1,
    legend.text.size = 0.5,
    legend.position = c("left","bottom"),
    #legend.bg.color = "white",
    legend.bg.alpha = 1)

print(map1)
#################oppure
map1 <- 
  tmap_mode ("view")
tm_shape(map_data1) +
  tmap::tm_polygons("SOM",palette="Paired") 


print(mapUV)

map2 <- 
  tmap_mode ("view")
tm_shape(map_data2) +
  tmap::tm_polygons("SOM",palette="Paired") 

############################################
UV13_16<-apply(onedigit13_16, 1,UV)
UV13_16<-as.data.frame(UV13_16)
write.csv(UV13_16, file = "UV13_16.csv")
UV13_16<-read.csv("UV13_16.csv", header=TRUE, sep=",", dec=",")
colnames(UV13_16)[1]<-"geo"
UV13_16[, 1] <- as.factor(UV13_16[, 1])
UV13_16$UV13_16<-as.numeric(as.character(UV13_16$UV13_16))

RV13_16<-rowSums(Hg13_16*onedigit13_16, na.rm = T)
RV13_16<-as.data.frame(RV13_16)
write.csv(RV13_16, file = "RV13_16.csv")
RV13_16<-read.csv("RV13_16.csv", header=TRUE, sep=",", dec=",")
colnames(RV13_16)[1]<-"geo"
RV13_16[, 1] <- as.factor(RV13_16[, 1])
RV13_16$RV13_16<- as.numeric(as.character(RV13_16$RV13_16))

map_dataUV <- inner_join(geodata_5,UV13_16 )
map_dataRV <- inner_join(geodata_5,RV13_16 )

mapUV <- 
  tmap_mode ("view")
tm_shape(map_dataUV) +
  tmap::tm_polygons(col="UV13_16",  style = "cont") 


print(mapUV)



mapRV <- 
  tmap_mode ("view")
tm_shape(map_dataRV) +
  tmap::tm_polygons(col="RV13_16",  style = "cont") 


print(mapRV)
##########################################
GR12_16<-as.data.frame(GR12_16[c("region", "gr")])
colnames(GR12_16)[1]<-"geo"

map_dataGR<- inner_join(geodata_5,GR12_16 )
mapGR <- 
  tmap_mode ("view")
tm_shape(map_dataGR) +
  tmap::tm_polygons(col="gr",  style = "cont") 


print(mapGR )

################################################

cluster07<-read.csv("cluster07.csv", header=TRUE, sep=",", dec=",")
#cluster16$Kmeans<-as.factor(cluster16$Kmeans)
cluster07$SOM<-as.factor(cluster07$SOM)
colnames(cluster07)[1]<-"geo"
cluster07[order(cluster07$SOM),]

map_data2 <- inner_join(geodata_5, cluster07)

map3 <- tmap::tm_shape(geodata_5) +
  tmap::tm_fill("lightgrey") +
  tmap::tm_shape(map_data2) +
  tmap::tm_grid() +
  tmap::tm_polygons("SOM", #title = "Industrial Structures 2002-2007",  
                    palette = "Paired") + 
  #can try also "spectral" "grandbudapest2"
  tm_layout(legend.title.size = 1,
            legend.text.size = 0.6,
            legend.position = c("left","bottom"),
            #legend.bg.color = "white",
            legend.bg.alpha = 1)
print(map3)

map4 <- tmap::tm_shape(geodata_5) +
  tmap::tm_fill("lightgrey") +
  tmap::tm_shape(map_data2) +
  tmap::tm_grid() +
  tmap::tm_polygons("DM34",  palette = "Oranges")
#+
tm_layout("Regional Specialization 2002-2008",
          legend.title.size = 5,
          legend.text.size = 0.6,
          legend.position = c("left","bottom"),
          
          #legend.bg.color = "white",
          #legend.bg.alpha = 1)
          
          legend.digits = 2)

print(map4)

tmap_mode("view")
map1
palette

tmap_mode("view")
map1

####################################################ALTRI DATI
library(dplyr)
sum16<-tapply(dati16$employ, dati16$region, na.rm=TRUE, FUN=sum)
sum14<-tapply(dati14$employ, dati14$region, na.rm=TRUE, FUN=sum)
sum12<-tapply(dati12$employ, dati12$region, na.rm=TRUE, FUN=sum)
sum10<-tapply(dati10$employ, dati10$region, na.rm=TRUE, FUN=sum)
sum08<-tapply(dati08$employ, dati08$region, na.rm=TRUE, FUN=sum)
sum06<-tapply(dati06$employ, dati06$region, na.rm=TRUE, FUN=sum)
sum04<-tapply(dati04$employ, dati04$region, na.rm=TRUE, FUN=sum)
sum02<-tapply(dati02$employ, dati02$region, na.rm=TRUE, FUN=sum)


setwd("C:/Users/nucciom/Google Drive/DATA/POP density")
library("readxl")
library(plyr)

POP_DEN<-read_excel("demo_r_d3dens.xls")
colnames(POP_DEN)[1] <- "region"

GDP<-read_excel ("GDP_MLL.xls")
colnames(GDP)[1] <- "region"

EU<-read_excel ("EU_HAB.xlsx")
colnames(EU)[1] <- "region"

ppp<-read_excel ("ppp_hab.xlsx")
colnames(ppp)[1] <- "region"

POP_DEN<-match_df(POP_DEN, DATI,on = "region")
GDP<-match_df(GDP, DATI,on = "region")
EU<-match_df(EU, DATI,on = "region")
ppp<-match_df(ppp, DATI,on = "region")

cond1 <- lapply(1, cds16, function(col) sum(x) < 0.12)

boxplot(as.data.frame(som_model$codes))
library(corrplot)
M<-cor(cds16)
corrplot(M, type = "upper", order="hclust")
library(igraph)
network <- from_incidence_matrix(M)
plot(network)


A <-as.data.frame(som_model$codes)


####
B <- DATI
a <- c(1990,2005,2015)
b <-rep(a,41)
B$year<-b

d<-c(2,3,4)
e <-rep(d,41)

B$numero<-e
B$country<-row.names(B)
B$caratteri<-nchar(B$country)-B$numero

B$country<-row.names(B)
B$contry1<-substr(B$country, 1, B$caratteri)



B$Cluster <- som_model$unit.classif
B$X <- som_model$grid$pts[som_model$unit.classif,"x"]
B$Y <- som_model$grid$pts[som_model$unit.classif,"y"]


plot(som_model, type="counts")
points(jitter(B$X[1:10]), jitter(B$Y[1:10]), col=B$Country)

plot(jitter(B$X[1:10]), jitter(B$Y[1:10]))

B$X1<-jitter(B$X)
B$Y1<-jitter(B$Y)
i <-0
E <- B[which(B$year%in% c("1990","2015")),]
for(i in c(0,18, 36, 54, 72, 88, 105)){
  
  
  C<-B[(i+1):(i+18),]
  
  p <- ggplot(C, aes(X1, Y1)) + geom_point(aes(colour = factor(contry1)))
  p <- p + geom_line(aes(X1,Y1, group=factor(contry1), colour = factor(contry1)))
  p <- p + theme(legend.title = element_blank())
  p <- p + labs(x = "Dimension 1") + labs(y = "Dimension 2")
  p <- p + geom_hline(yintercept=1.35, linetype="dashed", color = "black")
  p <- p + geom_hline(yintercept=2.15, linetype="dashed", color = "black")
  p <- p + geom_vline(xintercept=2,15, linetype="dashed", color = "black")
  p <- p + geom_vline(xintercept=2.85, linetype="dashed", color = "black")
  p
  
  ggsave(paste("plot",i,".png",sep=""), p)
}

D<-B[which(B$year==2015),]
p <- ggplot(D, aes(X1, Y1)) + geom_point(aes(colour = "black"))
p <- p + theme(legend.title = element_blank())
p <- p + labs(x = "Dimension 1") + labs(y = "Dimension 2")
p <- p + theme(legend.position = "none")
p <- p + geom_hline(yintercept=1.35, linetype="dashed", color = "black")
p <- p + geom_hline(yintercept=2.15, linetype="dashed", color = "black")
p <- p + geom_vline(xintercept=1.80, linetype="dashed", color = "black")
p <- p + geom_vline(xintercept=2.85, linetype="dashed", color = "black")

ggsave(paste("2015.png",sep=""), p)

D<-B[which(B$year==1990),]
p <- ggplot(D, aes(X1, Y1)) + geom_point(aes(colour = "black"))
p <- p + theme(legend.title = element_blank())
p <- p + labs(x = "Dimension 1") + labs(y = "Dimension 2")
p <- p + theme(legend.position = "none")
p <- p + geom_hline(yintercept=1.35, linetype="dashed", color = "black")
p <- p + geom_hline(yintercept=2.15, linetype="dashed", color = "black")
p <- p + geom_vline(xintercept=1.80, linetype="dashed", color = "black")
p <- p + geom_vline(xintercept=2.85, linetype="dashed", color = "black")

ggsave(paste("1990.png",sep=""), p)

################# FIINALE

E <- B[which(B$year%in% c("1990", "2015")),]
F <- B[which(B$year%in% c("1990")),]
G <- B[which(B$year%in% c("2015")),]
for(i in c(0,14, 28, 32)){
  
  
  C<-E[(i+1):(i+14),]
  
  p <- ggplot(C, aes(X1, Y1)) + geom_point(aes(colour = factor(contry1),  shape = factor(year)))
  p <- p  + geom_path(aes(x = X1, y = Y1, group = factor(contry1), colour = factor(contry1)), 
                      arrow = arrow(length = unit(0.55, "cm")))
  
  p <- p + theme(legend.title = element_blank())
  p <- p + labs(x = "Dimension 1") + labs(y = "Dimension 2")
  p <- p + geom_hline(yintercept=1.35, linetype="dashed", color = "black")
  p <- p + geom_hline(yintercept=2.15, linetype="dashed", color = "black")
  p <- p + geom_vline(xintercept=1.80, linetype="dashed", color = "black")
  p <- p + geom_vline(xintercept=2.85, linetype="dashed", color = "black")
  p
  
  ggsave(paste("plot",i,".png",sep=""), p)
}
