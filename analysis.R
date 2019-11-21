#LIBRARY

library(dplyr)
library(kohonen)
library(wesanderson)
library(RColorBrewer)
library(data.table)
library(factoextra)
source("functionheatmap.R") #I took it from https://github.com/shanealynn/Kohonen-Self-organising-maps-in-R/blob/master/plotHeatMap.R




# IMPORT DATA
DATI <- read.csv("FINAL3.csv", sep = ";", dec = ",")
DATI <- read.csv("otherindicator.csv", sep = ";", dec = ",")

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

#U_MATRIX 
plot(som_model, type="dist.neighbours", main = "", shape = "straight", palette.name=coolBlueHotRed)

#cds16 <- as.data.frame(som_model16$codes)
#fviz_nbclust(cds16, kmeans, method = "wss") #7 cluster


par(mfrow=c(1,2)) 
dev.off()
#rownames(dati13_16_som_final)<-region[,2] 

##############
#plot names of countries
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
     #labels=rownames( ), 
     cex=0.45,
     #palette.name= mypal,
     bgcol = mypal,
     shape="straight",
     #labels=rownames ( )
     #border ="darkgray"
     main=""
)
###########################################################

library (dummies)

for (i in 2:45) {
  png(paste("rplot_", i,"BIS.png", sep="")) 
  plotHeatMap(som_model,final, variable=i)
  dev.off()
  print(i)
}

#plotHeatMap(som_model,DATI)



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

#colnames(sort(codes[2,], decreasing = TRUE)[2])

#som_model$unit.classif

countrySOM<-cbind(as.data.frame(DATI$Country, stringsAsFactors=FALSE),som_model$unit.classif)



write.csv(countryclusters, "country_cluster.csv")
write.csv(codes, "codes.csv")




#####

A <-as.data.frame(som_model$codes)


#### EVOLUTION 1
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


#picture three years I go for start and end
# for(i in c(0,18, 36, 54, 72, 88, 105)){
#   
#   
#   C<-B[(i+1):(i+18),]
#   
#   p <- ggplot(C, aes(X1, Y1)) + geom_point(aes(colour = factor(contry1)))
#   p <- p + geom_line(aes(X1,Y1, group=factor(contry1), colour = factor(contry1)))
#   p <- p + theme(legend.title = element_blank())
#   p <- p + labs(x = "Dimension 1") + labs(y = "Dimension 2")
#   p <- p + geom_hline(yintercept=1.35, linetype="dashed", color = "black")
#   p <- p + geom_hline(yintercept=2.15, linetype="dashed", color = "black")
#   p <- p + geom_vline(xintercept=2,15, linetype="dashed", color = "black")
#   p <- p + geom_vline(xintercept=2.85, linetype="dashed", color = "black")
#   p
#   
#   ggsave(paste("plot",i,".png",sep=""), p)
# }

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

################# EVOLUTION 2

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
