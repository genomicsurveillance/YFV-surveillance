
library(devtools)
library(diagram)
require(seraphim)
library(rgdal)
library(ribge)
library(geobr)
library(sf)
library(dplyr)
library(scales)
library(openxlsx)


setwd("")

source("mccExtractions.r")

my_spdf <- readOGR(
  dsn= paste0("TM_WORLD_BORDERS_SIMPL-0.3") ,
  layer="TM_WORLD_BORDERS_SIMPL-0.3",
  verbose=FALSE
)

BRAZILBD <- my_spdf[my_spdf@data$NAME=="Brazil" , ]
borders=BRAZILBD
template_raster=BRAZILBD
pop <- populacao_municipios(2020)
pop$cod_municipio <- as.numeric(pop$cod_municipio)
br <- read_country(year=2020)
muni <- read_municipality(year=2020)
muni_final <- left_join(muni, pop, by=c("code_state"="codigo_uf", "code_muni"="cod_municipio", "abbrev_state"="uf"))
map.colors <- c("#eaf1e7", "#94C58C", "#429B46", "#094F29")

fcover <- read.csv("list_municipalities_FCOVER_map.csv", header = TRUE)

##### YFV Clade Southeast and Notheast

treefile <- "YFV2RELSKYLINE100ML_Prior_Comb.trees"

localTreesDirectory <-  "data/yfv2"
allTrees <- scan(file=treefile, what="", sep="\n", quiet=T)
burnIn <- 0
randomSampling <- FALSE
nberOfTreesToSample <- 100
mostRecentSamplingDatum <- 2021.4136986301369
coordinateAttributeName <- "Location"

treeExtractions(localTreesDirectory, allTrees, burnIn, randomSampling, nberOfTreesToSample, mostRecentSamplingDatum, coordinateAttributeName)
treefile <- "YFV2RELSKYLINE100ML_Prior_Comb.tree"
mcc_tre <- readAnnotatedNexus(treefile)
mcc_tab <- mccTreeExtraction(mcc_tre, mostRecentSamplingDatum)
write.csv(mcc_tab, "YFV2_cluster_MCC.csv", row.names=F, quote=F)
nberOfExtractionFiles <- nberOfTreesToSample
prob <- 0.95
precision <- 0.025
startDatum <- min(mcc_tab[,"startYear"])
polygons <- suppressWarnings(spreadGraphic2(localTreesDirectory, nberOfExtractionFiles, prob, startDatum, precision))
minYear <- min(mcc_tab[,"startYear"])
maxYear <- max(mcc_tab[,"endYear"])
mcc_tab_this <- mcc_tab %>% data.frame()
mcc_tab_this <- mcc_tab_this %>% dplyr::filter(startYear>=minYear) %>% dplyr::filter(endYear<=maxYear)
endYears_indices <- (((mcc_tab[,"endYear"]-minYear)/(maxYear-minYear))*100)+1
n_number_colours_needed <- max(round(endYears_indices))
n_repeats_discrete <- 10
c1 <- rev(brewer.pal(4,"PuRd"))
c2 <- (brewer.pal(9,"Blues"))
colours <- rev(rep(c(c1,c2), each=n_repeats_discrete))
colour_scale <- colorRampPalette(colours)(n_number_colours_needed)
endYears_colours <- colour_scale[round(endYears_indices)]
polygons_colours <- rep(NA, length(polygons))
for (i in 1:length(polygons)){
  date <- as.numeric(names(polygons[[i]]))
  polygon_index <- round((((date-minYear)/(maxYear-minYear))*100)+1)
  polygons_colours[i] <- paste0(colour_scale[polygon_index],"55")
}
muni_map <- left_join(muni_final, fcover, by=c("cod_munic6"="CODE"))
muni_map$sd[which(is.na(muni_map$sd))] <- 0
muni_map$mean[which(is.na(muni_map$mean))] <- 0
muni_map$median[which(is.na(muni_map$median))] <- 0
muni_map$colorMap <- NA
num <- seq(min(muni_map$mean), max(muni_map$mean), length = length(map.colors))
legendnum <- c()
for (kkk in 1:length(num)){
  if (kkk == length(num)){
    legendnum[kkk] <- paste0("> ", round(num[kkk], digits = 2))
  }else{
    legendnum[kkk] <- paste0(round(num[kkk], digits = 2), " - ", round(num[kkk+1], digits = 2))
  }
}
for(i in 1:length(num)){
  if(i == length(num)){
    muni_map$colorMap[which(muni_map$mean >= num[i])] <- map.colors[i]
  }else{
    muni_map$colorMap[which(muni_map$mean > num[i] & muni_map$mean <= num[i+1])] <- map.colors[i]
  }
}
southeast_northeast_mun <- subset(muni_map, (code_region == 2 | code_region == 3))
southeast_northeast_br <- subset(br, (code_region == 2 | code_region == 3))
southeast_northeast_br <- subset(southeast_northeast_br, code_state > 28)
southeast_northeast_mun <- subset(southeast_northeast_mun, code_state > 28)
pdf(paste0("YFV2_fcover_map_southeast_notheast_cluster.pdf"),width=6, height=4,bg="white")
ptsize <- 0.4
pitjit <- 0.08
par(mar=c(0,0,0,0), oma=c(1.2,3.5,1,0), mgp=c(0,0.4,0), lwd=0.2, bty="o")
plot(southeast_northeast_br$geom, col="white", box=F, axes=F, colNA="grey90", legend=F)
for(i in 1:nrow(southeast_northeast_mun)){
  plot(southeast_northeast_mun$geom[i], axes=F, col=southeast_northeast_mun$colorMap[i], add=T, border=NA)
}
plot(southeast_northeast_br$geom, add=T, lwd=0.1, border="gray10")
rastVeg <- raster(matrix(nrow=1, ncol=2))
rastVeg[1] <- 0
rastVeg[2] <- 100
for (i in length(polygons):1){
  plot(polygons[[i]], axes=F, col=polygons_colours[i], add=T, border=NA)
}
for (i in 1:dim(mcc_tab)[1]){
  curvedarrow(cbind(mcc_tab[i,"startLon"],mcc_tab[i,"startLat"]), cbind(mcc_tab[i,"endLon"],mcc_tab[i,"endLat"]), arr.length=0,
              arr.width=0, lwd=2.5*1.1, lty=1, lcol="grey22", arr.col=NA, arr.pos=FALSE, curve=0.3, dr=NA, endhead=F)
  curvedarrow(cbind(mcc_tab[i,"startLon"],mcc_tab[i,"startLat"]), cbind(mcc_tab[i,"endLon"],mcc_tab[i,"endLat"]), arr.length=0,
              arr.width=0, lwd=2*1.1, lty=1, lcol=endYears_colours[i], arr.col=NA, arr.pos=FALSE, curve=0.3, dr=NA, endhead=F)
}
for (i in dim(mcc_tab)[1]:1){
  xs <- mcc_tab[i,"startLon"]
  ys <- mcc_tab[i,"startLat"]
  xe <- jitter(mcc_tab[i,"endLon"],pitjit)
  ye <- jitter(mcc_tab[i,"endLat"],pitjit)
  if (i == 1){
    points(xs, ys, pch=16, col=colour_scale[1], cex=ptsize)
    points(xs, ys, pch=1, col="gray10", cex=ptsize)
  }
  points(xe, ye, pch=16, col=endYears_colours[i], cex=ptsize)
  points(xe, ye, pch=1, col="gray10", cex=ptsize)
}
xrange <- c(xmin(template_raster), xmax(template_raster))
yrange <- c(ymin(template_raster), ymax(template_raster))
rect(xrange[1], yrange[1], xrange[2], yrange[2], xpd=T, lwd=0.2)
axis(1, c(ceiling(xmin(template_raster)), floor(xmax(template_raster))), pos=ymin(template_raster), mgp=c(0,0.2,0), cex.axis=0.5, lwd=0, lwd.tick=0.2, padj=-0.8, tck=-0.01, col.axis="gray30")
axis(2, c(ceiling(ymin(template_raster)), floor(ymax(template_raster))), pos=xmin(template_raster), mgp=c(0,0.5,0), cex.axis=0.5, lwd=0, lwd.tick=0.2, padj=1, tck=-0.01, col.axis="gray30")
rast <- raster(matrix(nrow=1, ncol=2)); rast[1] = min(mcc_tab[,"startYear"]); rast[2] = max(mcc_tab[,"endYear"])
plot(rast, legend.only=T, add=T, col=colour_scale, legend.width=0.5, legend.shrink=0.3, smallplot=c(0.40,0.80,0.14,0.155),
     legend.args=list(text="", cex=0.7, line=0.3, col="gray30"), horizontal=T,
     axis.args=list(cex.axis=0.6, lwd=0, lwd.tick=0.2, tck=-0.5, col.axis="gray30", line=0, mgp=c(0,-0.02,0)))
legend("bottomleft", 
       legend = legendnum, 
       fill = map.colors, 
       bty = "n",
       cex = .6,
       border = "black")
a <- dev.off()

##### YFV Clade Southeast, Midwest and South
treefile <- "YFV3RELSKYLINE50ML-Prior_Comb.trees"
localTreesDirectory3 <- "data/yfv3"
allTrees <- scan(file=treefile, what="", sep="\n", quiet=T)
burnIn <- 0
randomSampling <- FALSE
nberOfTreesToSample <- 100
mostRecentSamplingDatum <- 2021.4136986301369
coordinateAttributeName <- "Location"
treeExtractions(localTreesDirectory3, allTrees, burnIn, randomSampling, nberOfTreesToSample, mostRecentSamplingDatum, coordinateAttributeName)
treefile <- "YFV3RELSKYLINE50ML-Prior_Comb.tree"
mcc_tre <- readAnnotatedNexus(treefile)
mcc_tab <- mccTreeExtraction(mcc_tre, mostRecentSamplingDatum)
write.csv(mcc_tab, "YFV3_cluster_MCC.csv", row.names=F, quote=F)
nberOfExtractionFiles <- nberOfTreesToSample
prob <- 0.95
precision <- 0.025
startDatum <- min(mcc_tab[,"startYear"])
polygons <- suppressWarnings(spreadGraphic2(localTreesDirectory3, nberOfExtractionFiles, prob, startDatum, precision))
minYear <- min(mcc_tab[,"startYear"])
maxYear <- max(mcc_tab[,"endYear"])
mcc_tab_this <- mcc_tab %>% data.frame()
mcc_tab_this <- mcc_tab_this %>% dplyr::filter(startYear>=minYear) %>% dplyr::filter(endYear<=maxYear)
endYears_indices <- (((mcc_tab[,"endYear"]-minYear)/(maxYear-minYear))*100)+1
n_number_colours_needed <- max(round(endYears_indices))
n_repeats_discrete <- 10
c1 <- rev(brewer.pal(4,"PuRd"))
c2 <- (brewer.pal(9,"Blues"))
colours <- rev(rep(c(c1,c2), each=n_repeats_discrete))
colour_scale <- colorRampPalette(colours)(n_number_colours_needed)
endYears_colours <- colour_scale[round(endYears_indices)]
polygons_colours <- rep(NA, length(polygons))
for (i in 1:length(polygons)){
  date <- as.numeric(names(polygons[[i]]))
  polygon_index <- round((((date-minYear)/(maxYear-minYear))*100)+1)
  polygons_colours[i] <- paste0(colour_scale[polygon_index],"55")
}
muni_map <- left_join(muni_final, fcover, by=c("cod_munic6"="CODE"))
muni_map$sd[which(is.na(muni_map$sd))] <- 0
muni_map$mean[which(is.na(muni_map$mean))] <- 0
muni_map$median[which(is.na(muni_map$median))] <- 0
muni_map$colorMap <- NA
num <- seq(min(muni_map$mean), max(muni_map$mean), length = length(map.colors))
legendnum <- c()
for (kkk in 1:length(num)){
  if (kkk == length(num)){
    legendnum[kkk] <- paste0("> ", round(num[kkk], digits = 2))
  }else{
    legendnum[kkk] <- paste0(round(num[kkk], digits = 2), " - ", round(num[kkk+1], digits = 2))
  }
}
for(i in 1:length(num)){
  if(i == length(num)){
    muni_map$colorMap[which(muni_map$mean >= num[i])] <- map.colors[i]
  }else{
    muni_map$colorMap[which(muni_map$mean > num[i] & muni_map$mean <= num[i+1])] <- map.colors[i]
  }
}
ssm_mun <- subset(muni_map, (code_region == 3 | code_region == 4 | code_region == 5))
ssm_br <- subset(br, (code_region == 3 | code_region == 4 | code_region == 5))
ssm_mun <- subset(ssm_mun, code_state < 50 | code_state >= 52)
ssm_br <- subset(ssm_br, code_state < 50 | code_state >= 52)
pdf(paste0("YFV3_fcover_map_southeast_midwest_south_cluster.pdf"),width=6, height=4,bg="white")
ptsize <- 0.4
pitjit <- 0.08
par(mar=c(0,0,0,0), oma=c(1.2,3.5,1,0), mgp=c(0,0.4,0), lwd=0.2, bty="o")
plot(ssm_br$geom, col="white", box=F, axes=F, colNA="grey90", legend=F)
for(i in 1:nrow(ssm_mun)){
  plot(ssm_mun$geom[i], axes=F, col=ssm_mun$colorMap[i], add=T, border=NA)
}
plot(ssm_br$geom, add=T, lwd=0.1, border="gray10")
rastVeg <- raster(matrix(nrow=1, ncol=2))
rastVeg[1] <- 0
rastVeg[2] <- 100
for (i in length(polygons):1){
  plot(polygons[[i]], axes=F, col=polygons_colours[i], add=T, border=NA)
}
for (i in 1:dim(mcc_tab)[1]){
  curvedarrow(cbind(mcc_tab[i,"startLon"],mcc_tab[i,"startLat"]), cbind(mcc_tab[i,"endLon"],mcc_tab[i,"endLat"]), arr.length=0,
              arr.width=0, lwd=2.5*1.1, lty=1, lcol="grey22", arr.col=NA, arr.pos=FALSE, curve=0.3, dr=NA, endhead=F)
  curvedarrow(cbind(mcc_tab[i,"startLon"],mcc_tab[i,"startLat"]), cbind(mcc_tab[i,"endLon"],mcc_tab[i,"endLat"]), arr.length=0,
              arr.width=0, lwd=2*1.1, lty=1, lcol=endYears_colours[i], arr.col=NA, arr.pos=FALSE, curve=0.3, dr=NA, endhead=F)
}
for (i in dim(mcc_tab)[1]:1){
  xs <- mcc_tab[i,"startLon"]
  ys <- mcc_tab[i,"startLat"]
  xe <- jitter(mcc_tab[i,"endLon"],pitjit)
  ye <- jitter(mcc_tab[i,"endLat"],pitjit)
  if (i == 1){
    points(xs, ys, pch=16, col=colour_scale[1], cex=ptsize)
    points(xs, ys, pch=1, col="gray10", cex=ptsize)
  }
  points(xe, ye, pch=16, col=endYears_colours[i], cex=ptsize)
  points(xe, ye, pch=1, col="gray10", cex=ptsize)
}
xrange <- c(xmin(template_raster), xmax(template_raster))
yrange <- c(ymin(template_raster), ymax(template_raster))
rect(xrange[1], yrange[1], xrange[2], yrange[2], xpd=T, lwd=0.2)
axis(1, c(ceiling(xmin(template_raster)), floor(xmax(template_raster))), pos=ymin(template_raster), mgp=c(0,0.2,0), cex.axis=0.5, lwd=0, lwd.tick=0.2, padj=-0.8, tck=-0.01, col.axis="gray30")
axis(2, c(ceiling(ymin(template_raster)), floor(ymax(template_raster))), pos=xmin(template_raster), mgp=c(0,0.5,0), cex.axis=0.5, lwd=0, lwd.tick=0.2, padj=1, tck=-0.01, col.axis="gray30")
rast <- raster(matrix(nrow=1, ncol=2)); rast[1] = min(mcc_tab[,"startYear"]); rast[2] = max(mcc_tab[,"endYear"])
plot(rast, legend.only=T, add=T, col=colour_scale, legend.width=0.5, legend.shrink=0.3, smallplot=c(0.40,0.80,0.14,0.155),
     legend.args=list(text="", cex=0.7, line=0.3, col="gray30"), horizontal=T,
     axis.args=list(cex.axis=0.6, lwd=0, lwd.tick=0.2, tck=-0.5, col.axis="gray30", line=0, mgp=c(0,-0.02,0)))
legend("bottomleft", 
       legend = legendnum, 
       fill = map.colors, 
       bty = "n",
       cex = .6,
       border = "black")
a <- dev.off()

