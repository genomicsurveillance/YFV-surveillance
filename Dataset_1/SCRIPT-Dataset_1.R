library(ggplot2)
library(ape)
library(repr)
library("readxl")
library('gridExtra')
library(dplyr)
library(hrbrthemes)
library(ggpubr)
library(cowplot)
library(ggthemes)
library(viridis)
library(ggrepel)
library("ggsci")
library(ggalt)
library("Hmisc")
library("scales")
if (!(require(ggtree))){
  if (!requireNamespace("BiocManager", quietly = TRUE))
    install.packages("BiocManager")
  BiocManager::install("ggtree")
}
library(ggtree)
library(tidyverse)
library(tidytree)
library(ape)
library(treeio)

### Fig2 ###
setwd("")                                                                                                                                     
tree<-read.tree('YFV_North+All_Edit_Final.nwk')
metadata_df <- read_excel('New-Final.xlsx')
metadata_df$clade <- as.character(metadata_df$clade)
metadata_df$date<-as.Date(cut(metadata_df$date,
                              breaks = "week",
                              start.on.monday = FALSE))
metadata_df$date2<-as.Date(cut(metadata_df$date,
                               breaks = "2 week",
                               start.on.monday = FALSE))
metadata_df$date3<-as.Date(cut(metadata_df$date,
                               breaks = "1 month",
                               start.on.monday = FALSE))
p <- ggtree(tree, mrsd= "2022-03-12")%<+% metadata_df +
  theme(legend.position = "left")  + 
  theme_tree2() +
  geom_tippoint(aes(fill=region), shape = 21, size= 2.5) +
  scale_x_continuous(breaks = c(2002, 2004, 2006, 2008, 2010, 2012, 2014, 2016, 2018, 2020, 2022))+
  ggplot2::ylim(0, 450)
p
metaDATA_YFV<- data.frame(C = metadata_df$clade, H = metadata_df$host, L =metadata_df$location)
rownames(metaDATA_YFV) <- metadata_df$name
metaDATA_YFVheat <- gheatmap(p, metaDATA_YFV, offset = 0.01, width=0.15, font.size=3, colnames_position= "top", colnames_angle = 0, colnames_offset_y = 0, hjust = 0) + 
  scale_fill_manual(values= c( "#f6c99c", "#95A5DF", "#EE6A50", "#6F62D7", "#8CD9B3", "#458B1F", "#E9E2CD", "#8B3A8F",
                               "#920A2B", "#E32E4F", "#DF5FC2", "#927E92", "#C6E2FF", "#f6c99c", "#95A5DF", "#EE6A50", 
                               "#6F62D7", "#8CD9B3", "#458B1F", "#E9E2CD", "#8B3A8F" ,"#920A2B", "#E32E4F", "#DF5FC2", 
                               "#927E92", "#C6E2FF"))
metaDATA_YFVheat



