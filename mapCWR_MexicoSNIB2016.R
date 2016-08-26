## This script plots the SNIB2015 information of crop wildrelatives in Mexico
library(maptools)
library(scales)
library(dplyr)
library(png)
library(ggplot2)

### Get data

##SNIB 
df<-read.delim("generos_de_interes_inf_adicional.txt")

# Quitar no alimentarias
forestales<- c("Pinaceae", "Burseraceae", "Cupressaceae", "Rutaceae")
df<- filter(df, !(familia %in% forestales)) # filter forestales from data
otrosquitar<-c("Euphorbia", "Dioscorea", "Castilla", "Dahlia", "Lophophora", "Nicotiana", "Tagetes")
df<-filter(df, !(genero %in% otrosquitar))
df<-droplevels(df)

# map
cont<-readShapePoly("conto4mgw/conto4mgw.shp")

#logo
conabio<-readPNG("logo_conabio.png")

### Plot

#same color
plot(cont, lwd=2, border="darkgrey")
title("Crop and crop wild relatives from Mexico (excluding maize)", line=-3)
rasterImage(conabio, xleft=-92, ybottom=26, xright=-88, ytop=30)
legend(x=-118.30, y=16, paste0("SNIB sampling point ", "(n=", nrow(df), ")"), pch= 19, cex=0.8, col=alpha("orangered4", 0.7), bty="n")

points(df$longitud, df$latitud, pch=19, col=alpha("orangered4", 0.7), cex=0.4)

#by family
length(levels(df$familia))
cols<-c("#ff95a6", "#27d094", "#c44283", "#42953c", "#8780e6", "#9c9f2c", "#a1a3ff", "#525e00", "#b7c2ff",
        "#bc7b22", "#0071b2", "#ffad6e", "#028eb0", "#cf4856", "#01bdbf", "#7f3c5d", "#d7c76d",
        "#534c82", "#b4cea5", "#ffa2ce", "#315c2f", "#debae7", "#305a4f", "#f2b9ae", "#515069",
        "#b3cbc4", "#7b433b", rainbow(3))

plot(cont, lwd=2, border="darkgrey")
title("Crop and crop wild relatives from Mexico (excluding maize)", line=-3)
rasterImage(conabio, xleft=-92, ybottom=26, xright=-88, ytop=30)

points(df$longitud, df$latitud, pch=19, col=alpha(cols, 0.7), cex=0.5)

legend(x=-118.30, y=24, levels(df$familia)[1:16], pch= 19, title="Family",
       cex=0.8, col=alpha(cols[1:15], 0.9), box.col="white", bg="white")
legend(x=-111.30, y=21, levels(df$familia)[17:30], pch= 19,
       cex=0.8, col=alpha(cols[16:27], 0.9), box.col="white", bg="white")


# Number of registries per family
p1<-ggplot(df, aes(x=familia, fill=familia)) +
  geom_bar(stat="count") + scale_fill_manual(values=cols) + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
p1

# 

p<-ggplot(df, aes(x=genero, fill=familia, group=familia)) +
  geom_bar(stat="count") + scale_fill_manual(values=cols) + 
  facet_grid(. ~ familia, scales="free_x", space="free_x") 

p + theme(legend.position="none") + 
    theme(strip.text.x =element_text(angle=90)) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1, face="italic", size=8)) +
    ylab("Número de Registros") + xlab("Género")
  

### Seleccionar milperas:
levels(df$genero)

# Keep only main milpa species by filtering frutales and cactus
milperas<-filter(df, !(genero %in% c("Anacardium", "Ananas", "Annona", "Arachis", "Bixa", "Brosimum", "Byrsonima", "Carica", "Chamaedorea", "Crataegus", "Diospyros", "Escontria", "Gossypium", "Helianthus", "Hylocereus", "Lemaireocereus", "Lophocereus", "Myrtillocactus", "Neobuxbaumia", "Nopalea", 
                                     "Manilkara", "Pachycereus", "Parmentiera", "Polaskia", "Pouteria", "Prunus", "Psidium")))
milperas<-filter(milperas, !(genero =="Dysphenia")) # delete because there are no entries
milperas<-droplevels(milperas)
levels(milperas$genero)


# Plot
p<-ggplot(milperas, aes(x=genero, fill=familia, group=familia)) +
  geom_bar(stat="count") + scale_fill_manual(values=cols) + 
  facet_grid(. ~ familia, scales="free_x", space="free_x") 

p + theme(legend.position="none") + 
  theme(strip.text.x =element_text(angle=90)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, face="italic", size=8)) +
  ylab("Número de Registros") + xlab("Género")

