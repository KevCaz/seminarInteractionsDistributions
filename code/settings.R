#################################################
################################################
## Figures conntext
##
## first version: Apr 17th 2016
## last modification: Apr 18th 2016
## Kevin Cazelles
##
## R version 3.2.3 (2015-12-10)
## Platform: x86_64-apple-darwin15.0.0 (64-bit)
## Running under: OS X 10.11 (El Capitan)
################################################
################################################

## Path
setwd("/Users/KevCaz/ownCloud/soutenance")

col1 <- "#2199e8"
col2 <- "#ec5840"
col3 <- "#3adb76"
colg1 <- "grey37"
colg2 <- "grey87"
colt <- "grey70"

cex.t <- 2
cex.l <- 1.1*2

wi <- 9
hg <- 5.2

library(graphicsutils)
library(showtext)
####### MAP
library(rgdal)
library(graphicsutils)
library(magrittr)
library(igraph)
library(rphylopic)
library(rgeos)
library(Hmisc)

## Loading Google fonts (http://www.google.com/fonts)
font.add.google("Arya", "arya")
## Automatically use showtext to render text for future devices
showtext.auto()


mypar <- list(
  cex = cex.t,
  cex.lab = cex.t*1.2,
  cex.axis = cex.t,
  las = 1,
  lwd = cex.t*1.6,
  bg = "transparent",
  fg= colt,
  col.axis = colt,
  family="arya")


  mypal <- colorRampPalette(c(colg1, col1, "white"))(100)
  mypal2 <- c(mypal[50],mypal[40],mypal[65],mypal[25],mypal[10],mypal[80])
  myblue <- col1
