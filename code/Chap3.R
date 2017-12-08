#################################################
################################################
## Figures Chapitre 3
##
## first version: Apr 18th 2016
## last modification: Apr 18th 2016
## Kevin Cazelles
##
## R version 3.2.3 (2015-12-10)
## Platform: x86_64-apple-darwin15.0.0 (64-bit)
## Running under: OS X 10.11 (El Capitan)
################################################
################################################




## Path
source("/Users/KevCaz/ownCloud/soutenance/code/settings.R")
# source("/Users/KevCaz/ownCloud/soutenance/code/Chap3.R")

## Europa (for Roslin dataset)
world <- readOGR(dsn="/Users/KevCaz/Documents/Data/Geodata/Pays_adm/countries_shp", layer="countries")
europe <- world[which(world$CONTINENT=="Europe"),]
lEUR0 <- gSimplify(europe, tol=0.001, topologyPreserve=TRUE)

myaxes <- function(seqlon, seqlat, endlat="°N", endlon="°W", ...){
  axis(1, lwd=0, at=seqlon, labels=paste0(abs(seqlon), endlon), ...)
  axis(3, lwd=0, at=seqlon, labels=paste0(abs(seqlon), endlon), ...)
  axis(2, lwd=0, at=seqlat, labels=paste0(abs(seqlat), endlat), ...)
  axis(4, lwd=0, at=seqlat, labels=paste0(abs(seqlat), endlat), ...)
}
load("/Users/KevCaz/ownCloud/Dom_cooc_inter/Data/roselin/roselin_sites.Rdata")


mapsal <- function(filename="fig/mapsal", part=1){

  filename <- paste0(filename, part, ".png")
  png(filename, units="in", width=wi, height=1.25*wi, res=300)
    par(mypar)
    par(mar=c(2.2,2.6,2.2,2.6))
    seqlon <- seq(0,40,10)
    seqlat <- seq(30,75,10)
    plot(c(-1,36), c(39,70), type="n", axes=FALSE, ann=FALSE)
    plot(lEUR0, lwd=1.8, add=TRUE)
    ##
    par(mgp=c(2,.25,1))
    myaxes(seqlon, seqlat, endlon=c("",rep("°E",3)), cex.axis=1.34)
    abline(v=seqlon, h=seqlat, lty=2, lwd=1.6)
    ##
    if (part>1) plot(roselin_sites, add=TRUE, pch=19, col=col1, cex=0.6)

    # addLab("Willow Leaf Communities", line=1.5)
  dev.off()
}
##
cat("figMap.....")
mapsal()
mapsal(part=2)
cat("DONE \n")

################################################
################################################

##
setwd("~/ownCloud/Dom_cooc_inter")
##
load("./data/roselin/roselin_species.Rdata")
##
load("./data/roselin/roselin_presences.Rdata")
load("./data/roselin/roselin_occ_glm.Rdata")
load("./data/roselin/roselin_occ_rf.Rdata")
##
load("./data/roselin/roselin_cooc_all.Rdata")
load("./data/pitcher/pitcher_cooc_all.Rdata")
##
source("/Users/KevCaz/ownCloud/soutenance/code/settings.R")
##
getIdOrder <- function(order_col){
  ##
  ord <- sort(unique(order_col))
  out <- list()
  for (i in 1:length(ord)) out[[i]] <- which(order_col==ord[i])
  ##
  return(out)
}
##
ros_ord <- getIdOrder(roselin_cooc_all$sht_pth)




################################################
############### FIGURE ORDER
################################################

##
id1 <- roselin_cooc_all$id_sp1
id2 <- roselin_cooc_all$id_sp2
##
id_SH <- which(roselin_species$Type[id1]=="Salix" & roselin_species$Type[id2]=="Herbivore")
id_HP <- which(roselin_species$Type[id1]=="Herbivore" & roselin_species$Type[id2]=="Parasitoid")
##
ls_SH <- lapply(ros_ord, function(x) x[x%in%id_SH])
ls_HP <- lapply(ros_ord, function(x) x[x%in%id_HP])


myRect <- function(col="#CCCCCC77") rect(par()$usr[1],-1.96,par()$usr[2],1.96, col=col, border=NA)
myboxplot <- function(x, at, ...) boxplot(x, at=at, add=TRUE, axes=FALSE, ann=FALSE, outline=FALSE, ...)#lwd=0.25, pars = list(boxwex = 0.8, staplewex = 0.65, outwex = 0.25),  ...)

# ros <- getVal(roselin_cooc_all)

##
figSG <- function(filename="./fig/figSG", ls_ord=ls_SH, part=1){
  filename <- paste0(filename, part, ".png")
  png(filename, height=hg, width=wi, unit="cm", res=300)
  par(mypar)
  par(mar=c(1.2,1,.65,.25))# mfrow=c(1,2))
  ##
  plot0(c(0.25,7), c(-3,20))
  if (part==0){
    mtext(3, at=0.65, text=expression(P(X[i],X[j])-P(X[i])*P(X[j])), cex=.85)
  } else mtext(3, at=0, text="Z-score", cex=1)

  if (part>0) myRect()
  ##
  lh=.6
  if (part>1) myboxplot(roselin_cooc_all$Z_hyp[ls_ord[[1]]], at=1, border=col1, lwd=lh)
  if (part>4) myboxplot(roselin_cooc_all$Z_rf[ls_ord[[1]]], at=1.75, border=colg2, lwd=lh)
  #
  if (part==2) {
    lines(c(4,4.5,5,5.5,6,6.5), rep(c(15,19), 3), col=colg1, lwd=1)
    points(c(4.5,5.5,6.5), c(19,19,19), pch=19, col=colg2, cex=.8)
    points(c(4,5,6), c(15,15,15), pch=19, col=colg2, cex=.8)
    text(c(4,5,6), c(15,15,15), labels=c("W"), cex=.35, col=colg1)
    text(c(4.5,5.5,6.5), c(19,19,19), labels=c("G"), cex=.35, col=colg1)
  }
  if (part>2) myboxplot(roselin_cooc_all$Z_hyp[ls_ord[[3L]]], at=3.25, border=col1, lwd=lh)

  if (part>4) myboxplot(roselin_cooc_all$Z_rf[ls_ord[[3L]]], at=4, border=colg2, lwd=lh)
  #
  if (part>3) myboxplot(roselin_cooc_all$Z_hyp[ls_ord[[5L]]], at=5.5, border=col1, lwd=lh)
  if (part>4) myboxplot(roselin_cooc_all$Z_rf[ls_ord[[5L]]], at=6.25, border=colg2, lwd=lh)
  #
  box2(1:2, lwd=1)
  if (part>0) abline(h=0, lwd=lh, col="black", lty=2)
  mtext(2, at=0, text=0, line=.2)

  if (part>1) {
    mtext(1, at=1.4, text="1", line=-.4)
    mtext(1, at=1.4, text=length(ls_ord[[1]]))
  }
  if (part>2) {
    mtext(1, at=3.6, text="3", line=-.4)
    mtext(1, at=3.6, text=length(ls_ord[[3]]))
  }
  if (part>3) {
    mtext(1, at=5.8, text="5", line=-.4)
    mtext(1, at=5.8, text=length(ls_ord[[5]]))
  }

  if (part==6) {
    if (grepl(x=filename,pattern="/fig/figSG")){
      text(5.5, 19, labels="Degree = 1.56", cex=.6)
    } else text(5.5, 19, labels="Degree = 13.78", cex=.6)
  }

  if (part>6) {
    if (grepl(x=filename,pattern="/fig/figSG")){
      text(5.5, 19, labels="Specialists", cex=.6)
    } else text(5.5, 19, labels="Generalists", cex=.6)
  }


  dev.off()
}

cat("fiSG....")
##
figSG(part=0)
figSG()
figSG(part=2)
figSG(part=3)
figSG(part=4)
figSG(part=5)
figSG(part=6)
figSG(part=7)
##
figSG(filename="./fig/figGP", ls_HP, part=5)
figSG(filename="./fig/figGP", ls_HP, part=6)
figSG(filename="./fig/figGP", ls_HP, part=7)
##
cat("DONE \n")



#################################
#################################

##-- selected the different kind of interaction
idsh <- which(roselin_cooc_all$type_sp1=="Salix" & roselin_cooc_all$type_sp2=="Herbivore")
idhp <- which(roselin_cooc_all$type_sp1=="Herbivore" & roselin_cooc_all$type_sp2=="Parasitoid")
##
getComp <- function(id, mode=1){
  out <- matrix(0,1,4)
  id2 <- roselin_cooc_all$id_sp2[id]
  id1 <- roselin_cooc_all$id_sp1[id]
  sg1 <- somers2(roselin_occ_glm[[id2]], roselin_presences[,-1][,id2])[1]
  sr1 <- somers2(roselin_occ_rf[[id2]], roselin_presences[,-1][,id2])[1]
  ss <- somers2(roselin_presences[,-1][,id1], roselin_presences[,-1][,id2])[1]
  out[1] <- ss / sg1
  out[2] <- ss / sr1
  out[3] <- roselin_cooc_all$sht_pth[id]
  if (mode==1) {
    out[4] <- roselin_cooc_all$deg_hs[id]
  } else {
    out[4] <- roselin_cooc_all$deg_ph[id]
  }
  return(out)
}
ressh <- do.call("rbind", lapply(as.list(idsh), getComp))
reshp <- do.call("rbind", lapply(as.list(idhp), getComp, mode=2))
##
getComp2 <- function(id, mode=1){
  out <- matrix(0,1,4)
  idset <-  roselin_cooc_all %$% which(id_sp2==id & sht_pth==1)
  idint <- roselin_cooc_all$id_sp1[idset]
  if (length(idint) > 1) ass <- roselin_presences[,-1][,idint] %>% apply(1,sum) %>% is_greater_than(0) %>% `*`(1)
  else ass <- roselin_presences[,-1][,idint]
  # print(ass)
  sg1 <- somers2(roselin_occ_glm[[id]], roselin_presences[,-1][,id])[1]
  sr1 <- somers2(roselin_occ_rf[[id]], roselin_presences[,-1][,id])[1]
  ss <- somers2(ass, roselin_presences[,-1][,id])[1]
  out[1] <- ss / sg1
  out[2] <- ss / sr1
  # out[3] <- roselin_cooc_all$sht_pth[id]
  if (mode==1) {
    out[4] <- roselin_cooc_all$deg_hs[idset[1]]
  } else {
    out[4] <- roselin_cooc_all$deg_ph[idset[1]]
  }
  return(out)
}

idh <-  roselin_cooc_all$id_sp2[which(roselin_cooc_all$type_sp2=="Herbivore")] %>% unique
resh <- do.call("rbind", lapply(as.list(idh), getComp2))
idp <-  roselin_cooc_all$id_sp2[which(roselin_cooc_all$type_sp2=="Parasitoid")] %>% unique
resp <- do.call("rbind", lapply(as.list(idp), getComp2, mode=2))


## S9
plot_auc <- function(x1, x2, y1, y2, inter, let=1, color=colg2, part=1){
  plot0(c(0.5,5.5), c(-1.2,1.2))
  ida <- which(inter>1)
  idb <- which(inter==1)
  if (part>4) points(x1[ida]-.18, log(y1[ida]), pch=21, col=color, cex=.65, lwd=1.5)
  if (part>5) points(x1[idb], log(y1[idb]), pch=22, col=color, cex=.65, lwd=1.5)
  if (part>6) points(x2+.18, log(y2), pch=24, col=color, cex=.65, lwd=1.5)
  abline(h=0, lty=2)
  ##
  box2(2)
  mtext(side=2, at=0, "0", cex=2, line=.2)
  ##
  mytxt <- c("GLM", "RF")
  if (part>1) mtext(side=3, at=percX(10), mytxt[let], cex=2, line=-.5)
  if (part>2) text(5,1.14, labels="biotic > abiotic", cex=.8, col=colg2)
  if (part>2) text(5,-1.14, labels="biotic < abiotic", cex=.8, col=colg2)
  if (part>3) mtext(1, at=1:5, text=1:5, cex=1.8, line=.1)
}

figAUC <- function(filename="fig/figAUC", part=1){
  ##
  filename <- paste0(filename, part, ".png")
  ##
  png(file=filename, width=wi, height=hg, units="in", res=300)
    layout(rbind(c(3,1), c(3,2)), widths=c(.1,1), heights=c(1, 1))
    par(mypar)
    par(mar=c(1.65, .5, .25, .5))
    ##---
    plot_auc(ressh[,4], resh[,4], ressh[,1], resh[,1], ressh[,3], part=part)
    ##--
    if (part==5) legend("top", legend="one 'non-host' as predictor", pch=21, bty="n", cex=.65, lwd=1.1, lty=NA)
    if (part==6) legend("top", legend="one host as predictor", pch=22, bty="n", cex=.65, lwd=1.1, lty=NA)
    if (part==7) legend("top", legend="all hosts as predictor", pch=24, bty="n", cex=.65, lwd=1.1, lty=NA)
    ##--
    plot_auc(ressh[,4], resh[,4], ressh[,2], resh[,2],  ressh[,3], let=2, part=part)
    ##
    par(mar=c(0,0,0,0))
    plot0()
    text(0, 0, labels="AUC ratio", srt=90, col=colt, cex=1.4)
    ##--
  dev.off()
}

cat("fiAUC....")
figAUC()
figAUC(part=2)
figAUC(part=2)
figAUC(part=3)
figAUC(part=4)
figAUC(part=5)
figAUC(part=6)
figAUC(part=7)
cat("DONE \n")









##
figSht2 <- function(distr, filename="fig/figshtenv", part=1){


  filename <- paste0(filename,part,".png")

  png(file=filename, res=300, width=wi, height=hg, unit="in")
  layout(rbind(1,2), heights=c(1,.25))
  par(mypar)
  par(mar=c(0,2,1.5,1))
  ##
  plot0(c(0.4,4), c(-4,12))
  mtext(3, at=0.65, text=expression(P(X[i],X[j])-P(X[i])*P(X[j])), cex=2.5)
  box2(1:2)
  abline(h=0, lty=2)
  mtext(2, at=0, text=0, line=.4, cex=3)
  ##
  for (i in 1:2){
    boxplot(distr[[i]], at=2*(i-1)+1, add=TRUE, axes=F, outline=F, border=col1, lwd=2)
    if (part>1) boxplot(distr[[2*(i-1)+2]], at=(2*(i-1)+1.65), add=TRUE, axes=F, outline=F,
      border=colg2, lwd=2)
  }
  if (part>1) text(3.2, 11.4, labels="+ abiotic variables", cex=1.32, col=colg2)
  #
  mtext(1, at=1.325, text="Shortest path = 0", cex=2.2, line=.4)
  mtext(1, at=3.325, text="Shortest path > 0", cex=2.2, line=.4)
  dev.off()
}
cat("figSHt2....")
my <- c(5.5, 2.4, 1, .35)
ec <- c(2, 1.5, 1, .85)
distr <- list()
for (i in 1:4) distr[[i]] <- rnorm(100, my[i], ec[i])
figSht2(distr, part=2)
figSht2(distr, part=1)
cat("DONE\n")
