#################################################
################################################
## Figures Chapitre 1
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
# source("/Users/KevCaz/ownCloud/soutenance/code/Chap1.R")


## ---- figMW MacArthur Wilson
n=100
A = 0:n
B = matrix(rep(0,3*(n+1)),101)
C = matrix(rep(0,3*(n+1)),101)
for (k in 0:n) {
  c=0.002
  e=0.002
  B[k+1,1]=1-(1-c)^(n-k)
  C[k+1,1]=1-(1-e)^k
  c=0.001
  e=0.005
  B[k+1,2]=1-(1-c)^(n-k)
  C[k+1,2]=1-(1-e)^k
  c=0.005
  e=0.001
  B[k+1,3]=1-(1-c)^(n-k)
  C[k+1,3]=1-(1-e)^k
}

figMW <- function(filename="./fig/figMW", part=1){
  ##
  filename <- paste0(filename,part,".png")
  ##
  png(filename, height=hg, width=wi, unit="in", res=300)
    layout(matrix(c(1,2),ncol=2), width=c(1,0.24))
    par(mypar)
    par(mar=c(1.5,0.5,1,0), mgp=c(2,1,0))
    pal=c(colt,col1,colt)
    ##

    plot(c(0,100),c(0,0.44), col=0, ann=FALSE, axes=FALSE)
    if (part>3){
      if (part>6) lines(A, B[,2], col=col1)
	    if (part>6) lines(A, C[,2], col=col1, lty=4)
	    if (part>4) lines(A, B[,3], col=colg2)
	    if (part>5) lines(A, C[,3], col=colg2, lty=4)
      mtext(1, text="Specific richness", cex=cex.l, line=.2)
      mtext(3, at=2, text="Rates", cex=cex.l, line=.1)
      box(bty="l")
      # axis(1)
      # axis(2)
      if (part>7) {
      par(mgp=c(2,.4,0))
       points(c(16.53,82.92), c(0.08,0.08), col=c(col1,colt), pch=19)
       axis(1,at=c(16.53,82.92), lwd=0, lwd.ticks=2, labels=c("-","+"), col=colg2, tck=.05)
      }
      if (part>4){
        if (part<6){
          legend("top", c("Colonisation"), lty=c(1), ncol=1, bty="n",
          seg.len=1.6, cex=1)
        } else {
          legend("top", c("Colonisation", "Extinction"), lty=c(1,4), ncol=1, bty="n",
          seg.len=1.6, cex=1)
        }
      }
    }
    ##
    par(mar=c(.5,0,.25,0.5))
    plot0(c(0,10),c(0,10))
    abline(h=8, lwd=2, col=colg2)
    rect(-1,8,11,11, col=colt, border=NA)
    text(5, 9.25, "CONTINENT", cex=.9, col=1)
    if (part>1){
    rect(.5, 6.5, 6, 5.25, col=colg2, border=NA)
    if (part>2) arrows2(x0=mean(c(.5,6)), y0=8, y1=6.5, col=colg2, border=NA, prophead=FALSE)
    text(3.25, 5.875, "ISLAND", cex=.75, col=1)
    }

    if (part>6) {
      arrows2(x0=mean(c(7.5,9.5)), y0=8, y1=2, col=colg2, border=NA, prophead=FALSE)
      rect(7.5, 2, 9.5, 1, col=col1, border=NA)
    }
  dev.off()
}
figMW()
figMW(part=2)
figMW(part=3)
figMW(part=4)
figMW(part=5)
figMW(part=6)
figMW(part=7)
figMW(part=8)







## ---- FIGURE 9 : Post Ecography
emptyply <- function(x,y) plot(x,y, type="n", axes=FALSE, ann=FALSE)
bgcol <- function(col=colt) rect(par()$usr[1],par()$usr[3],par()$usr[2],par()$usr[4], col=col, border=NA)


figEcogr <- function(filename="./fig/figeco", part=1){

  filename <- paste0(filename,part,".png")
  ##
  png(file=filename, res=300, width=wi, height=hg, unit="in")
  ##
  layout(matrix(c(1,2,2,2,3,4,5,6,0,7,7,7,9,8,8,8,9,10,10,10),4), heights=c(0.25,1), widths=c(0.9,1.1,0.1,0.16,0.6))
  par(mypar)
  par(mar=c(0,0,0,0))
  ## Title 1
  plot(c(0,1),c(0,1), type="n", axes=FALSE, ann=FALSE)
  text(0,.5, labels="Regional pool", cex=1, pos=4)

  ## Plot 1
  emptyply(c(0.5,3.5),c(0,5))
  seqx <- c(2,1,3,2,1,3)
  seqy <- c(4,3,3,2,1,1)
  edges <- matrix(c(1,2,1,3,2,4,2,5,4,5,3,6), ncol=2, byrow=TRUE)
  for (i in 1:nrow(edges)) lines(seqx[edges[i,]],seqy[edges[i,]], lwd=4, col=colg2)
  points(seqx, seqy, pch=21, bg=colg2, cex=4, lwd=4)
  text(seqx, seqy, labels=1:6, col=colg1)
  ##
  if (part>1){
  ## Title 2
  par(mar=c(0,0,0,0))
  plot(c(0,1),c(0,1), type="n", axes=FALSE, ann=FALSE)
  text(1,0.5, labels="Local communities", cex=1, pos=2)
  ## Plot 2-A
  par(mar=c(.5,2.5,.5,1))
  emptyply(c(0.5,3.5),c(0,5))
  bgcol(col=mypal[80])
  id_sp1 <- c(1,3,6)
  edges <- matrix(c(1,3,3,6), ncol=2, byrow=TRUE)
  for (i in 1:nrow(edges)) lines(seqx[edges[i,]],seqy[edges[i,]], lwd=3, col=colg2)
  points(seqx[id_sp1], seqy[id_sp1], pch=21, bg=colg2, cex=1.8, lwd=3)
  text(seqx[id_sp1], seqy[id_sp1], labels=id_sp1, cex=.9, col=colg1)
  }

  if (part>2){
  ## Plot 2-B
  emptyply(c(0.5,3.5),c(0,5))
  bgcol(col=mypal[40])
  id_sp1 <- c(1,2,3,4,6)
  edges <- matrix(c(1,2,1,3,2,4,3,6), ncol=2, byrow=TRUE)
  for (i in 1:nrow(edges)) lines(seqx[edges[i,]],seqy[edges[i,]], lwd=3, col=colg2)
  points(seqx[id_sp1], seqy[id_sp1], pch=21, bg=colg2, cex=1.8, lwd=3)
  text(seqx[id_sp1], seqy[id_sp1], labels=id_sp1, cex=.9, col=colg1)
  }

  if (part>2){
  ## Plot 2-C
  emptyply(c(0.5,8),c(0,5))
  bgcol(col=mypal[10])
  id_sp1 <- c(1,2,4,5)
  edges <- matrix(c(1,2,2,4,2,5,4,5), ncol=2, byrow=TRUE)
  for (i in 1:nrow(edges)) lines(seqx[edges[i,]],seqy[edges[i,]], lwd=3, col=colg2)
  points(seqx[id_sp1], seqy[id_sp1], pch=21, bg=colg2, cex=1.8, lwd=3)
  text(seqx[id_sp1], seqy[id_sp1], labels=id_sp1, cex=.9, col=colg1)
  sz_sg <- 0.4
  if (part>3)segments(x0=2-c(sz_sg,sz_sg),y0=2+1.6*c(sz_sg,-sz_sg),x1=2+c(sz_sg,sz_sg),
    y1=2-1.6*c(sz_sg,-sz_sg), lwd=3, col=col2)
  }
  #
  if (part>4){
  seqx <- 6.5+c(1,1,1)
  seqy <- c(.8, 2.5, 4.4)
  edges <- matrix(c(1,2,2,3), ncol=2, byrow=TRUE)
  for (i in 1:nrow(edges)) lines(seqx[edges[i,]],seqy[edges[i,]], lwd=3, col=colg2)
  arrows2(x0=2.8, y0=2.5, x1=6.6, cex.arr=2.8, lwd=1.6, col=colt, border=NA, prophead=FALSE)
  points(seqx, seqy, pch=21, bg=colg2, cex=1.8, lwd=3)
  text(seqx, seqy, labels=c(5,2,1), cex=.9, col=colg1)
  text(4.8, 3.8, "Extinction", cex=.8)
  }

  if (part>5){
  #### Gradient
  par(mar=c(0,0,0,0))
  plot(c(0,1),c(0,1), type="n", axes=FALSE, ann=FALSE)
  text(0.5,0.5,labels="Environmental gradient", srt=270)
  ####
  par(mar=c(1.25,.35,1.25,.35), font=2)
  image(matrix(1:100, nrow=1), col=mypal, axes=FALSE, ann=FALSE)

  ## Title 3
  par(mar=c(0,0,0,0))
  plot(c(0,1),c(0,1), type="n", axes=FALSE, ann=FALSE)
  text(0.5,.5, labels="Colonisation rate")
  #### Plot3
  par(mar=c(.5,0,.5,.5), xaxs="i",yaxs="i")
  seqt <- seq(-10,10, by=0.1)
  plot0(c(0,0.5),range(seqt))
  box2(2:3, col2fill=NULL, col=colg2)
  moy <- -c(0,2,-3,4,7.5,-6)
  ect <- c(4,1.5,1.5,1.2,1.5,1.5)
  wei <- c(3.2,1.4,0.7,1,1.4,1.2)
  #
  seqs <- 1
  if (part>6) seqs <- c(1,6,5)
  if (part>7) seqs <- 1:6
  for (i in seqs) lines(wei[i]*dnorm(seqt,moy[i],ect[i]),seqt, lwd=3.2, col=mypal2[i])
  abline(v=0,h=10,lwd=4)
  seqx <- c(0.38,0.44,0.25,0.4, 0.44, 0.38)
  seqy <- moy
  points(seqx[seqs], seqy[seqs], pch=21, bg=colg2, cex=2.4, lwd=3)
  text(seqx[seqs], seqy[seqs], labels=seqs, cex=.9, col=colg1)
  }
  ####
  if (part>1){
    if (part<=5) {
      par(mar=c(0,0,0,0))
      plot0()
      plot0()
      plot0()
    }
    par(new=TRUE, fig=c(0,1,0,1))
    emptyply(c(0,1),c(0,1))
    text(percX(33), percY(65), labels="Colonisation", cex=.9)
    arrows2(x0=percX(30), y0=percY(55), x1=percX(42), lwd=4, col=colt, border=NA)
  }

  ##
  dev.off()
}
for (i in 1:8) figEcogr(part=i)



######################

figNiche <- function(filename="fig/figniche", part=1){

  filename <- paste0(filename,part,".png")

  png(file=filename, res=300, width=wi, height=hg, unit="in")
  layout(matrix(1:3, ncol=1), height=c(1,.1,.18))

  par(mypar)
  par(mar=c(.25,1,1,.5))

  plot0(c(-10,10), c(0,.25))
  seqx <- seq(-10,10,.01)
  lines(seqx, dnorm(seqx, sd=2))
  points(0, .23, pch=21, bg=colg2, cex=2.4, lwd=3)
  text(0, .23, labels="1", col=colg1)
  mtext(at=-8.5, text="Colonisation rate", cex=cex.t)
  box2(c(1,2))

  if (part>1) {
    points(0, .08, pch=15, cex=7, col=mypal[50])
    points(0, .08, pch=21, bg=colg2, cex=2.4, lwd=3)
    text(0, .08, labels="1", col=colg1)
  }

  if (part>2) {
    points(-7, .18, pch=15, cex=7, col=mypal[85])
    points(7, .18, pch=15, cex=7, col=mypal[15])
  }

  par(mar=c(.25, 2, .25, 2))
  image(matrix(rev(1:100), ncol=1), col=mypal, axes=FALSE, ann=FALSE)

  #### Gradient
  par(mar=c(0,0,0,0))
  plot(c(0,1),c(0,1), type="n", axes=FALSE, ann=FALSE)
  text(0.5,0.7,labels="Environmental gradient")
  text(0.5,0.25, labels="(e.g. temperature)", cex=.75)
  ###

  dev.off()
}

figNiche()
figNiche(part=2)
figNiche(part=3)








############################################
####### FIGURE 10 : Connectance*Gradient

res <- list()
res[[3L]] <- read.delim("~/Documents/Data/Res/Gtib/ResGTIBf/bioconnectotfinal/bioconm.txt", sep="")
res[[2L]] <- read.delim("~/Documents/Data/Res/Gtib/ResGTIBf/bioconnectotfinal/bioconp.txt", sep="")
res[[1L]] <- read.delim("~/Documents/Data/Res/Gtib/ResGTIBf/bioconnectotfinal/bioconc.txt", sep="")
res0 <- apply(read.delim("~/Documents/Data/Res/Gtib/ResGTIBf/bioconnectotfinal/biocon0.txt", sep=""),1,mean)
#
seqx <- 1:length(res0)
mypalg <- colorRampPalette(c("black", col3, "white"))(100)
mypal3 <- mypalg[floor(seq(25,90,len=5))]

figCG <- function(filename = "./fig/figgtib", ind=1){

  filename <- paste0(filename, length(ind), ".png")
  #
  conc <- c(1,11,21,41,81)[ind]
  png(filename, height=hg, width=wi, unit="in", res=300)
    layout(rbind(c(8, 1,2,3), c(0,4,5,6), 7), widths=c(.14,1,1,1), heights=c(1,.1,.4))
    par(mypar)
    par(mar=c(.25, .6, 1.5, 0.1), tcl=-.2)
    Plab <- c("Competition (-/-)", "Predation (-/+)", "Mutualism (+/+)")
    ## trois plots
    ord1 <- c(5,4,6)
    for (i in 1:3){
      plot0(c(1,61), c(0,10))
      k <- 0
      for (j in conc) {
        k <- k+1
        lines(seqx, res0+res[[i]][,j], type="l", col=mypal3[k])
      }
      mtext(Plab[i], side=3, adj=0, line=.1, cex=cex.t)
      box2(1:2)
    }
    ##
    for (i in 1:3){
      par(mar=c(.3, .75, .1, .5))
      image(matrix(rev(1:100), ncol=1), col=mypal, axes=FALSE, ann=FALSE)
    }

    ## gradient
    par(mar=c(2.25, 7, 0.75, 3), mgp=c(1,.025,0))
    image(matrix(1:5), ylab="", axes=FALSE, col=mypal3)
    axis(1, at=seq(0,1,length.out=5), labels=format(c(0,.05,.1,.2,.4), digit=2),
    lwd=0, lwd.ticks=1, cex.axis=.65, col.axis=colt)
    mtext(side=2, text="Connectance   ", at=.16, line=.07, cex=2)
    ##
    par(mar=c(.5, .25, 1.5, 0.05))
    plot0()
    text(0, 0, labels="Species richness", srt=90)
    ##
  dev.off()
}
figCG(ind=NULL)
figCG(ind=1)
figCG(ind=1:2)
figCG(ind=1:3)
figCG(ind=1:4)
figCG(ind=1:5)
