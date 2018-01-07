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

## ---- Figure MW => see repo figTalks

## ---- Fig from Ecography post  => see repo figTalks


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
