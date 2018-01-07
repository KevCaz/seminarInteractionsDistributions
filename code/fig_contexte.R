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
source("/Users/KevCaz/ownCloud/soutenance/code/settings.R")



ls_img <- as.list(list.files("./img/animals", full=T)) %>%
  lapply(png::readPNG)

  mapw <- readOGR(dsn="/Users/KevCaz/ownCloud/soutenance/img/ne_110m_land/",
  "ne_110m_land")

coord <- cbind(
    x = c(.52, .84, .535, .25, .72, .36, .6),
    y = c(.7, .36, .55, .7, .7, .41, .1)
  )
cexi = c(.038, 0.05, .125, rep(.1,2), 0.09, .12)


figmapw <- function(filename="fig/figmapw", part=1){

  filename <- paste0(filename,part,".png")
  png(file=filename, res=300, width=wi, height=hg, unit="in")

  par(mypar)
  par(mar=c(0,0,0,0))
  plot0(c(-187, 185), c(-94, 102))
  plot(mapw, add=T, lwd=1.2)

  if (part>1){
    id <- 4
    if (part>2) id <- 1:length(ls_img)
    #
    for (i in id){
      add_phylopic_base(ls_img[[i]], coord[i,1], coord[i,2], ysize=cexi[i],
          color="grey75", alpha=1)
    }
  }

  dev.off()
}

figmapw()
figmapw(part=2)
figmapw(part=3)











# myspec <-  function(nb, minx, maxx, miny=minx, maxy=maxx, ...){
#   points(runif(nb,minx,maxx), runif(nb,miny,maxy), ...)
# }


############################
########################################################

figdem <- function(filename="fig/figdem", part=1){

  filename <- paste0(filename,part,".png")

  png(filename, width=wi, height=hg, unit="in", res=300)
  layout(matrix(1:2, ncol=2), width=c(1,1))
  par(mypar)

  ##
  par(mar=c(1,0,0,2))
  plot(quebec, lwd=1, main="")
  plot(Presok[[1]], add=TRUE, col="#BFBFBFaa", border="transparent")
  if (part>4) plot(Presok[[3]], add=TRUE, col=paste0(col1,"aa"), border="transparent")
  plot(quebec, lwd=1.5, add=TRUE)
  mtext(1, text="Today", cex=2.8, line=-0.1)
  ##

  if (part>0){
  par(mar=c(1,2,0,0))
  plot(quebec, lwd=1, main="")
  mtext(1, text="Tomorrow", cex=2.8, line=-0.1)
  if (part==2) text(-72,54,label="?", cex=8, col=col2)
  if (part>2){
    plot(Presok[[2]], add=TRUE, col="#BFBFBFaa", border="transparent")
    if (part>4) plot(Presok[[4]], add=TRUE, col=paste0(col1,"aa"), border="transparent")
    plot(quebec, lwd=1.5, add=TRUE)
    if (part>6) text(-72,54,label="?", cex=8, col=col2)
    }
  }

  if (part>5){
  par(mar=c(.5,.5,.5,.5), new=T, fig=c(.25,.65,.4, 1))
  plot(metaweb3,
    vertex.frame.color=colt,
    # vertex.frame.width=2,
    vertex.color=colt,
    vertex.label.color=colg1,
    vertex.label.font=1,
    vertex.size=15,
    vertex.label="",#LETTERS[1:ncol(matweb3)],
    layout=layout.fruchterman.reingold,
    edge.arrow.size=0,
    edge.width=1.8,
    edge.color=colt
  )
  }

  par(new=T, fig=c(0,1,0,1))
  if (part==4 | part==5) {
    plot0()
    add_phylopic_base(sug, .44, .35, ysize=.17, alpha=1, color=colg2)
    if (part==5) add_phylopic_base(abi, .45, .6, ysize=.16, alpha=1, color=col1)

  }
  dev.off()
}

############################
load("/Users/KevCaz/Documents/Presentations/CSBQ-2015/fig/Presok.Rdata")
path <- "/Users/KevCaz/Documents/Presentations/CSBQ-2015/fig"
quebec <- readOGR(dsn=path, layer="quebec")

sug <- png::readPNG("img/acer.png")
abi <-  png::readPNG("img/abies.png")

matweb3 <- matrix(rbinom(12*12,1,0.2),12,12)
diag(matweb3) <- 0
metaweb3 <- graph.adjacency(matweb3)


figdem(part=0)
figdem()
figdem(part=2)
figdem(part=3)
figdem(part=4)
figdem(part=5)
figdem(part=6)
figdem(part=7)


figweb <- function(){
  png("fig/figweb.png", width=hg, height=hg, unit="in", res=300)
    par(mypar)
    par(mar=c(.5,.5,.5,.5))
    plot(metaweb3,
      vertex.frame.color=colt,
      # vertex.frame.width=2,
      vertex.color=colt,
      vertex.label.color=colg1,
      vertex.label.font=1,
      vertex.size=15,
      vertex.label="",#LETTERS[1:ncol(matweb3)],
      layout=layout.fruchterman.reingold,
      edge.arrow.size=0,
      edge.width=1.8,
      edge.color=colt
    )
  dev.off()
}
figweb()


################################


figNiche <- function(filename="fig/figgrad", part=1){

  filename <- paste0(filename,part,".png")

  png(file=filename, res=300, width=wi, height=hg, unit="in")
  layout(matrix(1:3, ncol=1), height=c(1,.1,.18))

  par(mypar)
  par(mar=c(.25,1,1,.5))
  plot0(c(-10,10), c(0,.35))
  seqx <- seq(-10,10,.01)
  if (part>1) lines(seqx, dnorm(seqx, -3, sd=1.6))
  if (part>2) lines(seqx, dnorm(seqx, 4, sd=1.4), col=col1)
  if (part>3) text(1,.32, labels="Independent", cex=1.6, col=colg2)
  mtext(at=-8, text="Presence probability", cex=cex.t)
  box2(c(1,2))

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
figNiche(part=4)
