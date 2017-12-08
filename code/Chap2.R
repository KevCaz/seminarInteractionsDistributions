#################################################
################################################
## Figures Chapitre 2
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
## source("/Users/KevCaz/ownCloud/soutenance/code/Chap2.R")

bgcol2 <- function(color1=col1, color2=colg2){
  plot0(c(0,1), c(0,1))
  polygon(c(0,1,0), c(0,0,1), col=color1, border=NA)
  polygon(c(0,1,1), c(1,1,0), col=color2, border=NA)
}

bgcol3 <- function(color1=col1, color2=colg2, color3=col3){
  plot0(c(0,1), c(0,1))
  rect(0, 0, .33, 1, col=color1, border=NA)
  rect(.33, 0, .66, 1, col=color2, border=NA)
  rect(.66, 0, 1, 1, col=color3, border=NA)
}



figCooc <- function(filename="fig/figcooc", part=1){

  filename <- paste0(filename,part,".png")

  png(file=filename, res=300, width=wi, height=hg, unit="in")
  par(mypar)
  layout(cbind(0, rbind(c(1,0,2), c(0,5,0), c(3, 0, 4)), 0))
  par(mar=c(1,1,1,1), xaxs="i", yaxs="i")
  cexp <- 8

  if (part>1) {
    plot0()
    box2(1:4)
  } else {
    plot0()
  }
  if (part>2) {
    plot0(fill=colg2)
    text(0, 0, labels="1", cex=6, col=colg1)
  } else plot0()
  if (part>3) {
    plot0(fill=col1)
    text(0, 0, labels="2", cex=6, col=colg1)
  } else plot0()
  if (part>4) {
    bgcol2()
    text(c(.25,.75), c(.25,.75), labels=c(1,2), cex=6, col=colg1)
  } else plot0()

  plot0()
  if (part>5) lines(c(-.65,.65), c(0,0), lwd=5)
  points(-.65, 0, pch=21, bg=colg2, cex=10, lwd=3)
  text(-.65, 0, labels="1", col=colg1, cex=4)
  points(.65, 0, pch=21, bg=colg2, cex=10, lwd=3)
  text(.65, 0, labels="2", col=colg1, cex=4)


  ##
  dev.off()
}

figCooc()
figCooc(part=2)
figCooc(part=3)
figCooc(part=4)
figCooc(part=5)
figCooc(part=6)






### FIGURE C-score
nsite <- 144
presA <- rbinom(nsite,1,0.35)
presB <- rbinom(nsite,1,0.35)*2
prest3 <- prest2 <- prest <- presA+presB
prest2[prest2==3] <- runif(length(prest2[prest2==3]))>.5

prest3 <- (rbinom(length(prest), 1, 0.4))*3

# prest2[prest2==3] <- runif(length(prest2[prest2==3]))>.5
mypalcb <- c("transparent", col1, colg2, mypal[15])

figCB <- function(filename="./fig/figCS"){

  filename <- paste0(filename, 1:3, ".png")

  png(filename[1L], height=hg, width=hg, unit="in", res=300)
  par(mypar)
  par(mfrow=c(12,12), mar=c(0.1,0.1,0.1,0.1), xaxs="i", yaxs="i")
    for (i in 1:nsite){
      if (prest[i]>2) {
        bgcol2()
      } else plot0(fill=mypalcb[1+prest[i]])
    }
  dev.off()
  ##
  png(filename[2L], height=hg, width=hg, unit="in", res=300)
  par(mypar)
  par(mfrow=c(12,12), mar=c(0.1,0.1,0.1,0.1), xaxs="i", yaxs="i")
    for (i in 1:nsite){
      plot0(fill=mypalcb[1+prest2[i]])
    }
  dev.off()
  ##
  png(filename[3L], height=hg, width=hg, unit="in", res=300)
  par(mypar)
  par(mfrow=c(12,12), mar=c(0.1,0.1,0.1,0.1), xaxs="i", yaxs="i")
    for (i in 1:nsite){
      if (prest3[i]>2) {
        bgcol2()
      } else plot0(fill=mypalcb[1+prest3[i]])
    }
  dev.off()


}
figCB()




nsite <- 144
presA <- rbinom(nsite,1,0.45)
presB <- rbinom(nsite,1,0.45)*2
presA[1:50] <- 0
presB[96:144] <- 0
prest2 <- prest <- presA+presB
mypalcb <- c("transparent", col1, colg2, mypal[15])


figAB <- function(filename="./fig/figab", part=1){
  filename <- paste0(filename, part, ".png")
  png(filename, height=hg, width=wi, unit="in", res=300)
    layout(cbind(0, 0, 0, matrix(1:144, 12, 12, byrow=T), 0, 145, 0, 0))
    par(mypar)
    par(mar=rep(0.05, 4), xaxs="i", yaxs="i")
    for (i in 1:nsite){
      if (prest[i]>2) {
        bgcol2()
      } else plot0(fill=mypalcb[1+prest[i]])

    }
    if (part>1){
      par(mar=c(1, 0, 1,0))
      image(matrix(1:100, nrow=1), col=mypal, axes=FALSE, ann=FALSE)
    }
  dev.off()
}

figAB()
figAB(part=2)






##
figSht <- function(distr, filename="fig/figsht", part=1){

  filename <- paste0(filename,part,".png")

  png(file=filename, res=300, width=wi, height=hg, unit="in")

  layout(rbind(1,2), heights=c(1,.25))
  par(mypar)
  par(mar=c(0,2,1.5,1))
  ##
  plot0(c(0.4,3.6), c(-4,12))
  mtext(3, at=0.65, text=expression(P(X[i],X[j])-P(X[i])*P(X[j])), cex=2.5)
  box2(1:2)
  abline(h=0, lty=2)
  mtext(2, at=0, text=0, line=.4, cex=3)
  ##
  if (part>0){
    for (i in 1:part){
      boxplot(distr[[i]], at=i, add=TRUE, axes=F, outline=F, border=col1, lwd=2)
    }
  ##
    par(mar=c(1, 2, .25, 1))
    ecr <- 4
    plot0(c(0,60), c(0,2))
    lines(5.25+1:2*ecr, c(1,1), lwd=3)
    points(5.25+1:2*ecr, c(1,1), pch=19, col=colg2, cex=1.6)
    if (part>1){
      lines(22.25+1:3*ecr, c(1,1,1), lwd=3)
      points(22.25+1:3*ecr, c(1,1,1), pch=19, col=c(colg2, colg1, colg2), cex=1.6)
    }
    if (part>2){
      lines(39.25+1:4*ecr, c(1,1,1,1), lwd=3)
      points(39.25+1:4*ecr, c(1,1,1,1), pch=19, col=c(colg2, colg1, colg1, colg2), cex=1.6)
    }
  ##
  }
  dev.off()
}

my <- c(5.5, 3, 0.25)
ec <- c(2, 1.5, 1)
distr <- list()
for (i in 1:3) distr[[i]] <- rnorm(100, my[i], ec[i])
figSht(distr, part=0)
figSht(distr)
figSht(distr, part=2)
figSht(distr, part=3)




##
figDeg <- function(distr, filename="fig/figdeg", part=1){

  filename <- paste0(filename,part,".png")

  png(file=filename, res=300, width=wi, height=hg, unit="in")

  layout(rbind(1,2), heights=c(1,.25))
  par(mypar)
  par(mar=c(0,2,1.5,1))
  ##
  plot0(c(0.4,3.6), c(-4,12))
  mtext(3, at=0.65, text=expression(P(X[i],X[j])-P(X[i])*P(X[j])), cex=2.5)
  box2(1:2)
  abline(h=0, lty=2)
  mtext(2, at=0, text=0, line=.4, cex=3)
  ##
  if (part>0){
    for (i in 1:part){
      boxplot(distr[[i]], at=i, add=TRUE, axes=F, outline=F, border=col1, lwd=2)
    }
    ##
    par(mar=c(0.5,2.2,0,1))
    plot0(c(0.4,3.6), c(0,1))
    seqc <- seq(0,2*pi,pi/10)
    id <- list(1, 2*(1:8), 1:20)
    for (i in 1:part){
      for (j in id[[i]]){
        # print(j)
        lines(c(i,i+.36*cos(seqc[id[[i]][j]])), c(.5, .5+.36*sin(seqc[id[[i]][j]])), col=colg1, lwd=2)
      }
      points(i, .5, pch=21, bg=colg2, cex=1.1, lwd=.8)
      points(i+.36*cos(seqc[id[[3]]]), .5+.36*sin(seqc[id[[3]]]), pch=21, bg=colg2, cex=.4, lwd=.5)
    }
  }
  # points(.5, .5, pch=21, bg=colg2, cex=4, lwd=3)
  # text(.5, .5, labels="1", cex=1.6, col=colg1)
  dev.off()
}

my <- c(5.5, 3, 0.25)
ec <- c(2, 1.5, 1)
distr <- list()
for (i in 1:3) distr[[i]] <- rnorm(100, my[i], ec[i])
figDeg(distr, part=0)
figDeg(distr)
figDeg(distr, part=2)
figDeg(distr, part=3)




##### LAST FIGURE

nsite <- 144
presA <- rbinom(nsite, 1, rep(seq(0,.75,.15), each=nsite/6))
# prest2 <- prest <- presA+presB
presB <- presA
presB[as.logical(rbinom(nsite, 1, .27))] <-0
presC <- presB
presC[as.logical(rbinom(nsite, 1, .17))] <-0


sug <- png::readPNG("img/acer.png")
abi <-  png::readPNG("img/abies.png")
sawf <-  png::readPNG("img/parasit.png")
para <-  png::readPNG("img/sawfly.png")

##
figLast <- function(filename="fig/figLast", part=1){

  filename <- paste0(filename, part, ".png")
  ##
  png(file=filename, res=300, width=wi, height=hg, unit="in")
    par(mypar)
    layout(rbind(0, cbind(1, matrix(2:145, 12, 12, byrow=T), 0, 146, 0),0,0), widths=c(.9, rep(.05,15)))
    ##
    par(mar=rep(0.1, 4), xaxs="i", yaxs="i")
    plot0()
    if (part==2) {
      lines(c(-.3, .6), c(-.67, -.67), lwd=4)
    }
    if (part>2) {
      text(c(.2,.5), c(-.67, -.67), labels=c("W", "E"), cex=c(2,6))
    }
    if (part>3) {
      lines(c(-.3, -.3), c(-.67, 0), lwd=4)
      text(c(.2,.5), c(-.2, -.2), labels=c("W", "E"), cex=c(4,4))
    }
    if (part>4) {
      text(c(.2,.5), c(.55, .55), labels=c("W", "E"), cex=c(6,2))
      lines(c(-.3, -.3), c(0.1, 0.7), lwd=4)
    }
    if(part>5) {
      arrows2(-.8, -.95, -.8, .85, col=colg2, border=NA)
      text(-.33, .94, "Energetic constraints", cex=2.6)
      text(-0, -.95, "Chapter 4", cex=2.6)
    }
    add_phylopic_base(sug, .35, .17, ysize=.2, alpha=1, color=colg2)
    if(part==2) {
      add_phylopic_base(abi, .77, .17, ysize=.15, alpha=1, color=col1)
    }
    if(part>3) add_phylopic_base(sawf, .35, .55, ysize=.18, alpha=1, color=col2)
    if(part>4) add_phylopic_base(para, .35, .87, ysize=.15, alpha=1, color=col3)

    ##
    par(mar=c(0.07,0.07,0.07,0.07))
    ##
    if (part>3){
      if (part>4){
        pres <- presA + presB + presC
      } else pres <- presA + presB
    } else pres <- presA
    ##
    for (i in 1:nsite){
      if (pres[i]){
        if (pres[i]==1) plot0(fill=colg2)
        if (pres[i]==2) bgcol2(colg2, col2)
        if (pres[i]==3) bgcol3(colg2, col2, col3)
      } else plot0()
    }
    #
    image(matrix(1:100, nrow=1), col=mypal, axes=FALSE, ann=FALSE)
    ##
  dev.off()
}

figLast()
figLast(part=1)
figLast(part=2)
figLast(part=3)
figLast(part=4)
figLast(part=5)
figLast(part=6)
figLast(part=7)
