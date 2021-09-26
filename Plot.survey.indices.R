# Program plots stratified means, proportion of positive tows and average length distribution


stand.alone <- 'n'
# Dummy variable to indicate whether the code will be used by itself (stand-alone), or whether it will be called via a skeleton program


if(stand.alone == 'y')  {

  rm(list=ls())
  ls()

  output.results <- 'y'# 'y' or 'n'; output csv data files?
  save.wkspace   <- 'y'

  species <- c("AmericanShad")   # For folder names
  sp.abb <- c("AmShad")        # For file names
  survey <- 'Fall'  #'Fall','Spring','Winter','Shrimp'
  
  if(species=="Mackerel")  {
      sp.dir <- paste("P:\\",species,"\\Fishery-independent.data\\NEFSC.Bottom.Trawl.Survey\\Stratified_Mean_Catch\\Spring-2009-2017-queried-on-04-12-18\\",sep="")  }
  # sp.dir <- paste("P:\\",species,"\\SAGA\\Stratified_Mean_Catch\\Explore_Strata_Set_2017\\",survey,"\\run110",sep="")  }
  if(species=='Blueback' | species=='Alewife')  {
      sp.dir <- paste("P:\\RiverHerring\\SAGA\\1975_2018\\",survey, species, sep="\\")  }
  if(species=="Sturgeon")  {
      sp.dir <- paste("P:\\",species,"\\SAGA\\1975_2015\\Fall",sep="")  }
  if(species=='AmericanShad')  {
      sp.dir <- paste("P:\\",species,"\\SAGA\\2009_2016\\",survey,sep="")  }
  
  # Base file name
  base.fname <- paste(sp.abb,"Fall_2009_2016_DAY",sep="_")

  # Year range
  fyr <- 2009
  lyr <- 2016
  
  ### Figure Details
  fig.type <- 'wmf' #'wmf' or 'gui'

  fig.ht.2 <- 5.5
  fig.wd.3 <- 7.3
  
  fig.ht.1 <- 4.5
  fig.wd.1 <- 6.5

  a.size.1 <- 1.0  # axis text size
  l.size.1 <- 1.0  # label text size
  p.size.1 <- 1.0  # point size (i.e. points on figures)

  color.list <- c("blue","firebrick1","gray17","limegreen","orange")
  pch.list<- c(1,2,5,15,17,16)
  

  # File names
  props.fname      <- paste(sp.abb,"proportion.positive.tows.csv",sep=".")
  means.fname      <- paste(sp.abb,"Stratified.Means.csv",sep=".")
    
  totlen.fname     <- paste(sp.abb,"Total.mean.number.at.length.csv",sep=".")
  yrlen.fname      <- paste(sp.abb,"Annual.mean.number.at.length.csv",sep=".")
  yrlen.long.fname <- paste(sp.abb,"Annual.mean.number.at.length.long.format.csv",sep=".")
  prop.len.fname   <- paste(sp.abb,"Proportion.at.length.long.format.csv",sep=".")
  avg.prop.len.fname <- paste(sp.abb,"Average.Proportion.at.length.csv",sep=".")
  avg.len.fname    <- paste(sp.abb,"Average.annual.length.csv",sep=".")  


  # Read in files
  setwd(sp.dir)
  strat.mean <- read.csv(means.fname,header=TRUE)
    rownames(strat.mean) <- strat.mean$X
    strat.mean$X <- NULL
  prop.pos.tows <- read.csv(props.fname,header=TRUE)
    rownames(prop.pos.tows) <- prop.pos.tows$X
    prop.pos.tows$X <- NULL
  numlen.csv <- read.csv(totlen.fname, header=TRUE)  
  tot.num.len <- numlen.csv$num.len
    names(tot.num.len) <- numlen.csv$X
    rm(numlen.csv)
  t.num.len <- read.csv(yrlen.fname, header=TRUE)
    rownames(t.num.len) <- as.character(t.num.len[,1])
    t.num.len$X <- NULL
    colnames(t.num.len) <- do.call(rbind,strsplit(colnames(t.num.len),'X'))[,2]
  long.num.len <- read.csv(yrlen.long.fname, header=TRUE)  
  long.prop <- read.csv(prop.len.fname, header=TRUE)
  avg.prop <- read.csv(avg.prop.len.fname, header=TRUE)
    rownames(avg.prop) <- avg.prop$X
    avg.prop$X <- NULL
    colnames(avg.prop) <- rownames(t.num.len)
  avg.len.yr.mat <- read.csv(avg.len.fname, header=TRUE)
    colnames(avg.len.yr.mat)[1] <- 'year'
  avg.len.yr <- avg.len.yr.mat[,'avg.len']
    names(avg.len.yr) <- avg.len.yr.mat$year       
 } # end of stand.alone If Statement
  
  

# ------ Function Definitions ----------------------- #


library(Hmisc)
library(lattice)



# Function to plot survey index without error bars
  # y.val <- strat.mean$mean.num
  # y.lab <- c("Number-per-tow")
plot.index <- function(y.val, y.lab)  {
  plot(rownames(strat.mean), y.val, ylim=c(0,max(y.val,na.rm=TRUE)), axes=FALSE, xlab="",ylab="",pch=19, type="b",lty=1)
    axis(side=1, at=axTicks(1), labels=TRUE, padj = -0.3)
    axis(side=2, at=axTicks(2), labels=TRUE, cex.axis=1.0, hadj = 0.7)
    box()
    mtext(y.lab, side=3, cex=0.8, line=0.3, font=1)
  }

# Function to plot survey index with error bars representing 2 standard errors
  # y.val <- strat.mean$mean.num
  # y.se <- strat.mean$se.mean.num
  # y.lab <- c("Number-per-tow")
plot.index.se <- function(y.val, y.se, y.lab)  {
    yplus  <- y.val + 2*y.se
    yminus <- y.val - 2*y.se
  plot(as.numeric(rownames(strat.mean)), y.val, ylim=c(0,max(yplus,na.rm=TRUE)), axes=FALSE, xlab="",ylab="",pch=19, type="b",lty=1)
    errbar(as.numeric(rownames(strat.mean)), y.val, yplus, yminus, cap=0.015, add=TRUE)
    axis(side=1, at=axTicks(1), labels=TRUE, padj = -0.3)
    axis(side=2, at=axTicks(2), labels=TRUE, cex.axis=1.0, hadj = 0.7)
    box()
    mtext(y.lab, side=3, cex=0.8, line=0.3, font=1)
  }



# Function to create final figure
plot.survey.figure <- function(fig.fname, plot.serror)  {

  fig.title <- paste(species,": ", survey, " survey ",fyr,"-",lyr, sep="")  
  windows(height = fig.ht.2, width = fig.wd.3)
  par(las=1)
  par(mfrow=c(2,3))
  par(mar=c(2.2, 1.3, 4, 1) +0.1);  par(oma=c(1.7,1,0.3,0))

  # Stratified mean number
    if(plot.serror == 'n') {plot.index(strat.mean$mean.num, c("Number-per-tow"))}
    if(plot.serror == 'y') {plot.index.se(strat.mean$mean.num, strat.mean$se.mean.num, c("Number-per-tow"))}
  # Stratified mean weight
    if(plot.serror == 'n') {plot.index(strat.mean$mean.wt, c("Weight-per-tow (kg)"))}
    if(plot.serror == 'y') {plot.index.se(strat.mean$mean.wt, strat.mean$se.mean.wt, c("Weight-per-tow (kg)"))}
    mtext("Year", side=1, cex=0.8, line=2.7, font=1)
  # Proportion positive tows
  plot(rownames(prop.pos.tows), prop.pos.tows$prop, ylim=c(0,max(prop.pos.tows$prop, na.rm=TRUE)), axes=FALSE, xlab="", ylab="",pch=19, type="b",lty=1)
    axis(side=1, at=axTicks(1), labels=TRUE, padj = -0.3)
    axis(side=2, at=axTicks(2), labels=TRUE, cex.axis=1.0, hadj = 0.7)
    box()
    mtext("Proportion of positive tows", side=3, cex=0.8, line=0.3, font=1)
  # Average length distribution
  plot(names(tot.num.len),tot.num.len, ylim=c(0,max(tot.num.len)), axes=FALSE, xlab="", ylab="", type="l")
    axis(side=1, at=axTicks(1), labels=TRUE, padj = -0.3)
    axis(side=2, at=axTicks(2), labels=TRUE, cex.axis=1.0, hadj = 0.7)
    box()
    mtext("Average frequency-at-length", side=3, cex=0.8, line=0.3, font=1)
  # Annual length distributions
    x.len <- as.numeric(colnames(t.num.len))
    ncolors <- round(nrow(t.num.len)*1.5)
    line.col <- gray((0:ncolors)/ncolors)
  plot(x.len,t.num.len[1,], ylim=c(0,max(t.num.len, na.rm=TRUE)), axes=FALSE, xlab="", ylab="", type="l", col=line.col[1], lty=1)
    for (r in 2:nrow(t.num.len))  {
      lines(x.len,t.num.len[r,], type="l", col=line.col[r], lty=1)
      }
    axis(side=1, at=axTicks(1), labels=TRUE, padj = -0.3)
    axis(side=2, at=axTicks(2), labels=TRUE, cex.axis=1.0, hadj = 0.7)
    box()
    mtext("Annual frequency-at-length", side=3, cex=0.8, line=0.3, font=1)
    mtext("Length (cm)", side=1, cex=0.8, line=2.7, font=1)
    legend("topright",as.character(c(fyr,lyr)),col=c(line.col[1],line.col[nrow(t.num.len)]), bty="n",lty=1)

  mtext(fig.title, side=3, line=-1.7, cex=0.9, font=2, outer=T)
  if(output.results=='y'){savePlot(file=paste(fig.fname,fig.type,sep="."),type=fig.type)}
  
  } # end of plot.survey.figure function




# Function to plot number and weight per tow on one figure
plot.numb.wt <- function(fig.fname)  {

  windows(height = fig.ht.1, width = fig.wd.1)
  par(mfrow=c(1,1))
  par(oma=c(1.7,2.5,0.3,2.5))
  par(las=1)
  par(mar=c(2.2, 1.3, 1, 1.3) +0.1);

  y.val.1 <- strat.mean$mean.num
  y.lab.1 <- c("Number-per-tow")
  plot(rownames(strat.mean), y.val.1, ylim=c(0,max(y.val.1, na.rm=TRUE)), axes=FALSE, xlab="",ylab="",pch=19, type="b",lty=1, cex=p.size.1)
  lines(rownames(strat.mean), rep(median(y.val.1, na.rm=TRUE),nrow(strat.mean)), lty=1, col="red")
    axis(side=1, at=axTicks(1), labels=TRUE, cex.axis=a.size.1, padj = -0.3)
    axis(side=2, at=axTicks(2), labels=TRUE, cex.axis=a.size.1, hadj = 0.7)
  # Add new plot for second y-axis
  par(new=T)
  y.val.2 <- strat.mean$mean.wt
  y.lab.2 <- c("Weight-per-tow")
  plot(rownames(strat.mean), y.val.2, ylim=c(0,max(y.val.2, na.rm=TRUE)), axes=FALSE, xlab="",ylab="",pch=17, type="b",lty=2, cex=p.size.1)
  lines(rownames(strat.mean), rep(median(y.val.2, na.rm=TRUE),nrow(strat.mean)), lty=2, col="red")
    axis(side=4, at=axTicks(2), labels=TRUE, cex.axis=a.size.1, hadj = 0.3)
  box()

  mtext('Year', side=1, cex=l.size.1, line=2.3, font=1)
  par(las=0)
  mtext(y.lab.1, side=2, cex=l.size.1, line=2.3, font=1)
  mtext(y.lab.2, side=4, cex=l.size.1, line=2.3, font=1)
  legend("top",c("number","weight","median number","median weight"),cex=(a.size.1-0.1), lty=c(1:2,1:2),col=c(rep("black",2),rep("red",2)), bty="n", pch=c(19,17,NA,NA) )

  if(output.results=='y'){savePlot(file=paste(fig.fname,fig.type,sep="."),type=fig.type)}
  } # end of plot.numb.wt function



# Function to plot proportion of positive tows as a separate figure
plot.prop.pos.tows <- function(fig.fname)  {

  windows(height = fig.ht.1, width = fig.wd.1)
  par(mfrow=c(1,1))
  par(oma=c(1.7,2.5,0.3,0))
  par(las=1)
  par(mar=c(2.2, 1.3, 1, 1.3) +0.1);  

  plot(rownames(prop.pos.tows), prop.pos.tows$prop, ylim=c(0,max(prop.pos.tows$prop, na.rm=TRUE)), axes=FALSE, xlab="", ylab="",pch=19, type="b",lty=1)
    axis(side=1, at=axTicks(1), labels=TRUE, padj = -0.3)
    axis(side=2, at=axTicks(2), labels=TRUE, cex.axis=1.0, hadj = 0.7)
    box()

  mtext('Year', side=1, cex=l.size.1, line=2.3, font=1)
  par(las=0)
  mtext("Proportion", side=2, cex=l.size.1, line=2.7, font=1)

  if(output.results=='y'){savePlot(file=paste(fig.fname,fig.type,sep="."),type=fig.type)}
  } # end of plot.prop.pos.tows function



### Length-frequency plots

# Function to plot proportions-at-length for all years in one figure using the lattice package
plot.prop.len.allyrs <- function(fig.fname)  {
  windows(width=10,height=7.5)

  tmp.tr3 <- xyplot(prop~length|as.factor(year),data=long.prop,as.table=TRUE, type='h', col='black', ylab="Proportion",xlab="Length (cm)", scales='same' )  
  print(tmp.tr3,  packet.panel = function(layout, row, column, ...) { 
      layout <- layout[c(2, 1, 3)]
      packet.panel.default(layout = layout, row = column, column = row, ...) 
      })
  if(output.results=='y'){savePlot(file=paste(fig.fname,fig.type,sep="."),type=fig.type)}
  } # end of plot.prop.len.allyrs function



# Function to plot proportions-at-length for all years over multiple figures (specified by nlenfigs)
plot.prop.len.figs <- function(nlenfigs,base.fname)  {
  # nlenfigs <- number of figures
  # base.fname <- paste("./",sp.abb,".LenFrqs.Pt",sep="")
  yrs <- fyr:lyr
  nplots <- floor(length(yrs)/nlenfigs)+1

  for (i in 1:nlenfigs)  {
    fyr.fig <- yrs[(nplots*(i-1)+1)]
    if(i==nlenfigs)  {lyr.fig <- tail(yrs,1)}  else {lyr.fig <- yrs[(nplots*i)]}
    yrs.fig <- fyr.fig:lyr.fig

    prop.fig <- long.prop[long.prop$year%in%yrs.fig,]

    windows(width=7, height=9)
    tmp.tr3 <- xyplot(prop~length|as.factor(year),data=prop.fig,as.table=TRUE, type='h', col='black', ylab="Proportion",xlab="Length (cm)", ylim=c(0,max(long.prop$prop,na.rm=TRUE)), scales='same' )  
    print(tmp.tr3,  packet.panel = function(layout, row, column, ...) { 
        layout <- layout[c(2, 1, 3)]
        packet.panel.default(layout = layout, row = column, column = row, ...) 
      }) # end of print statement

    if(output.results=='y'){
      savePlot(file=paste(base.fname,i,fig.type,sep="."),type=fig.type)}
    } # end of for loop
  } # end of plot.prop.len.figs function
  


# Function to plot numbers-at-length for all years over multiple figures (specified by nlenfigs)
plot.numlen.figs <- function(nlenfigs,base.fname)  {
  # nlenfigs <- number of figures
  # base.fname <- paste("./",sp.abb,".LenFrqs_number.Pt",sep="")
  yrs <- fyr:lyr
  nplots <- floor(length(yrs)/nlenfigs)+1

  for (i in 1:nlenfigs)  {
    fyr.fig <- yrs[(nplots*(i-1)+1)]
    if(i==nlenfigs)  {lyr.fig <- tail(yrs,1)}  else {lyr.fig <- yrs[(nplots*i)]}
    yrs.fig <- fyr.fig:lyr.fig
    numlen.fig <- long.num.len[long.num.len$year%in%yrs.fig,]

    windows(width=7, height=9)
    tmp.tr3 <- xyplot(num.len~Length|as.factor(year),data=numlen.fig,as.table=TRUE, type='h', col='black', ylab="Frequnecy",xlab="Length (cm)", ylim=c(0,max(long.num.len$num.len,na.rm=TRUE)), scales='same' )  
    print(tmp.tr3,  packet.panel = function(layout, row, column, ...) { 
        layout <- layout[c(2, 1, 3)]
        packet.panel.default(layout = layout, row = column, column = row, ...) 
      }) # end of print statement

    if(output.results=='y'){
      fig.fname <- paste(base.fname,i,fig.type,sep=".")
      savePlot(file=fig.fname,type=fig.type)}
    } # end of for loop
  } # end of plot.numlen.figs function



# Function to plot the average proportions-at-length (over defined year intervals)
plot.avg.prop.len <- function(fig.fname)  {
  windows(width=fig.wd.1, height=fig.ht.1)

  par(mar=c(2.2, 1.3, 1, 1.3) +0.1);  par(oma=c(1.7,2.5,0.3,2.5))
  matplot(rownames(t(avg.prop)), t(avg.prop),type="o", xlab="",ylab='',axes=FALSE, ylim=c(0,max(avg.prop,na.rm=TRUE)), col=color.list[1:nrow(avg.prop)], pch=pch.list[1:nrow(avg.prop)], cex=p.size.1, lty=(1:nrow(avg.prop)) )
    axis(side=1, at=axTicks(1), labels=TRUE, cex.axis=a.size.1, padj=-1)
    axis(side=2, at=axTicks(2), labels=TRUE, cex.axis=a.size.1, padj=0.5)
    box()
    mtext("Length (cm)", side=1, cex=l.size.1, line=1.8, font=1)
    mtext("Average proportion-at-length", side=2, cex=l.size.1, line=2, font=1)

    # Add legend
    # par(xpd=NA) if want legend in outer margain
    y.tmp <- max(avg.prop, na.rm=TRUE)
    x.tmp <- mean(as.numeric(colnames(avg.prop)))
    legend(x =x.tmp, y=y.tmp, rownames(avg.prop),bty="n", col=color.list[1:nrow(avg.prop)], pch=pch.list[1:nrow(avg.prop)], lty=(1:nrow(avg.prop)), cex=(a.size.1-0.1), ncol=1) 

  if(output.results == 'y')    {savePlot(file=paste(fig.fname,fig.type,sep="."),type=fig.type)}
    
  }  # end of plot.avg.prop.len function



# Function to plot annual average length
plot.avg.annual.len <- function(fig.fname)  {
  windows(width=fig.wd.1, height=fig.ht.1)

  #par(mar=c(2.2, 1.3, 1, 1.3) +0.1);  par(oma=c(1.7,2.5,0.3,2.5))
  par(mar=c(1, 1.1, 1.0, 0.5) +0.1);  par(oma=c(2,1.8,0.5,0.7))
  plot(names(avg.len.yr), avg.len.yr, axes=FALSE, xlab="",ylab="",pch=19, cex=p.size.1, type="b",lty=1)
  axis(side=1, at=axTicks(1), labels=TRUE, cex.axis=a.size.1, padj=-0.3)
  axis(side=2, at=axTicks(2), labels=TRUE, cex.axis=a.size.1, padj=0.5)
  box()
  mtext("Year", side=1, cex=l.size.1, line=2.0, font=1)
  mtext("Average Length (cm)", side=2, cex=l.size.1, line=2.0, font=1)
  
  if(output.results == 'y')    {savePlot(file=paste(fig.fname,fig.type,sep="."),type=fig.type)}
}  # end of plot.avg.prop.len function





# ------ End of Function Definitions ----------------------- #





# Create summary figure without error bars
plot.survey.figure(fig.fname = paste("./",sp.abb,".Summary.figure", sep=""),  plot.serror = "n")

# Create summary figure with error bars
plot.survey.figure(fig.fname = paste("./",sp.abb,".Summary.figure.se", sep=""),  plot.serror = "y")

# Plot number and weight in one figure
plot.numb.wt(fig.fname = paste("./",sp.abb,".Number.and.Wt.tow", sep=""))

# Plot proportion of positive tows
plot.prop.pos.tows(fig.fname = paste("./",species,".Proportion.of.Pos.Tows", sep=""))

# Plot proportions-at-length for all years in one figure
plot.prop.len.allyrs(fig.fname = paste("./",sp.abb,".LenFrqs.AllYrs", sep=""))

# Plot proportions-at-length for all years across multiple figures
plot.prop.len.figs(nlenfigs = 1, base.fname = paste("./",sp.abb,".LenFrqs.Pt",sep=""))

# Plot numbers-at-length for all years across multiple figures
plot.numlen.figs(nlenfigs = 1, base.fname = paste("./",sp.abb,".LenFrqs_number.Pt",sep=""))

# Plot average proportions-at-length (over defined year intervals)
plot.avg.prop.len(fig.fname = paste("./",sp.abb,".Average.LenFrqs", sep=""))

# Plot average annual length
plot.avg.annual.len(fig.fname = paste("./",sp.abb,".Average.Annual.Length", sep=""))


 