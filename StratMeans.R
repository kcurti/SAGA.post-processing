# Program organizes the AX10.csv file
# Program does not modify variance, SE and CV estimates for bigelow years to account for added variance due to conversion factors;  
      # Use StratMeans_Bigelow_VarAdjust if this modification is necessary (only necessary if combining Albatross and Bigelow surveys into one time series)


stand.alone <- 'n'
# Dummy variable to indicate whether the code will be used by itself (stand-alone), or whether it will be called via a skeleton program


if(stand.alone == 'y')  {
  rm(list=ls())
  ls()

  species <- c("Mackerel")   # For folder names
  sp.abb <- c("Mack")        # For file names

  # Year range
  fyr <- 1968
  lyr <- 2015

  sp.dir <- paste("P:\\",species,"\\SAGA\\Stratified_Mean_Catch\\Spring-1968-2015-queried-on-12-18-15\\",sep="")
  base.fname <- paste(sp.abb,"SPRING_1968-2015",sep="_")

  output.results <- 'y'# 'y' or 'n'; output csv data files?

  omit.survey.list <- data.frame(matrix(NA,nrow=0,ncol=2,dimnames=list(c(),c("Season","Year"))))
  if(survey=='Fall' && lyr>=2018) 
  {
    omit.survey.list[1,] <- c(survey,2018)
  }
}


# -------------- Do not modify below here --------------- #


### Read and organize ax10 file (old ax2.file); Remove extra column, set column names as header, delete old column names in first row
setwd(sp.dir)
ax10.fname <- paste(base.fname,"AX10.csv",sep="_")
ax10 <- read.table(ax10.fname,sep=",",fill=T,skip=3, stringsAsFactors=F, header=FALSE, na.strings=NA)
dim(ax10)
ax10.vars <- c('set','year','mean.num','var.mean.num','se.mean.num','cv.mean.num','sample.var.num','tot.pop.num','var.pop.num',
               'mean.wt','var.mean.wt','se.mean.wt','cv.mean.wt','samp.var.wt','tot.pop.wt','var.pop.wt')
colnames(ax10) <- ax10.vars
rownames(ax10) <- as.character(ax10$year)
# Streamline table for output
strat.mean <- ax10[,c('mean.num','var.mean.num','se.mean.num','cv.mean.num','mean.wt','var.mean.wt','se.mean.wt','cv.mean.wt')]

# Set observations for any omitted zurveys to NA
if(nrow(omit.survey.list)>0)  {strat.mean[as.character(omit.survey.list$Year),] <- NA}

if (output.results == 'y')  {
  setwd(sp.dir)
  write.csv(round(strat.mean,4), file= paste(sp.abb,"Stratified.Means.csv",sep="."), row.names=TRUE)
}
  

rm(ax10.vars)


