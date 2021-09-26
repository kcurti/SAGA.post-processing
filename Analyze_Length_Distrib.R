# This program uses the stratified mean numbers-at-length from the RDAT file (formerly in AXX file) 
     # to calculate the length distribution over all years by summing the number-at-length from individual years
# Changes from old version:  saga is now rdat; lenfq is now num.len; yr.numlen is now t(num.len)


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
  
  # Approximate number of years in each year bin for average length frequencies
  yrs.bin <- 10
  
  output.results <- 'y'# 'y' or 'n'; output csv data files?
  
  omit.survey.list <- data.frame(matrix(NA,nrow=0,ncol=2,dimnames=list(c(),c("Season","Year"))))
  if(survey=='Fall' && lyr>=2018) 
  {
    omit.survey.list[1,] <- c(survey,2018)
  }
}
  

# -------------- Do not modify below here --------------- #


# Load RDAT file
setwd(sp.dir)
rdat.fname <- paste(base.fname,"RDAT",sep=".")
rdat <- dget(rdat.fname)


# Organize year information  
yrs <- rdat$setsum$year
nyr <- length(yrs)


# Stratified mean number-at-length
num.len <- data.frame(rdat$lennumbers)
  colnames(num.len) <- as.character(yrs)
  dim(num.len)
  nyr
if(nrow(omit.survey.list)>0)  {num.len[,as.character(omit.survey.list$Year)] <- NA}
t.num.len <- t(num.len)  


# Sum to obtain total number-per-tow-at-length over years  
tot.num.len <- rowSums(num.len, na.rm=TRUE)
tot.num.len.mat <- as.matrix(tot.num.len)
  colnames(tot.num.len.mat) <- c("num.len")
  

# Calculate proportion-at-length
calc.prop <- function(x){prop <- x/sum(x)}
prop.len <- data.frame(apply(num.len,2,calc.prop))
  colnames(prop.len) <- as.character(yrs)
  colSums(prop.len)  # ensure that sum to 1
  
  
# Reshape proportion-at-length from wide to long format
  # prop.len = dataframe: length(cm) x c(years,length.id)
    # rownames = 7,8,9,10,11,12 = lengths in cm
    # colnames = 1968, 1970, 1971, .... 2012, length
prop.len$length <- round(as.numeric(rownames(prop.len)),digits = 0)
  dim(prop.len)
long.prop <- reshape(prop.len, idvar='length', varying=as.character(yrs), v.names='prop', direction='long', timevar='year', times=yrs)
  # idvar = Variable(s) that ID multiple records from the same group
  # varying = Names of the variables in the wide format that correspond to single variables in the long format
  # v.names = Name of the variable in long format the correspond to multple variables in the wide format
  # timevar = Variable in long format that differentiates multple records from the same group
  rownames(long.prop) <- NULL
# Test that proportions still sum to one in each year
tapply(long.prop$prop, long.prop$year, sum)
  
  
# Reshape numbers-at-length from wide to long format
tmp.numlen <- num.len  # Create tmp data.frame so can add column with length
tmp.numlen$Length <- as.numeric(rownames(tmp.numlen))
long.num.len <- reshape(tmp.numlen, idvar='Length', varying=as.character(yrs), v.names='num.len', direction='long', timevar='year', times=yrs)
  # idvar = Variable(s) that ID multiple records from the same group
  # varying = Names of the variables in the wide format that correspond to single variables in the long format
  # v.names = Name of the variable in long format the correspond to multple variables in the wide format
  # timevar = Variable in long format that differentiates multple records from the same group
  rownames(long.num.len) <- NULL
rm(tmp.numlen)


# Create year bins for average length frequencies
# Number of year bins
nbreaks <- ceiling(length(yrs)/yrs.bin)

# Interval number for each year
yrint.len <- as.numeric(cut(as.numeric(yrs),nbreaks,labels=as.character(1:nbreaks)))
  names(yrint.len) <- as.character(yrs)
  length(yrint.len)==length(yrs)

# Yr bin labels  
yrint.split <- split(yrint.len,yrint.len)
yr.range <- lapply(yrint.split,function(x){range( as.numeric(names(x))) })
yrint.len.key <- c( do.call(rbind, 
                        lapply(yr.range, function(x){ paste(x[1],x[2],sep="-") })
                        ) )
rm(yrint.split, yr.range)  


# Function to calculate average proportions-at-length for investigation of changes in size structure
colMeans.na.rm <- function(x) {colMeans(x, na.rm=TRUE)}
calc.avg.prop <- function(prop.len, yr.int, int.key) {
  t.prop.len <- data.frame(t(prop.len[,as.character(yrs)]))
    colnames(t.prop.len) <- rownames(prop.len)
  split.prop <- split(t.prop.len,yr.int)
  avg.prop <- do.call(rbind, lapply(split.prop, colMeans.na.rm))
    rownames(avg.prop) <- int.key
  avg.prop
}
# Calculate average proportion-at-length
avg.prop <- calc.avg.prop(prop.len, yrint.len, yrint.len.key)


# Calculate average annual length
len.bins <- as.numeric(rownames(num.len))
avg.len.yr <- apply(num.len, 2, function(x) {
  #x <- num.len[,'2015']
  sum(len.bins*x)/sum(x)
})
avg.len.yr.mat <- as.matrix(avg.len.yr)
colnames(avg.len.yr.mat) <- c("avg.len")


# Export numbers-at-length
if (output.results == 'y')  {
  setwd(sp.dir)
  write.csv(tot.num.len.mat, file= paste(sp.abb,"Total.mean.number.at.length.csv",sep="."), row.names=TRUE)
  write.csv(num.len, file= paste(sp.abb,"Annual.mean.number.at.length.csv",sep="."), row.names=TRUE)
  write.csv(long.prop, file= paste(sp.abb,"Proportion.at.length.long.format.csv",sep="."), row.names=FALSE)
  write.csv(long.num.len, file= paste(sp.abb,"Annual.mean.number.at.length.long.format.csv",sep="."), row.names=FALSE)
  write.csv(avg.prop, file= paste(sp.abb,"Average.Proportion.at.length.csv",sep="."), row.names=TRUE)
  write.csv(avg.len.yr.mat, file= paste(sp.abb,"Average.annual.length.csv",sep="."), row.names=TRUE)
}


