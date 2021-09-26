# Program takes the AX12.csv file (former *.AX8 file), and calculates the proportion of positive tows


stand.alone <- 'n'
# Dummy variable to indicate whether the code will be used by itself (stand-alone), or whether it will be called via a skeleton program

  
if(stand.alone == 'y')  {
  rm(list=ls())
  ls()

  species <- c("RiverHerring")  # For folder names
  sp.abb <- c("Blueback")       # For file names

  # Year range
  fyr <- 1975
  lyr <- 2015
  
  sp.dir <- paste("P:\\", species, "\\SAGA\\1975_2015\\Fall\\", sp.abb ,sep="")
  base.fname <- paste(sp.abb,"FALL_1975_2015_DAY",sep="_")
  
  output.results <- 'y'# 'y' or 'n'; output csv data files?

  omit.survey.list <- data.frame(matrix(NA,nrow=0,ncol=2,dimnames=list(c(),c("Season","Year"))))
  if(survey=='Fall' && lyr>=2017) 
  {
    omit.survey.list[(nrow(omit.survey.list)+1),] <- c(survey,2017) # vessel breakdown
  }
  if(survey=='Spring' && lyr>=2020) 
  {
    omit.survey.list[(nrow(omit.survey.list)+1),] <- c(survey,2020) # covid
  } 
}



# -------------- Do not modify below here --------------- #


# Read in file and organize data
setwd(sp.dir)
ax12.fname <- paste(base.fname,"AX12.csv",sep="_")
orig <- read.table(ax12.fname,sep=",",fill=T,skip=0, stringsAsFactors=F, header=TRUE)
# Remove extra column
dim(orig)
ax12 <- orig[,!apply(is.na(orig),2,all)]
dim(ax12)

# Sum Number_tows (ntows) and Non_Zero_Tows (pos.tows) by year (sum over strata)
ntows    <- tapply(ax12$Number_Tows,   ax12$Year, sum)
pos.tows <- tapply(ax12$Non_Zero_Tows, ax12$Year, sum)

# Concatenate and calculate proportion of positive tows
prop.pos.tows <- data.frame(cbind(ntows,pos.tows))
prop.pos.tows$prop <- round((pos.tows/ntows),3)

# Set observations for any omitted zurveys to NA
if(nrow(omit.survey.list)>0)  {prop.pos.tows[as.character(omit.survey.list$Year),] <- NA}

# Export proportions
if (output.results == 'y')  {
  setwd(sp.dir)
  write.csv(prop.pos.tows, file= paste(sp.abb,"proportion.positive.tows.csv",sep="."), row.names=TRUE)
  }


rm(ntows, orig, pos.tows)



