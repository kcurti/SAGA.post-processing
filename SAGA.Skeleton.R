# 2019 Jun: Updated paths
# 2017 Apr: Deleted Bigelow variance adjsutments because not sure correct
# 2015 Dec: Modified program because now using updated SAGA
# Created by K.Curti


rm(list=ls())
ls()

output.results <- 'y' # 'y' or 'n'; output csv data files?
save.wkspace   <- 'y'

# Species/season
species <- c("Blueback")   # For folder names, e.g. Mackerel
sp.abb <- c("Blueback")    # For file names, e.g. Mack
survey <- c('Fall')     #'Fall','Spring','Winter','Shrimp'

# Year range
fyr <- 1975
lyr <- 2019
  if(fyr==1976 && survey=='Spring') {folder.fyr <- 1975}  else  {folder.fyr <- fyr}

# Approximate number of years in each year bin for average length frequencies
yrs.bin <- 3

# Directories - need to manually change for Mackerel
net.dir <- '//net.nefsc.noaa.gov/home0/kcurti'
if(species=="Mackerel")  {
    sp.dir <- file.path(net.dir,species,"Fishery-independent.data/NEFSC.Bottom.Trawl.Survey/Stratified_Mean_Catch/Spring-2009-2019-queried-on-06-29-20",sep="")  }
if(species=='Blueback' | species=='Alewife')  {
    sp.dir <- file.path(net.dir,"RiverHerring/SAGA",paste(folder.fyr,lyr,sep="_"),survey, species)  }
if(species=="Sturgeon")  {
    sp.dir <- file.path(net.dir,species,"SAGA/1975_2015/Fall")  }
if(species=='AmericanShad')  {
    sp.dir <- file.path(net.dir,species,"Fishery-independent.data/SAGA",paste(folder.fyr,lyr,sep="_"),survey)  }

# Base file name
if(species%in%c('Blueback','Alewife','AmericanShad'))  
{
  base.fname <- paste(sp.abb,survey,fyr,lyr,"DAY",sep="_")
}  else if (species=='Mackerel') {
  base.fname <- paste(sp.abb,survey,paste(fyr,lyr,sep='-'),sep="_")
}  else {
  base.fname <- paste(sp.abb,survey,fyr,lyr,sep="_")
}


# Years/surveys to omit
omit.survey.list <- data.frame(matrix(NA,nrow=0,ncol=2,dimnames=list(c(),c("Season","Year"))))
if(survey=='Fall' && lyr>=2017) 
{
  omit.survey.list[1,] <- c(survey,2017)
}

# Figure details
fig.type <- 'wmf'

fig.ht.2 <- 5.5
fig.wd.3 <- 7.3

fig.ht.1 <- 4.5
fig.wd.1 <- 6.5

a.size.1 <- 1.0  # axis text size
l.size.1 <- 1.0  # label text size
p.size.1 <- 1.0  # point size (i.e. points on figures)

color.list <- c("blue","firebrick1","gray17","limegreen","orange")
pch.list<- c(1,2,5,15,17,16)
  

# -------------------------------------------------------------------------------- #


# Calculate proportion of positive tows
source(file.path(net.dir,"Program_Code/R/SAGA/Analyze_Prop_PosTows.R"))
  
  
# Organize stratified means (removed old code that calculated adjusted variance for Bigelow years because wasn't sure if the code was correct)
source(file.path(net.dir, "Program_Code/R/SAGA/StratMeans.R"))


# Compute average length distribution
source(file.path(net.dir, "Program_Code/R/SAGA/Analyze_Length_Distrib.R"))

  
# Plot survey indices
source(file.path(net.dir, "Program_Code/R/SAGA/Plot.survey.indices.R"))


# Save workspace
if(save.wkspace == 'y')  {save.image(file = paste(base.fname,"RDATA",sep="."))}



########################################################################


##### Load previous workspace

# Species/season
species <- c("Mackerel")   # For folder names, e.g. Mackerel
sp.abb <- c("Mack")    # For file names, e.g. Mack
survey <- c('Spring')     #'Fall','Spring','Winter','Shrimp'

# Year range
fyr <- 2009
lyr <- 2019
if(fyr==1976 && survey=='Spring') {folder.fyr <- 1975}  else  {folder.fyr <- fyr}

# Directories - need to manually change for Mackerel
net.dir <- '//net.nefsc.noaa.gov/home0/kcurti'
if(species=="Mackerel")  {
  sp.dir <- file.path(net.dir,species,"Fishery-independent.data/NEFSC.Bottom.Trawl.Survey/Stratified_Mean_Catch/Spring-2009-2019-queried-on-06-29-20",sep="")  }
if(species=='Blueback' | species=='Alewife')  {
  sp.dir <- file.path(net.dir,"RiverHerring/SAGA",paste(folder.fyr,lyr,sep="_"),survey, species)  }
if(species=="Sturgeon")  {
  sp.dir <- file.path(net.dir,species,"SAGA/1975_2015/Fall")  }
if(species=='AmericanShad')  {
  sp.dir <- file.path(net.dir,species,"Fishery-independent.data/SAGA",paste(folder.fyr,lyr,sep="_"),survey)  }

# Base file name
if(species%in%c('Blueback','Alewife','AmericanShad'))  
{
  base.fname <- paste(sp.abb,survey,fyr,lyr,"DAY",sep="_")
}  else if (species=='Mackerel') {
  base.fname <- paste(sp.abb,survey,paste(fyr,lyr,sep='-'),sep="_")
}  else {
  base.fname <- paste(sp.abb,survey,fyr,lyr,sep="_")
}

load(file = file.path(sp.dir, paste(base.fname,"RDATA",sep=".")))




