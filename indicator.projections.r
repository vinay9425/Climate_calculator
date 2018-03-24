# Master code to calculate annual climate indicators for India
# using observed and statistically downscaled weather station data (ARRM)
# and statistically downscaled gridded data (NEX)
#
# This code was created by Katharine Hayhoe from Texas Tech University
# For questions on the indicator code contact: katharine.hayhoe@ttu.edu
# For questions on the observations contact: ranjini.swaminathan@ttu.edu
# For questions on ARRM downscaling contact: anne.stoner@ttu.edu
# For questions on NEX downscaling contact: bthrasher@gmail.com
#
# March 2017
#
oldw <- getOption("warn")
options(warn = -1)

# ONE. Define the variables being used
library('ncdf4')
mod <- c('CCSM4','GFDL-ESM2G','IPSL-CM5A-LR','MIROC5','MPI-ESM-LR','MRI-CGCM3')
ens <- c('r6i1p1','r1i1p1','r1i1p1','r1i1p1','r1i1p1','r1i1p1')
scen <- c('rcp45','rcp85')

vars <- c('maximum daytime temperature','minimum nighttime temperature','average daily temperature','precipitation','cooling degree-days')
grid.var <- c('tasmax','tasmin','dummy','pr','dummy') # netCDF variables
name.var <- c('tmax','tmin','tavg','pr','cdd')

years <- c(1950:2100)
months <- c('jan','feb','mar','apr','may','jun','jul','aug','sep','oct','nov','dec')
begin.year <- c(1950,1980,2010,2040,2070)
begin.year <- begin.year-1949
end.year <- c(1979,2009,2039,2069,2099)
end.year <- end.year-1949

# TWO. functions to ask questions

# containing the interactive questions to ask the user
#

# QUESTION 1
#


read.q0a <- function()
{
 n <- readline(prompt="Please enter the name of the input directory. Make sure to include the final slash at the end:")
 return(as.character(n))
}

read.q0b <- function()
{
 n <- readline(prompt="Please enter the name of the output directory:")
 return(as.character(n))
}

read.q0c <- function()
{
 n <- readline(prompt="Please enter the name of the directory containing the code. Make sure to include the final slash at the end:")
 return(as.character(n))
}

read.q1 <- function()
{ 
  cat("Do you want to generate the data to create a map of India or a time series for an individual weather station?")
  n <- readline(prompt=" Enter 1 for a map, 2 for a weather station: ")
  n <- as.integer(n)
  if((!grepl("^[0-9]+$",n))|(n>2)|(n<0))
  {
    return(read.q1())
  }
  
  return(as.integer(n))
}

# QUESTION 1A
#

read.q1a <- function()
{ 
  cat("Do you want to map the results from one climate model, or from an ensemble average of multiple models?")
  cat(" Warning: the ensemble average may take over an hour to calculate, depending on the speed of your computer - don't do it unless you have the time!",sep="\n\n")
  n <- readline(prompt=" Enter 1 for one model, and 2 for all the models:")
  n <- as.integer(n)
  if((!grepl("^[0-9]+$",n))|(n>2)|(n<0))
  {
    return(read.q1a())
  }
  
  return(as.integer(n))
}

# QUESTION 1B
#

read.q1b <- function()
{ 
  cat("Which weather station do you want to use?",sep="\n\n")
  for (i in 1:length(stations)) {
   cat(paste(i,' - ',stations[i]),sep='\n')
  }
  n <- readline(prompt=" Enter the number of the weather station: ")
  n <- as.integer(n)
  if((!grepl("^[0-9]+$",n))|(n>length(stations))|(n<0))
  {
    return(read.q1b())
  }
  
  return(as.integer(n))
}

# QUESTION 2
#

read.q2 <- function()
{ 
  cat("Which variable to you want to work with?",sep="\n\n")
  for (i in 1:length(vars)) {
   cat(paste(i,' - ',vars[i]),sep="\n\n")
  }
  n <- readline(prompt=" Enter the variable number:")
  n <- as.integer(n)
  if((!grepl("^[0-9]+$",n))|(n>length(vars))|(n<0))
  {
    return(read.q2())
  }
  
  return(as.integer(n))
}

# QUESTION 3
#

read.q3 <- function()
{
  cat("What type of indicator do you want to calculate?",sep="\n")
  cat("1 - an annual or seasonal average",sep="\n")
  cat("2 - the number of days above or below a threshold value ",sep="\n")
  cat("3 - an extreme or record value for each year",sep="\n")

  n <- readline(prompt=" Enter the indicator type:")
  n <- as.integer(n)
  if((!grepl("^[0-9]+$",n))|(n>3)|(n<0))
  {
    return(read.q3())
  }

  return(as.integer(n))
}

# QUESTION 4A
#

read.q4a <- function()
{
  cat("In what month does your season begin?",sep="\n\n")
  n <- readline(prompt=" Enter the first month here, using numbers from 1 (Jan) to 12 (Dec):")
  n <- as.integer(n)
  if((!grepl("^[0-9]+$",n))|(n>12)|(n<0))
  {
    return(read.q4a())
  }

  return(as.integer(n))
}

# QUESTION 4B
#

read.q4b <- function()
{
  cat("In what month does your season end?",sep="\n\n")
  n <- readline(prompt=" Enter the last month here, using numbers from 1 (Jan) to 12 (Dec):")
  n <- as.integer(n)
  if((!grepl("^[0-9]+$",n))|(n>12)|(n<0))
  {
    return(read.q4b())
  }

  return(as.integer(n))
}

# QUESTION 4C
#

read.q4c <- function()
{
  n <- readline(prompt=" Enter the threshold value in degrees C for temperature or mm for precipitation:")
  n <- as.numeric(n)
  if((n>65)|(n<(-50)))
  {
    return(read.q4d())
  }
  return(as.numeric(n))
}

# QUESTION 4D
#

read.q4d <- function()
{
  cat("Would you like to calculate the number of days per year above or below that threshold?",sep="\n\n")
  n <- readline(prompt=" Enter 1 for above, 2 for below:")
  n <- as.integer(n)
  if((!grepl("^[0-9]+$",n))|(n>2)|(n<0))
  {
    return(read.q4d())
  }

  return(as.integer(n))
}

# QUESTION 4E
#

read.q4e <- function()
{
  cat("For how many days in a row do you want to calculate an extreme? For example, if you want one day, enter 1; a week, enter 7; two weeks, enter 14.",sep="\n\n")
  n <- readline(prompt=" Enter your answer in number of days from 1 to 14:")
  n <- as.integer(n)
  if((!grepl("^[0-9]+$",n))|(n>14)|(n<0))
  {
    return(read.q4e())
  }

  return(as.integer(n))
}

# QUESTION 4F
#

read.q4f <- function()
{
  if (variable.to.use<4) cat(paste("Do you want the coldest or hottest day(s)?"),sep="\n\n")
  if (variable.to.use==4) cat(paste("Do you want the driest or wettest day(s)?"),sep="\n\n")
  if (variable.to.use<4) n <- readline(prompt=" Enter 1 for coldest, 2 for hottest:")
  if (variable.to.use==4) n <- readline(prompt=" Enter 1 for driest, 2 for wettest:")
  n <- as.integer(n)
  if((!grepl("^[0-9]+$",n))|(n>2)|(n<0))
  {
    return(read.q4f())
  }

  return(as.integer(n))
}

# QUESTION 5
#

read.q5 <- function()
{
  cat("What threshold would you like to use to calculate Cooling Degree-Days? Values typically range from 18 to 25oC.",sep="\n\n")
  n <- readline(prompt=" Enter the temperature at which people turn on their air conditioning, in degrees C:")
  n <- as.numeric(n)
  if((n>35)|(n<15))
  {
    return(read.q5())
  }

  return(as.numeric(n))
}


# TWO. Ask the input questions to determine what the user wants to do


code.dir <- read.q0c()
cat('',sep="\n\n\n")


station.names.file <- read.csv(paste(code.dir,'station.list.tmin.txt',sep=''),header=T,sep=',')
stations <- station.names.file$station_name
station.id <- station.names.file$GHCN_ID

input.dir <- read.q0a()
cat('',sep="\n\n\n")

output.dir <- read.q0b()
cat('',sep="\n\n\n")

grid.or.station <- read.q1()
cat('',sep="\n\n\n")

how.many.models <- length(mod)
if (grid.or.station==1) {
  hh <- read.q1a()
  cat('',sep="\n\n\n")
  if (hh==1) how.many.models <- 1
}

if (grid.or.station==2) station.number <- read.q1b()
cat('',sep="\n\n\n")

variable.to.use <- read.q2()
cat('',sep="\n\n\n")

if (variable.to.use!=5) indicator.type <- read.q3()
if (variable.to.use==5) {
   indicator.type <- 1 # cooling degree-days can only be a seasonal or annual average
   cdd.threshold <- read.q5()
}
cat('',sep="\n\n\n")


if (indicator.type==1) {
  begin.month <- read.q4a()
  end.month <- read.q4b()
  indicator <- paste(name.var[variable.to.use],'.average.from.',months[begin.month],'.to.',months[end.month],sep='')
} else if (indicator.type==2) {
  threshold.value <- read.q4c()
  above.or.below <- read.q4d()
cat('',sep="\n\n\n")
  if (above.or.below==1&variable.to.use<4) indicator <- paste(name.var[variable.to.use],'.days.per.year.above.',threshold.value,'.degC',sep='')
  if (above.or.below==2&variable.to.use<4) indicator <- paste(name.var[variable.to.use],'.days.per.year.below.',threshold.value,'.degC',sep='')
  if (above.or.below==1&variable.to.use==4) indicator <- paste(name.var[variable.to.use],'.days.per.year.above.',threshold.value,'.mm',sep='')
  if (above.or.below==2&variable.to.use==4) indicator <- paste(name.var[variable.to.use],'.days.per.year.below.',threshold.value,'.mm',sep='')
} else if (indicator.type==3) {
  if (grid.or.station==2) sequential.days <- read.q4e()
  if (grid.or.station==1) sequential.days <- 1
  if (variable.to.use==4) low.or.high <- 2
  if (variable.to.use!=4)  low.or.high <- read.q4f()
  if (low.or.high==1) indicator <- paste(name.var[variable.to.use],'.of.coldest.',sequential.days,'.days',sep='')
  if (low.or.high==2&variable.to.use<4) indicator <- paste(name.var[variable.to.use],'.of.hottest.',sequential.days,'.days',sep='')
  if (low.or.high==2&variable.to.use==4) indicator <- paste(name.var[variable.to.use],'.of.wettest.',sequential.days,'.days',sep='')
}

cat('',sep="\n\n\n")

# Now read the subroutines
source(paste(code.dir,'thresholds.r',sep='')) # subroutine containing threshold calculation code
source(paste(code.dir,'thresholds.grid.r',sep='')) # subroutine containing threshold calculation code
source(paste(code.dir,'seas.r',sep='')) # subroutine containing seasonal average code
source(paste(code.dir,'seas.grid.r',sep='')) # subroutine containing seasonal average code
source(paste(code.dir,'degreedays.r',sep='')) # subroutine containing seasonal average code
source(paste(code.dir,'degreedays.grid.r',sep='')) # subroutine containing seasonal average code
source(paste(code.dir,'extremes.r',sep='')) # subroutine containing seasonal average code
source(paste(code.dir,'extremes.grid.r',sep='')) # subroutine containing seasonal average code


# TWO B. Depending on if it's gridded or station data, do the calculations

if (grid.or.station==1) { # gridded data to make a map

# set up array to contain the results of the calculations, one value for each year and time period

 mod.file <- nc_open(paste(input.dir,grid.var[variable.to.use],'_day_',mod[1],'_',scen[1],'_r1i1p1_2020.nc',sep=''))
 lons <- ncvar_get(mod.file,'lon')
 lats <- ncvar_get(mod.file,'lat')
 nc_close(mod.file)

 grid.output <- array(NA,c(length(scen),how.many.models,length(lons),length(lats),(length(years)-1))) 
 final.output <- array(NA,c(length(scen),length(lons),length(lats),(length(begin.year))))

 for (ss in 1:length(scen)) {

  for (mm in 1:how.many.models) {
   cat(paste('Working on model ',mod[mm],' and scenario ',scen[ss],' - please be patient, this will take some time.',sep=''))
cat('',sep="\n\n\n")
   source(paste(code.dir,'calc.grid.r',sep=''))
 }}

} else { # read in ARRM station files

# read in observations and calculate annual values
  source(paste(code.dir,'calc.obs.r',sep=''))

# set up array to contain the results of the calculations, one value for each year and time period
 stn.output <- array(NA,c(length(scen),how.many.models,length(years))) 
 final.output1 <- array(NA,c(length(years),6))
 final.output2 <- array(NA,c(length(begin.year),6))

 for (ss in 1:length(scen)) {
  for (mm in 1:how.many.models) {
   source(paste(code.dir,'calc.stn.r',sep=''))
 }}

} 

cat("Calculations are finished! You can find the results in the output directory you chose.")
cat('',sep="\n\n\n")


options(warn = oldw)
