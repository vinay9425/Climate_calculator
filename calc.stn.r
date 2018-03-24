# Subroutine to calculate indicators for station data

# ONE. Read in the data
  
# Open the station file and extract variables needed for that year

   if (variable.to.use<3) { # for Tmax or Tmin
    mod.file <- nc_open(paste(input.dir,'downscaled.',mod[mm],'.',scen[ss],'.',ens[mm],'.',grid.var[variable.to.use],'.',station.id[station.number],'.nc',sep=''))
    temp <- ncvar_get(mod.file,grid.var[variable.to.use])
    nc_close(mod.file)
   } else if (variable.to.use==4) { # for precipitation
     mod.file <- read.table(paste(input.dir,station.id[station.number],'.',mod[mm],'.',ens[mm],'.',scen[ss],'.',grid.var[variable.to.use],'.1950.2100.txt',sep=''),header=T,sep=' ')
     temp <- mod.file[,4]
   } else { # for Tavg or cooling degree-days
    mod.file <- nc_open(paste(input.dir,'downscaled.',mod[mm],'.',scen[ss],'.',ens[mm],'.tasmax.',station.id[station.number],'.nc',sep=''))
    temp1 <- ncvar_get(mod.file,grid.var[1])
    nc_close(mod.file)
    mod.file <- nc_open(paste(input.dir,'downscaled.',mod[mm],'.',scen[ss],'.',ens[mm],'.tasmin.',station.id[station.number],'.nc',sep=''))
    temp2 <- ncvar_get(mod.file,grid.var[2])
    nc_close(mod.file)
    temp <- (temp1+temp2)/2
   }

   temp.stn <- matrix(temp,nrow=365) # turn it into a matrix for easier calculation

# TWO. Calculate the indicator the user has selected

    if (indicator.type==1) { # annual or seasonal average

      if (variable.to.use<4) test <- seas(temp.stn,begin.month,end.month,mean) 
      if (variable.to.use==4) test <- seas(temp.stn,begin.month,end.month,sum) 
      if (variable.to.use==5) test <- degreedays(temp.stn,cdd.threshold,1) #CDD threshold 25oC

    } else if (indicator.type==2) { # threshold.value and above.or.below

      test <- thresholds(temp.stn,threshold.value,above.or.below) 

    } else { # single day extreme value: low.or.high

      if (low.or.high==1) test <- extremes(temp.stn,min,1)
      if (low.or.high==2) test <- extremes(temp.stn,max,1)

    }

    stn.output[ss,mm,] <- test

# If it's the last model, then average and write out
  if (mm==length(mod) & ss==length(scen)) {

# THREE. Calculate the multi-model average, high and low, and the time periods

# First, the multi-model average, high and low values
  for (s2 in 1:length(scen)) {
    input <- stn.output[s2,,]
    if (s2==1) index <- c(1,2,3)
    if (s2==2) index <- c(4,5,6)
    final.output1[,index[1]] <- apply(input,c(2),min,na.rm=T)
    final.output1[,index[2]] <- apply(input,c(2),mean,na.rm=T)
    final.output1[,index[3]] <- apply(input,c(2),max,na.rm=T)
  }

# Next, the time period averages
  for (s2 in 1:length(scen)) {
   for (j in 1:length(begin.year)) {
    input <- stn.output[s2,,begin.year[j]:end.year[j]]
    input <- apply(input,1,mean,na.rm=T) # now I just have a single value for each model
    if (s2==1) index <- c(1,2,3)
    if (s2==2) index <- c(4,5,6)
    final.output2[j,index[1]] <- min(input,na.rm=T)
    final.output2[j,index[2]] <- mean(input,na.rm=T)
    final.output2[j,index[3]] <- max(input,na.rm=T)
  }}

# FOUR. Output the results to a CSV file to read into Excel

# Fix any NaN or Inf values
  final.output1[which(is.infinite(final.output1))] <- NA
  final.output1[which(is.nan(final.output1))] <- NA
  final.output1[which(final.output1>1000000)] <- NA
  final.output1 <- round(final.output1,2)

  final.output2[which(is.infinite(final.output2))] <- NA
  final.output2[which(is.nan(final.output2))] <- NA
  final.output2[which(final.output2>1000000)] <- NA
  final.output2 <- round(final.output2,2)

# write out variables, one variable per file
   outfile1 <- paste(output.dir,indicator,'.',stations[station.number],'.timeseries.csv',sep='')
   outfile2 <- paste(output.dir,indicator,'.',stations[station.number],'.barchart.csv',sep='')

   c1 <- c('Year','Obs','Min.Lower.Scenario','Mean.Lower.Scenario','Max.Lower.Scenario','Min.Higher.Scenario','Mean.Higher.Scenario','Max.Higher.Scenario')
   c2 <- c('Year','Min.Lower.Scenario','Mean.Lower.Scenario','Max.Lower.Scenario','Min.Higher.Scenario','Mean.Higher.Scenario','Max.Higher.Scenario')

   write.out <- cbind(years,obs.output,final.output1)
   write.table(write.out,file=outfile1,quote=F,append=F,col.names=c1,row.names=F,sep=',')

   years2 <- paste((begin.year+1949),(end.year+1949),sep='-')
   write.out <- cbind(years2,final.output2)
   write.table(write.out,file=outfile2,quote=F,append=F,col.names=c2,row.names=F,sep=',')

} # end of if statement

