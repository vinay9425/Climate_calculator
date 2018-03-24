# Subroutine to calculate indicators for gridded data

# ONE. Read in the data

   for (i in 1:150) { # from 1950 to 2099
    if (((i-1)%%10)==0) cat(paste('Working on the ',1949+i,'s',sep=''))
    if (((i-1)%%10)==0) cat('',sep='\n\n')

# Open each annual file and extract variables needed for that year
   if (variable.to.use<3|variable.to.use==4) {

    if ((1949+i)<2006) { scen.in <- 'historical'
    } else { scen.in <- scen[ss] }

    mod.file <- nc_open(paste(input.dir,grid.var[variable.to.use],'_day_',mod,'_',scen.in,'_r1i1p1_',(i+1949),'.nc',sep=''))
    temp <- ncvar_get(mod.file,grid.var[variable.to.use])
    nc_close(mod.file)
   } else { # for Tavg or cooling degree-days
    mod.file <- nc_open(paste(input.dir,'tasmax_day_',mod,'_',scen.in,'_r1i1p1_',(i+1949),'.nc',sep=''))
    temp1 <- ncvar_get(mod.file1,grid.var[1])
    nc_close(mod.file)
    mod.file <- nc_open(paste(input.dir,'tasmin_day_',mod,'_',scen.in,'_r1i1p1_',(i+1949),'.nc',sep=''))
    temp2 <- ncvar_get(mod.file2,grid.var[2])
    nc_close(mod.file)
    temp <- (temp1+temp2)/2
   }
   if (variable.to.use!=4) temp <- temp-273.15 # convert to degrees C
   if (variable.to.use==4) temp <- temp*86400 # convert to mm

# TWO. Calculate the indicator the user has selected, using the variable temp

    if (indicator.type==1) { # annual or seasonal average

      if (variable.to.use<4) test <- seas.grid(temp,begin.month,end.month,mean) 
      if (variable.to.use==4) test <- seas.grid(temp,begin.month,end.month,sum) 
      if (variable.to.use==5) test <- degreedays.grid(temp,cdd.threshold,1) 

    } else if (indicator.type==2) { # threshold.value and above.or.below

      test <- thresholds.grid(temp,threshold.value,above.or.below) 

    } else { # single day extreme value: low.or.high

      if (low.or.high==1) test <- extremes.grid(temp,min,1)
      if (low.or.high==2) test <- extremes.grid(temp,max,1)

    }
    grid.output[ss,mm,,,i] <- test
   } # end the year loop

# If it's the last model, then average and write out
  if (mm==how.many.models) {

# THREE. Calculate the multi-model average and the time periods
# starting with a variable dimensioned [models, lon, lat, year]
  
  temp1 <- apply(grid.output,c(1,3,4,5),mean,na.rm=T) # averages across all climate models
  
  for (j in 1:length(begin.year)) {
   temp2 <- temp1[ss,,,begin.year[j]:end.year[j]] # now temp2 is lon, lat, years
   final.output[ss,,,j] <- apply(temp2,c(1,2),mean,na.rm=T) # keep only lon and lat
  }

# FOUR. Output the results to a netCDF file to read into Panoply
  if (ss==length(scen)) {
   cat('',sep="\n\n\n")
   cat('Writing the output file now - almost done.')
   cat('',sep="\n\n\n")

# change grid.output to a different variable with different time dimensions
  outfile <- paste(output.dir,'india.map.',indicator,'.nc',sep='')

  x <- ncdim_def("lon","degrees_east",lons)
  y <- ncdim_def("lat","degrees_north",lats)
  years2 <- as.integer(begin.year+1949)
  t <- ncdim_def("time","beginning_of_30y_climatological_period",years2,unlim=T)
  
  if (variable.to.use<4) units <- c('degC')
  if (variable.to.use==4) units <- c('mm')
  if (variable.to.use==5) units <- c('cumulative cooling hours')

  var1 <- ncvar_def(paste(scen[1],indicator,sep='.'),units,list(x,y,t),1.e30)
  var2 <- ncvar_def(paste(scen[2],indicator,sep='.'),units,list(x,y,t),1.e30)

  nc <- nc_create(outfile,list(var1,var2))

  ncvar_put( nc, var1, final.output[1,,,] ) # no start or count: write all values
  ncvar_put( nc, var2, final.output[2,,,] ) # no start or count: write all values

  nc_close(nc)

}} # end if statements
