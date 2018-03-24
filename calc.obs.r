# Subroutine to calculate indicators for observed (real world) station data

# ONE. Read in the data
  
# Open the station file and extract variables needed for that year

   if (variable.to.use<3 | variable.to.use==4) { # for Tmax or Tmin or Pr
    temp <- read.table(paste(input.dir,station.id[station.number],'.',grid.var[variable.to.use],'.1950.2012.csv',sep=''),header=T,sep=',')
   } else { # for Tavg or cooling degree-days
    temp1 <- read.table(paste(input.dir,station.id[station.number],'.tasmax.1950.2012.csv',sep=''),header=T,sep=',')
    temp2 <- read.table(paste(input.dir,station.id[station.number],'.tasmin.1950.2012.csv',sep=''),header=T,sep=',')
    temp <- temp1
    temp[,4] <- (temp1[,4]+temp2[,4])/2
   }

#   temp <- remove_leap_days(temp,min(temp[,1]),max(temp[,1])) # the data has already had Feb 29 removed
   if (variable.to.use==4) temp[,4] <- temp[,4]/10
   temp.obs <- matrix(temp[,4],nrow=365) # turn it into a matrix for easier calculation

   na.years <- apply(temp.obs,2,function(x) sum(is.na(x)))

# THREE. Calculate the indicator the user has selected

    if (indicator.type==1) { # annual or seasonal average

      if (variable.to.use<4) test <- seas(temp.obs,begin.month,end.month,mean) 
      if (variable.to.use==4) test <- seas(temp.obs,begin.month,end.month,sum) 
      if (variable.to.use==5) test <- degreedays(temp.obs,cdd.threshold,1) #CDD threshold 25oC

    } else if (indicator.type==2) { # threshold.value and above.or.below

      test <- thresholds(temp.obs,threshold.value,above.or.below) 

    } else { # single day extreme value: low.or.high

      if (low.or.high==1) test <- extremes(temp.obs,min,1)
      if (low.or.high==2) test <- extremes(temp.obs,max,1)

    }

    obs.output <- test


# FOUR. If there are less than (365-125) days of data for that year, set the calculated value to NA

  obs.output[which(na.years>125)] <- NA
  obs.output[which(is.infinite(obs.output))] <- NA
  obs.output[which(is.nan(obs.output))] <- NA
  obs.output[which(obs.output>1000000)] <- NA
  obs.output <- round(obs.output,2)

# Add NAs to the end to make the time series the same length as the downscaled
  add.na <- rep(NA,(151-length(obs.output)))
  obs.output <- c(obs.output, add.na)
