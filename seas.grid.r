# Calculate thresholds passed to the function for whichever variable is chosen
# x is the input (dimensioned lon x lat x 365)
# then the output (dimensioned lon x lat)
# y are the thresholds

seas.grid <- function(x,y1,y2,func){

    month <- rep(1:12, c(31,28,31,30,31,30,31,31,30,31,30,31))
    day <- c(1:365)
    if (y1<=y2) days <- day[which(month>(y1-1)&month<(y2+1))]
    if (y1>y2) {
      days <- day[which(month>(y1-1)&month<13)] # the end of the year
      days <- c(day[which(month>0&month<(y2+1))],days) # the beginning of the next year
    }
    x2 <- x[,,c(days)]

    z <- apply(x2,c(1,2),func,na.rm=T)

    z[is.infinite(z)] <- NA
    z[is.nan(z)] <- NA

    x <- z

}

