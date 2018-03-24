# Calculate thresholds passed to the function for whichever variable is chosen
# x is the input (dimensioned lon x lat x 365)
# y are the thresholds

degreedays.grid <- function(x,y,z){

    if (z==1) x[which(is.na(x))] <- -1000
    if (z==2) x[which(is.na(x))] <- 1000
    if (z==1) x <- x - y
    if (z==2) x <- y - x
    x[which(x<0)] <- 0
    x <- apply(x,c(1,2),sum,na.rm=T)

}

