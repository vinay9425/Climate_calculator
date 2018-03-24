# Calculate thresholds passed to the function for whichever variable is chosen
# x is the input (dimensioned 365x151) 
# then the output (dimensioned 151)
# y are the thresholds

degreedays <- function(x,y,z){

    if (z==1) x[which(is.na(x))] <- -1000
    if (z==2) x[which(is.na(x))] <- 1000
    if (z==1) x <- x - y
    if (z==2) x <- y - x
    x[which(x<0)] <- 0
    x <- apply(x,2,sum,na.rm=T)

}

