# Calculate thresholds passed to the function for whichever variable is chosen
# x is the input (dimensioned lon x lat x 365)
# then the output (dimensioned lon x lat)
# y are the thresholds

thresholds.grid <- function(x,y,z){

    if (z==1) count <- function(x) length(which(x>=y & !is.na(x))) 
    if (z==2) count <- function(x) length(which(x<=y & !is.na(x))) 
    if (z==3) count <- function(x) length(which(x==y & !is.na(x))) 

    if (length(which(!is.na(x)))>1) {
      aa <- apply(x,c(1,2),count)
    }

    x <- aa
}
