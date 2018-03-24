# Calculate thresholds passed to the function for whichever variable is chosen
# x is the input (dimensioned lon x lat x 365)
# then the output (dimensioned lon x lat)
# y are the thresholds

extremes.grid <- function(x,func,n){

    if (n==1) z <- apply(x,c(1,2),func,na.rm=T)

    if (n>1) {
    z <- array(NA,dim=c(ncol(x)))
    for (yy in 1:ncol(x)) {
     if (n==2) y <- apply(cbind(x[1:(nrow(x)-1),yy], x[2:(nrow(x)),yy] ), 1, mean,na.rm=T)
     if (n==3) y <- apply(cbind(x[1:(nrow(x)-2),yy], x[2:(nrow(x)-1),yy],x[3:(nrow(x)),yy] ), 1, mean,na.rm=T)
     if (n==4) y <- apply(cbind(x[1:(nrow(x)-3),yy], x[2:(nrow(x)-2),yy],x[3:(nrow(x)-1),yy], x[4:(nrow(x)),yy] ), 1, mean,na.rm=T)
     if (n==5) y <- apply(cbind(x[1:(nrow(x)-4),yy], x[2:(nrow(x)-3),yy], x[3:(nrow(x)-2),yy],x[4:(nrow(x)-1),yy], x[5:(nrow(x)),yy] ), 1, mean,na.rm=T)
     if (n==6) y <- apply(cbind(x[1:(nrow(x)-5),yy], x[2:(nrow(x)-4),yy], x[3:(nrow(x)-3),yy], x[4:(nrow(x)-2),yy],x[5:(nrow(x)-1),yy], x[6:(nrow(x)),yy] ), 1, mean,na.rm=T)
     if (n==7) y <- apply(cbind(x[1:(nrow(x)-6),yy], x[2:(nrow(x)-5),yy], x[3:(nrow(x)-4),yy], x[4:(nrow(x)-3),yy], x[5:(nrow(x)-2),yy],x[6:(nrow(x)-1),yy], x[7:(nrow(x)),yy] ), 1, mean,na.rm=T)
     if (n==8) y <- apply(cbind(x[1:(nrow(x)-7),yy], x[2:(nrow(x)-6),yy], x[3:(nrow(x)-5),yy], x[4:(nrow(x)-4),yy], x[5:(nrow(x)-3),yy], x[6:(nrow(x)-2),yy],x[7:(nrow(x)-1),yy], x[8:(nrow(x)),yy] ), 1, mean,na.rm=T)
     if (n==9) y <- apply(cbind(x[1:(nrow(x)-8),yy], x[2:(nrow(x)-7),yy], x[3:(nrow(x)-6),yy], x[4:(nrow(x)-5),yy], x[5:(nrow(x)-4),yy], x[6:(nrow(x)-3),yy], x[7:(nrow(x)-2),yy],x[8:(nrow(x)-1),yy], x[9:(nrow(x)),yy] ), 1, mean,na.rm=T)
     if (n==10) y <- apply(cbind(x[1:(nrow(x)-9),yy], x[2:(nrow(x)-8),yy], x[3:(nrow(x)-7),yy], x[4:(nrow(x)-6),yy], x[5:(nrow(x)-5),yy], x[6:(nrow(x)-4),yy], x[7:(nrow(x)-3),yy], x[8:(nrow(x)-2),yy],x[9:(nrow(x)-1),yy], x[10:(nrow(x)),yy] ), 1, mean,na.rm=T)
     if (n==14) y <- apply(cbind(x[1:(nrow(x)-13),yy],x[2:(nrow(x)-12),yy],x[3:(nrow(x)-11),yy],x[4:(nrow(x)-10),yy],x[5:(nrow(x)-9),yy], x[6:(nrow(x)-8),yy], x[7:(nrow(x)-7),yy], x[8:(nrow(x)-6),yy], x[9:(nrow(x)-5),yy], x[10:(nrow(x)-4),yy], x[11:(nrow(x)-3),yy], x[12:(nrow(x)-2),yy],x[13:(nrow(x)-1),yy], x[14:(nrow(x)),yy] ), 1, mean,na.rm=T)
       z[yy] <- func(y,na.rm=T)
    }}

    z[is.infinite(z)] <- NA
    z[is.nan(z)] <- NA

    x <- z
}
