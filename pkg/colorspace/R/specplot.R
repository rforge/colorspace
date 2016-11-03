
# Plotting spectrum
specplot <- function(x, cex=1, plot=TRUE) {

   # Replace NA x with white, required for hex2RGB.
   # Store indizes of NA x to x.na for further
   # processing.
   x.na <- which(is.na(x))
   if ( length(x.na) > 0 ) x[x.na] <- "#ffffff"
   RGB <- hex2RGB(x)
   HCL <- as(RGB, "polarLUV")

   # Replace coordinates of NA x with NA
   RGB <- coords(RGB)
   HCL <- coords(HCL)
   if ( length(x.na) > 0 ) {
      for ( i in 1:3 ) HCL[x.na,i] <- NA
      for ( i in 1:3 ) RGB[x.na,i] <- NA
   }

   # Fixing H-paths: as(RGB,"polarLUV") returns hue's in the
   # range of 0-360. A palette from -100 to +100 results in
   # c(260-360,0,100) - the iterative approach corrects this.
   for ( i in 2:nrow(HCL) ) {
      d <- HCL[i,"H"]-HCL[i-1,"H"]
      if ( d > 320 ) { HCL[i,"H"] <- HCL[i,"H"] - 360 } else if ( d < -320 ) { HCL[i,"H"] <- HCL[i,"H"] + 360 }
      if ( HCL[i,"H"] >  360 ) HCL[1:i,"H"] <- HCL[1:i,"H"]-360
      if ( HCL[i,"H"] < -360 ) HCL[1:i,"H"] <- HCL[1:i,"H"]+360
   }

   # Create plot
   if ( plot ) {

      pl <- layout(matrix(1:3,ncol=1,nrow=3),heights=c(10,2,10))
      hold <- list('xaxt'=par()$xaxt,'yaxt'=par()$yaxt)
      
      # Start plotting
      par(xaxt="n",yaxs="i",xaxs="i",
          mar=c(.2,0,.2,0), oma=c(2,3,2,3),cex=cex)
      # RGB
      plot(0,type="n",ylim=c(0,1),xlim=c(1,length(x)))
         lines(RGB[,"R"],     lwd=2*cex,  col=2)
         lines(RGB[,"G"],     lwd=2*cex,  col=3)
         lines(RGB[,"B"],     lwd=2*cex,  col=4)
         legend("topleft",ncol=3,bty="n",fill=2:4,legend=c("red","green","blue"))
      mtext(side=3,"RGB Spectrum",    cex=cex, line=0.2)
      mtext(side=2,"all coordinates", cex=cex, line=2.0)

      par(xaxt='n',yaxt='n')
      image(matrix(1:length(x),ncol=1),col=x)
      par(yaxt='s')

      # blending Hue values where Chroma is very low
      idx <- which(HCL[,"C"] < 8)
      if ( length(idx) > 0 ) { HCL[idx,"H"] <- 0 }
      # HCL
      plot(0,type="n",ylim=c(0,100),xlim=c(1,length(x)))
         cols <- rainbow_hcl(3)
         ## For testing only
         lines((HCL[,"H"]+360)/7.2, lwd=2*cex, col=cols[1L])
         lines(HCL[,"C"],           lwd=2*cex, col=cols[2L])
         lines(HCL[,"L"],           lwd=2*cex, col=cols[3L])
         labels <- seq(-360,360,length.out=5)
         axis(side=4,at=labels/7.2+50,labels=labels)
         legend("bottomleft",ncol=3,bty="n",fill=cols,legend=c("hue","chroma","luminance"))
      mtext(side=1,"HCL Spectrum",         cex=cex, line=0.2)
      mtext(side=2,"chroma and luminance", cex=cex, line=2.0)
      mtext(side=4,"hue",                  cex=cex, line=2.0)

      for ( p in names(hold) ) par(p=hold[p])
   }

   # Return
   invisible( list("RGB"=RGB,"HCL"=HCL,"hex"=x) ) 
}

