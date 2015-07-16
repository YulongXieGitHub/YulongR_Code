plotChains = function( nodename , saveplots=F , filenameroot="DeleteMe", path.pdf = "none") {
    summarytable = samplesStats(nodename)
    show( summarytable )
    nCompon = NROW(summarytable)
    nPlotPerRow = 5
    nPlotRow = ceiling(nCompon/nPlotPerRow)
    nPlotCol = ceiling(nCompon/nPlotRow)
    windows(3.75*nPlotCol,3.5*nPlotRow)
    par( mar=c(4,4,3,1) , mgp=c(2,0.7,0) )
    samplesHistory( nodename , ask=F , mfrow=c(nPlotRow,nPlotCol) ,
                    cex.lab=1.5 , cex.main=1.5 )
    if ( saveplots ) {
       dev.copy2pdf( file=paste(path.pdf,paste( filenameroot ,"_", toupper(nodename) ,
                                 "history.pdf" , sep="" ),sep="/")) }
    windows(3.75*nPlotCol,3.5*nPlotRow)
    par( mar=c(4,4,3,1) , mgp=c(2,0.7,0) )
    samplesAutoC( nodename , chain=1 , ask=F , mfrow=c(nPlotRow,nPlotCol) ,
                  cex.lab=1.5 , cex.main=1.5 )
    if ( saveplots ) {
       dev.copy2pdf( file=paste(path.pdf,paste( filenameroot ,"_", toupper(nodename) ,
                     "autocorr.pdf" , sep="" ),sep="/")) }
    windows(3.75*nPlotCol,3.5*nPlotRow)
    par( mar=c(4,4,3,1) , mgp=c(2,0.7,0) )
    samplesBgr( nodename , ask=F , mfrow=c(nPlotRow,nPlotCol) ,
                cex.lab=1.5 , cex.main=1.5 )
    if ( saveplots ) {
       dev.copy2pdf( file=paste(path.pdf,paste( filenameroot , "_",toupper(nodename) ,
                     "bgr.pdf" , sep="" ),sep="/")) }
    return( summarytable )
}
