plotBIC <- function(mc)
{
    vvv <- mc$BIC[,"VVV"]
    vvv <- vvv[!is.na(vvv)]

    op <- par()
    plot(2:length(vvv),vvv[-1],type="b",pch=0, col=2,ylab="BIC(VVV)",xlab="Number of components")
    points(which.max(vvv),max(vvv),pch=15,cex=1.5,col=2)

    # calculate position of inset
    plotdim <- par("plt") # c(x1,x2,y1,y2)
    xleft   <- (plotdim[1]+plotdim[2])/2 - (plotdim[2]-plotdim[1])*0.25 + 0.07
    xright  <- (plotdim[1]+plotdim[2])/2 + (plotdim[2]-plotdim[1])*0.25 + 0.03
    ybottom <- plotdim[3] + (plotdim[4] - plotdim[3]) * 0.1
    ytop    <- plotdim[3] + (plotdim[4] - plotdim[3]) * 0.6  #

    # set position for inset
    par(
        fig = c(xleft, xright, ybottom, ytop)
        , mar=c(0,0,0,0)
        , new=TRUE
    )

    # add inset
    plot(mc,what="BIC",legendArgs=list(cex=0.6,ncol=3),cex.axis=0.5,cex.lab=0.5) # inset bottommiddle
}
