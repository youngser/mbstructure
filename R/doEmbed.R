doEmbed <- function(g, dmax, plotElbow=TRUE)
{
    suppressMessages(library(igraph))

    ase <- embed_adjacency_matrix(g,dmax)
    (elb.dir.u <- getElbows(ase$D,plot=FALSE))

    if (plotElbow) {
        plot(ase$D[1:20],type="b",xlab=expression("index " * italic(i)),ylab=expression("singlular value " * sigma[italic(i)]))
        points(elb.dir.u[2],ase$D[elb.dir.u[2]],col=1,pch=19,cex=1.5)
    }

    Xhat1 <- as.matrix(ase$X[,1:elb.dir.u[2]]) # "in"
    Xhat2 <- as.matrix(ase$Y[,1:elb.dir.u[2]]) # "out"
    Xhat3 <- cbind(Xhat1,Xhat2)

    return(Xhat3)
}
