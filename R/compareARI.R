compareARI <- function(g.uw, g.w, vdf)
{
    suppressMessages(library(Matrix))
    suppressMessages(library(igraph))
    suppressMessages(library(mclust))
    suppressMessages(library(reshape2))
    suppressMessages(library(ggplot2))

    dmax2 <- 20
    plotmax <- 8

    # unweighted & directed
    ase.uw <- embed_adjacency_matrix(g.uw,dmax2)

    # weighted & directed
    ase.w <- inverse.rdpg(g.w[],dmax2)

    # weighted & undirected
    A2 <- (g.w[] + Matrix::t(g.w[])) / 2
    ase2.w <- inverse.rdpg(A2, dmax2)
    # unweighted & undirected
    A2 <- (g.uw[] + Matrix::t(g.uw[])) / 2
    ase2.uw <- inverse.rdpg(A2, dmax2)

    ari.uw <- ari.w <- data.frame(dim=1:dmax2,sym=rep(0,dmax2),out=rep(0,dmax2),ind=rep(0,dmax2),joint=rep(0,dmax2))
    for (i in 1:dmax2) {
        # ari.uw
        Xhat.sym <- ase2.uw$X[,1:i,drop=FALSE]
        Xhat.out <- ase.uw$X[,1:i,drop=FALSE]
        Xhat.in <- ase.uw$Y[,1:i,drop=FALSE]
        Xhat.inout <- cbind(Xhat.out,Xhat.in)
        mc.sym <- Mclust(Xhat.sym)
        mc.out <- Mclust(Xhat.out)
        mc.in <- Mclust(Xhat.in)
        mc.inout <- Mclust(Xhat.inout)
        tmp1 <- adjustedRandIndex(vdf$type, mc.sym$class)
        tmp2 <- adjustedRandIndex(vdf$type, mc.out$class)
        tmp3 <- adjustedRandIndex(vdf$type, mc.in$class)
        tmp4 <- adjustedRandIndex(vdf$type, mc.inout$class)
        ari.uw[i,] <- c(i,tmp1,tmp2,tmp3,tmp4)
        # ari.w
        Xhat.sym <- ase2.w$X[,1:i,drop=FALSE]
        Xhat.out <- ase.w$Y[,1:i,drop=FALSE] # note X&Y switch!
        Xhat.in <- ase.w$X[,1:i,drop=FALSE]
        Xhat.inout <- cbind(Xhat.out,Xhat.in)
        mc.sym <- Mclust(Xhat.sym)
        mc.out <- Mclust(Xhat.out)
        mc.in <- Mclust(Xhat.in)
        mc.inout <- Mclust(Xhat.inout)
        tmp1 <- adjustedRandIndex(vdf$type, mc.sym$class)
        tmp2 <- adjustedRandIndex(vdf$type, mc.out$class)
        tmp3 <- adjustedRandIndex(vdf$type, mc.in$class)
        tmp4 <- adjustedRandIndex(vdf$type, mc.inout$class)
        ari.w[i,] <- c(i,tmp1,tmp2,tmp3,tmp4)
    }

    ari.uw <- ari.uw[1:(plotmax*2),]
    names(ari.uw)[3] <- c("out")
    usejoint <- ari.uw$joint[1:plotmax]
    ari.uw$joint[seq(1,plotmax*2,2)][1:plotmax] <- NA
    ari.uw$joint[seq(2,plotmax*2,2)][1:plotmax] <- usejoint
    ari.uw$out[(plotmax+1):(plotmax*2)] <- NA
    ari.uw$ind[(plotmax+1):(plotmax*2)] <- NA
    names(ari.uw)[4] <- "in"

    ari.w <- ari.w[1:(plotmax*2),]
    names(ari.w)[3] <- c("out")
    usejoint <- ari.w$joint[1:plotmax]
    ari.w$joint[seq(1,plotmax*2,2)][1:plotmax] <- NA
    ari.w$joint[seq(2,plotmax*2,2)][1:plotmax] <- usejoint
    ari.w$out[(plotmax+1):(plotmax*2)] <- NA
    ari.w$ind[(plotmax+1):(plotmax*2)] <- NA
    names(ari.w)[4] <- "in"


    ari.all <- rbind(ari.uw, ari.w)
    ari.all$graph <- as.factor(rep(c("unweighted","weighted"),each=plotmax*2))

    df.all <- melt(ari.all,c("dim","graph"))
    df.all <- na.omit(df.all)
    df.all <- subset(df.all, subset=dim<=8)
    names(df.all) <- c("dim","graph","method","ari")
    p <- ggplot(data=df.all, aes(x=dim, y=ari, group=method, color=method, shape=method)) +
        facet_grid(.~graph) +
        geom_line() +
        geom_point(size=2) + ylab("ARI") + xlab("dimension")
    print(p)
}
