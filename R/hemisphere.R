right.vs.left <- function(Xhat, vdf, out100)
{
    suppressMessages(library(ggplot2))
    suppressMessages(library(mclust))
    suppressMessages(library(RColorBrewer))
    suppressMessages(library(packcircles))
    suppressMessages(library(mvtnorm))

    # Figure 9
    dd <- data.frame(x=Xhat[,1], y=Xhat[,2], vdf)
#    dd <- dd[,c("x","y","cluster","type","age")]
    mytab <- with(dd,table(type,cluster))
    type2 <- factor(rownames(mytab)[apply(mytab,2,which.max)])
    dd$type2 <- type2[vdf$cluster]
    Khat <- max(as.numeric(dd$cluster))
    dd3 <- subset(dd, type2!="KC")    # ellipse
    vars <- t(sapply(1:dim(out100$vars)[3],function(x) sqrt(diag(out100$vars[,,x]))))
    dd4 <- data.frame(x=out100$means[,1],y=out100$means[,2],sdx=vars[,1],sdy=vars[,2])

    ## model: right
    p3 <- ggplot(dd,aes(x=x,y=y)) +
        geom_point(aes(color=type,shape=type,fill=type),size=0,alpha=0.9) +
        scale_shape_manual(values=rep("R",4),guide=FALSE) +
        stat_ellipse(data=dd3,geom="polygon",aes(fill=type2,color=type2), alpha=.2, show.legend=FALSE) +
        geom_line(data=dd4,color="darkred",size=1.5) +
        geom_ribbon(data=dd4,aes(ymin=y-1.96*sdx,ymax=y+1.96*sdy),alpha=0.2,fill="darkred") +
        xlab("out 1") + ylab("out 2")


    ## point: left
    out.left <- generate.graph(newldat, vdf.left)
    g.left <- out.left$g
    vdf.left <- out.left$vdf
    Xhat.left <- doEmbed(g.left, dmax)
    mc.left <- Mclust(Xhat.left, 2:10)
    mytab.left <- table(vdf.left$type,mc.left$class)
    type2.left <- factor(rownames(mytab.left)[apply(mytab.left,2,which.max)])
    type2.left <- type2.left[mc.left$class]

    ## find the best sign combinations == minimizing nonpar stat
    stat <- matrix(0,2,2)
    ind <- 1
    for (i in 1:2) {
        for (j in 1:2) {
            stat[i,j] <- kernel.stat(Xhat.left[,1:2] %*% diag(c((-1)^i,(-1)^j)), Xhat[,1:2])
        }
    }
    min.stat <- which(stat==min(stat), arr.ind=TRUE)
    Xhat.left <- Xhat.left[,1:2] %*% diag(c((-1)^min.stat[1], (-1)^min.stat[2]))
    dd.left <- data.frame(x=Xhat.left[,1],y=Xhat.left[,2],type=vdf.left$type, type2=type2.left)

    p4 <- p3 + geom_point(data=dd.left, aes(x=x,y=y,color=type),alpha=0.7)
    #p4

    ## right & left
#    dd3 <- dd3[,-7]
    dd.right <- dd[,c("x","y","type","type2")]
    dd.right$side <- "right hemisphere MB"
    dd.left$side <- "left hemisphere MB"
    dd.all <- rbind(dd.right,dd.left)

    type <- names(table(dd.all$type))[-1]
    means <- t(sapply(type, function(x) colMeans(dd.all[dd.all$type2==x,1:2])))
    means <- data.frame(x=means[,1],y=means[,2],col=factor(type))

    layout <- data.frame(id=1:nrow(dd4), x=dd4$x, y=dd4$y, r=1.96*dd4$sdx)
    cir <- circlePlotData(layout, id.col=1, xyr.cols=2:4)

    p5 <- ggplot(dd.all,aes(x=x,y=y)) +
        #p3 <- ggplot(dd,aes(x=x,y=y)) +
        geom_polygon(data=cir, aes(x, y, group=id), fill="rosybrown1", colour="rosybrown1", alpha=1) +
        geom_point(aes(color=type,fill=type),size=2,alpha=0.5) +
        facet_wrap(~side) +
        stat_ellipse(data=dd3,geom="polygon",aes(fill=type2,color=type2), alpha=.2, show.legend=FALSE) +
        geom_line(data=dd4,color="darkred",size=1.5) +
        geom_point(data=means, aes(x=x,y=y,color=col),size=5,show.legend=FALSE) +
        #    geom_ribbon(data=dd4,aes(ymin=y-1.96*sdx,ymax=y+1.96*sdy),alpha=0.2,fill="darkred") +
        xlab("out 1") + ylab("out 2")
    print(p5)
}
