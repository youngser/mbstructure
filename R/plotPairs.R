plotClustering <- function(Xhat,mc,vdf)
{
    suppressMessages(library(ggplot2))
    suppressMessages(library(RColorBrewer))

    lab3 <- mc$classification
    df3 <- data.frame(Xhat)
    gg3 <- makePairs(df3)
    ndf <- nrow(gg3$all)
    mydf3 <- data.frame(gg3$all, neuron=rep(vdf$type,length=ndf), cluster=factor(rep(lab3,length=ndf)))

    # pairs plot
    p <- ggplot(mydf3, aes_string(x = "x", y = "y")) +
        facet_grid(xvar ~ yvar, scales = "free") +
        geom_point(aes(colour=neuron, shape=cluster), na.rm = TRUE, alpha=1,size=3) +
        #  scale_shape_manual(values=1:nlevels(mydf3$cluster)) +
        scale_shape_manual(values=as.character(1:mc$G)) +
        stat_density(aes(x = x, y = ..scaled.. * diff(range(x)) + min(x)),
                     data = gg3$densities, position = "identity",
                     colour = "grey20", geom = "line") +
        theme(axis.title=element_text(size=0)) +
        theme(axis.text.x=element_text(size=0)) +
        theme(axis.ticks = element_line(size = 0)) +
        theme(axis.text.y=element_text(size=0)) +
        theme(strip.text=element_text(size=rel(1.2))) +
        theme(legend.title = element_text(colour="black", size=14, face="bold")) +
        theme(legend.text = element_text(colour="black", size = 12, face = "plain"))

    print(p)

#    kable(table(type=vdf$type,Khat6=mc$class))

    mydf4 <- subset(mydf3,select=c("X1","X2"))
    mydf4$neuron <- vdf$type
    mydf4$cluster <- factor(lab3)
    mydf4 <- subset(mydf4, neuron=="KC")
    mydf4$claw <- vdf$claw[!is.na(vdf$claw)]
    p <- ggplot(mydf4, aes_string(x="`X1`",y="`X2`")) +
        geom_point(aes(color=cluster,size=claw),alpha=.7) +
        scale_shape_manual(values=1:nlevels(mydf4$cluster)) +
        xlab("out 1") + ylab("out 2")

    mycols <- gg_color_hue(4)
    p2 <- p + geom_point(data=subset(mydf4, cluster==5), color=mycols[4], alpha=.7)
    print(p2)

}
