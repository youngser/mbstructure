syntheticValidation <- function(g, Xhat, vdf)
{
    suppressMessages(library(mvtnorm))
    suppressMessages(library(ggplot2))
    suppressMessages(library(RColorBrewer))

    KC <- vdf$type=="KC"
    Khat <- length(table(vdf$type))
    sout4 <- synthMB(g, Xhat, vdf, labK=vdf$type, Khat=Khat, dtype="real", doplot=FALSE)
    ase4 <- sout4$ase$X[KC,]
    aseBobs4 <- sout4$aseBobs$X[KC,]

    Khat <- max(as.numeric(vdf$cluster))
    sout6 <- synthMB(g, Xhat, vdf, labK=vdf$cluster, Khat=Khat, dtype="synth", doplot=FALSE)
    ase6 <- ase4 #$X[KC,]
    aseBobs6 <- sout6$aseBobsnew[KC,] # procrusted

    Xhat4 <- Xhat[KC,]
    out8 <- semiparYQ(Xhat4, K=8, verbose=FALSE)
    means <- out8$means
    vars <- out8$vars
    pis <- out8$pis
    (K <- nrow(means))
    nk <- sum(KC)
    lab8 <- rep(0,nk)
    for (i in 1:nk) {
        tmp <- rep(0,K)
        for (k in 1:K) {
            tmp[k] <- pis[k]*dmvnorm(Xhat4[i,], means[k,], vars[,,k], log=FALSE)
        }
        lab8[i] <- which.max(tmp)
    }
    tlab <- as.numeric(vdf$type)
    lab8 <- c(lab8, tlab[tlab>1]+(K-1))
    Khat <- max(lab8)
    sout8 <- synthMB(g, Xhat, vdf, labK=lab8, Khat=Khat, dtype="synth", doplot=FALSE)

    ase8 <- ase4 #$X[KC,]
    aseBobs8 <- sout8$aseBobs$X[KC,]

    df1 <- data.frame(x=-ase4[,1],y=ase4[,2],type=NA,dat="K==4",neuron="KC")
    df1 <- rbind(df1, data.frame(x=-ase6[,1],y=ase6[,2],type=NA,dat="hat(K)==6",neuron="KC"))
    df1 <- rbind(df1, data.frame(x=-ase8[,1],y=ase8[,2],type=NA,dat="semipar",neuron="KC"))

    df2 <- data.frame(x=-aseBobs4[,1],y=-aseBobs4[,2],type="small",dat="K==4",neuron="small")
    df2 <- rbind(df2, data.frame(x=aseBobs6[,1],y=-aseBobs6[,2],type="small",dat="hat(K)==6",neuron="small"))
    df2 <- rbind(df2, data.frame(x=aseBobs8[,1],y=-aseBobs8[,2],type="small",dat="semipar",neuron="small"))

    mypal <- colorRampPalette( brewer.pal( 6 , "Set2" ) )
    mycols <- mypal(8)

    # Figure 14
    p <- ggplot(df1,aes(x=x,y=y,color=neuron)) + geom_point(alpha=0.5,size=2) +
            geom_point(data=df2,color=mycols[4],aes(shape=type),alpha=0.8,size=3) +
            scale_shape_manual(values=c(18),labels=c("n=213")) +
            facet_grid(~dat, labeller = label_parsed) +
            xlab("out 1") + ylab("out 2") + theme(legend.position="none")
    print(p)
}
