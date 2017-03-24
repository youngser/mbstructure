synthMB <- function(g, vdf, labK, Khat=6, dtype="truth", doplot=TRUE)
{
    suppressMessages(library(igraph))

    set.seed(12345)
    if (dtype=="truth") {
        tab <- table(vdf$type)
        Khat <- length(tab)
        ase <- embed_adjacency_matrix(g,Khat) # observed MB connectome
        Bobsvec = c( 0.36202020 , 0.44571429 , 0.49448276 , 0 , 0.38333333 , 0 , 0.11986864 , 0 , 0 , 0.09359606 , 0.20812808 , 0 , 0.07587302 , 0 , 0 , 0 )
        Bobs <- matrix(Bobsvec,ncol=Khat,byrow=T)
    } else {
        ase <- embed_adjacency_matrix(g,Khat) # observed MB connectome
        labK <- as.numeric(labK)
        tab <- table(labK)
        Bobsvec <- matrix(0,Khat,Khat);
        for (i in 1:Khat) for(j in 1:Khat) { Bobsvec[i,j] <- mean(as.matrix(g[])[labK==i,labK==j]) }
        Bobs <- matrix(Bobsvec,ncol=Khat,byrow=F)
    }

    svdBobs <- svd(Bobs) # true latent positions for SBM on Bobs
    Bobsout <- svdBobs$u %*% diag(svdBobs$d^(1/2))
    Bobsin  <- diag(svdBobs$d^(1/2)) %*% t(svdBobs$v)

    ABobs <- sample_sbm(sum(tab),Bobs,as.numeric(tab),directed=T) # n=213 SBM on Bobs (with observed block proportions)
    vec <- round(10000*rep(1/Khat, Khat))
    ABobs10000 <- sample_sbm(sum(vec),Bobs,vec,directed=T) # monster SBM on Bobs (with balanced block proportions)
    aseBobs10000 <- embed_adjacency_matrix(ABobs10000,Khat,options = list(maxiter=2000))

    if (dtype=="truth") {
        aseBobs <- embed_adjacency_matrix(ABobs,Khat)
        aseBobsnew <- Bobsoutnew <- aseBobs10K <- labK <- NULL
    } else {
        vorder <- unlist(sapply(1:Khat, function(x) which(labK==x)))
        aseBobs <- embed_adjacency_matrix(permute.vertices(ABobs,vorder),Khat)
        aseBobsnew <- aseBobs$X[,1:Khat] %*% procrustes(aseBobs$X[,1:Khat], ase$X[,1:Khat])$W
        asemean <- t(sapply(1:Khat, function(x) colMeans(ase$X[labK==x,])))
        proc1 <- procrustes(Bobsout[,1:Khat],asemean[,1:Khat])$W
        Bobsoutnew <- Bobsout %*% proc1
        labK.10K <- rep(1:Khat,times=vec)
        Bobsmean <- t(sapply(1:Khat, function(x) colMeans(aseBobs10000$X[labK.10K==x,])))
        proc2 <- procrustes(Bobsmean[,1:Khat], asemean[,1:Khat])$W
        aseBobs10K <- aseBobs10000$X[,1:Khat] %*% proc2
    }

    # Figure 7
    if (doplot) {
        suppressMessages(library(ggplot2))
        suppressMessages(library(RColorBrewer))

        df1 <- data.frame(x=-ase$X[,1],y=ase$X[,2],type=NA,neuron=vdf$type)
        df2 <- data.frame(x=-aseBobs$X[,1],y=aseBobs$X[,2],type="small",neuron="small")
        df2 <- rbind(df2, data.frame(x=aseBobs10000$X[,1],y=aseBobs10000$X[,2],type="big",neuron="big"))
        df2 <- rbind(df2, data.frame(x=Bobsout[,1],y=Bobsout[,2],type="true",neuron="true"))

        mypal <- colorRampPalette( brewer.pal( 6 , "Set2" ) )
        mycols <- mypal(8)

        p <- ggplot(df1,aes(x=x,y=y,color=neuron)) + geom_point(alpha=0.5,size=2) +
            geom_point(data=subset(df2,type=="true"),color=mycols[2],aes(shape=type),alpha=0.1,size=25) +
            geom_point(data=subset(df2,type=="true"),color=mycols[2],aes(shape=type),alpha=0.3,size=20) +
            geom_point(data=subset(df2,type=="true"),color=mycols[2],aes(shape=type),alpha=0.5,size=15) +
            geom_point(data=subset(df2,type=="true"),color=mycols[2],aes(shape=type),alpha=0.8,size=10) +
            geom_point(data=subset(df2,type=="small"),color=mycols[4],aes(shape=type),alpha=0.8,size=3) +
            geom_point(data=subset(df2,type=="big"),color=mycols[8],aes(shape=type),alpha=0.8,size=2) +
            scale_shape_manual(values=c(17,18,19),labels=c("n=10000","n=213","theoretical")) +
            xlab("out 1") + ylab("out 2") +
            guides(colour=guide_legend(title="observed",override.aes=list(size=3)),
                   shape=guide_legend(title="synthetic",override.aes=list(size=3,color=c(mycols[c(8,4,2)]))))
        print(p)
    }

    return(list(ase=ase, aseBobs10000=aseBobs10000, aseBobs=aseBobs,
                Bobsin=Bobsin, Bobsout=Bobsout, Bobs=Bobs, Bobsvec=Bobsvec,
                aseBobsnew=aseBobsnew, Bobsoutnew=Bobsoutnew, aseBobs10K=aseBobs10K, labK=labK))
}
