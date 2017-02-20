synthMB <- function(gomni2, vdf)
{
    suppressMessages(library(igraph))
    suppressMessages(library(ggplot2))
    suppressMessages(library(RColorBrewer))


    Bobsvec = c( 0.36202020 , 0.44571429 , 0.49448276 , 0 , 0.38333333 , 0 , 0.11986864 , 0 , 0 , 0.09359606 , 0.20812808 , 0 , 0.07587302 , 0 , 0 , 0 )
    Bobs <- matrix(Bobsvec,ncol=4,byrow=T)

    set.seed(12345)
    ase <- embed_adjacency_matrix(gomni2,4) # observed MB connectome

    svdBobs <- svd(Bobs) # true latent positions for SBM on Bobs
    Bobsout <- svdBobs$u %*% diag(svdBobs$d^(1/2))
    Bobsin  <- diag(svdBobs$d^(1/2)) %*% t(svdBobs$v)

    ABobs <- sample_sbm(213,Bobs,c(100,21,29,63),directed=T) # n=213 SBM on Bobs (with observed block proportions)
    aseBobs <- embed_adjacency_matrix(ABobs,4)

    ABobs10000 <- sample_sbm(10000,Bobs,10000*c(1/4,1/4,1/4,1/4),directed=T) # monster SBM on Bobs (with balanced block proportions)
    aseBobs10000 <- embed_adjacency_matrix(ABobs10000,4)

    # Figure 7
    df1 <- data.frame(x=-ase$X[,1],y=ase$X[,2],type=NA,neuron=vdf$type)
    df2 <- data.frame(x=-aseBobs$X[,1],y=aseBobs$X[,2],type="small",neuron="small")
    df2 <- rbind(df2, data.frame(x=aseBobs10000$X[,1],y=aseBobs10000$X[,2],type="big",neuron="big"))
    df2 <- rbind(df2, data.frame(x=Bobsout[,1],y=Bobsout[,2],type="true",neuron="true"))

    mypal <- colorRampPalette( brewer.pal( 6 , "Set2" ) )
    mycols <- mypal(8)

    p <- ggplot(df1,aes(x=x,y=y,color=neuron)) + geom_point(alpha=0.5,size=2)
    p2 <- p + geom_point(data=subset(df2,type=="true"),color=mycols[2],aes(shape=type),alpha=0.1,size=25)
    p2 <- p2 + geom_point(data=subset(df2,type=="true"),color=mycols[2],aes(shape=type),alpha=0.3,size=20)
    p2 <- p2 + geom_point(data=subset(df2,type=="true"),color=mycols[2],aes(shape=type),alpha=0.5,size=15)
    p2 <- p2 + geom_point(data=subset(df2,type=="true"),color=mycols[2],aes(shape=type),alpha=0.8,size=10)
    p3 <- p2 + geom_point(data=subset(df2,type=="small"),color=mycols[4],aes(shape=type),alpha=0.8,size=3)
    p4 <- p3 + geom_point(data=subset(df2,type=="big"),color=mycols[8],aes(shape=type),alpha=0.8,size=2)
    p4 <- p4 +  scale_shape_manual(values=c(17,18,19),labels=c("n=10000","n=213","theoretical")) +
        xlab("out 1") + ylab("out 2") +
        guides(colour=guide_legend(title="observed",override.aes=list(size=3)),
               shape=guide_legend(title="synthetic",override.aes=list(size=3,color=c(mycols[c(8,4,2)]))))
    print(p4)
}
