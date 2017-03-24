semiparYQ <- function(Xhat4, K=8, verbose=FALSE)
{
    suppressMessages(library(mvtnorm))

    n=dim(Xhat4)[1]
    p=dim(Xhat4)[2]

    x=Xhat4
    n=dim(x)[1]
    p=dim(x)[2]
    #    K=8
    pis=rep(1/K,K)
    mean1=rep(0,p)
    mean2=c(-1.1,-0.3,0.5,-0.8,-0.5,-0.2)
    mean3=c(-0.5,2,-1,-0.5, 1.5,-0.1)
    mean1=mean1+0.2*mean3
    mean2=0.8*mean2
    mean3=0.1*mean3
    var1=0.005
    var2=0.008
    means=matrix(NA,K,p) # K x d
    vars=array(NA,dim=c(p,p,K))
    for (i in 1:K)
    {
        t=(i-1)/(K-1)
        means[i,]=(1-t)^2*mean1+2*(1-t)*t*mean3+t^2*mean2
        vars[,,i]=(1-t)*diag(var1,p)+t*diag(var2,p)
    }

    min_vars <- function(arg)
    {
        vars=array(NA,dim=c(p,p,K))
        lkhd=0
        for (k in 1:K)
        {
            t=(k-1)/(K-1)
            vars[,,k]=(1-t)*diag(rep(arg[1],p))+t*diag(rep(arg[2],p))
            lkhd=lkhd+sum(z[,k]*(dmvnorm(x, means[k,], vars[,,k], log=TRUE)+log(pis[k])))
        }
        return(-lkhd)
    }

    min_means <- function(arg)
    {
        mean1=arg[1:p]
        mean2=arg[(p+1):(2*p)]
        mean3=arg[(2*p+1):(3*p)]
        means=matrix(NA,K,p)
        lkhd=0
        for (k in 1:K)
        {
            t=(k-1)/(K-1)
            means[k,]=(1-t)^2*mean1+2*(1-t)*t*mean3+t^2*mean2
            lkhd=lkhd+sum(z[,k]*(dmvnorm(x, means[k,], vars[,,k], log=TRUE)+log(pis[k])))
        }
        return(-lkhd)
    }



    it_max=30
    for (it in 1:it_max)
    {
        means_old=means

        ## E step
        comp_pdf=matrix(NA,n,K)
        z=matrix(NA,n,K)
        for (k in 1:K)
        {
            comp_pdf[,k]=dmvnorm(x, means[k,], vars[,,k], log=FALSE)
        }
        for (i in 1:n)
        {
            z[i,]=comp_pdf[i,]*pis/sum(comp_pdf[i,]*pis)
        }

        ## M step for variance
        opt_var=optim(c(var1,var2), min_vars, method = "L-BFGS-B",
                      lower=c(0.001,0.001),
                      upper=c(0.3,0.3))
        var1=opt_var$par[1]
        var2=opt_var$par[2]
        vars=array(NA,dim=c(p,p,K))
        for (i in 1:K)
        {
            t=(i-1)/(K-1)
            vars[,,i]=(1-t)*diag(rep(var1,p))+t*diag(rep(var2,p))
        }

        ## E step
        comp_pdf=matrix(NA,n,K)
        z=matrix(NA,n,K)
        for (k in 1:K)
        {
            comp_pdf[,k]=dmvnorm(x, means[k,], vars[,,k], log=FALSE)
        }
        for (i in 1:n)
        {
            z[i,]=comp_pdf[i,]*pis/sum(comp_pdf[i,]*pis)
        }

        ## M step for means
        min_means(c(mean1,mean2,mean3))
        opt_means=optim(c(mean1,mean2,mean3), min_means, method = "L-BFGS-B",
                        lower=c(apply(x,2,min),apply(x,2,min),apply(x,2,min)-3*apply(x,2,sd)),
                        upper=c(apply(x,2,max),apply(x,2,max),apply(x,2,max)+3*apply(x,2,sd))
        )
        mean1=opt_means$par[1:p]
        mean2=opt_means$par[(p+1):(2*p)]
        mean3=opt_means$par[(2*p+1):(3*p)]
        means=matrix(NA,K,p)
        for (i in 1:K)
        {
            t=(i-1)/(K-1)
            means[i,]=(1-t)^2*mean1+2*(1-t)*t*mean3+t^2*mean2
        }
        member=apply(z,1,function(x){which(x==max(x))[1]})
        table(member)

        ## update pis or not
        pis=apply(z,2,sum)/sum(z)


        diff=sum(abs(means-means_old))
        ## likelihood function
        individual_pdf=rep(0,n)
        for (k in 1:K)
        {
            individual_pdf=individual_pdf+pis[k]*dmvnorm(x, means[k,], vars[,,k], log=FALSE)
        }
        incomp_lkhd=sum(log(individual_pdf))
        if (verbose) {
            cat("incomplete likelihood is",incomp_lkhd,"\n")
            cat("diff is",diff,"\n")
        }
    }

    # Ending EM algorithm
    return(list(means=means,mean1=mean1,mean2=mean2,mean3=mean3,vars=vars,pis=pis))
}


plotMLE <- function(Xhat, vdf)
{
    suppressMessages(library(ggplot2))
    suppressMessages(library(RColorBrewer))
    suppressMessages(library(packcircles))


    K <- 100
    Xhat4 <- Xhat[vdf$type=="KC",]
    n <- nrow(Xhat4)
    p <- ncol(Xhat4)

    out100 <- semiparYQ(Xhat4, K=K, verbose=FALSE)

    means <- out100$means
    vars <- t(sapply(1:dim(out100$vars)[3],function(x) sqrt(diag(out100$vars[,,x]))))
    K <- nrow(means)

    Xhat5 <- rbind(Xhat4,means)
    colnames(Xhat5) <- c(paste("out",1:3),paste("in",1:3))
    ggY <- makePairs(as.data.frame(Xhat5))
    ndf <- nrow(ggY$all)
    mydf.yq <- data.frame(ggY$all, type=rep(c(rep("KC",nrow(Xhat4)),rep("MLE",nrow(means))),length=ndf))

    alpha <- 0.7
    tmpcol <- gg_color_hue(2,alpha=alpha)

    # Figure 8
    p1 <- ggplot(mydf.yq, aes_string(x = "x", y = "y")) +
        facet_grid(xvar ~ yvar, scales = "free") +
        geom_point(aes(colour=type), na.rm = TRUE, alpha=alpha,size=1) +
        scale_colour_manual(values=c(tmpcol[1],"darkred")) +
        #  scale_shape_manual(values=1:nlevels(mydf3$cluster)) +
        #  scale_shape_manual(values=as.character(1:mc3$G)) +
        stat_density(aes(x = x, y = ..scaled.. * diff(range(x)) + min(x)),
                     data = ggY$densities, position = "identity",
                     colour = "grey20", geom = "line") +
        theme(axis.title=element_text(size=0)) +
        theme(axis.text.x=element_text(size=0)) +
        theme(axis.ticks = element_line(size = 0)) +
        theme(axis.text.y=element_text(size=0)) +
        theme(strip.text=element_text(size=rel(1.2))) +
        theme(legend.title = element_text(colour="black", size=14, face="bold")) +
        theme(legend.text = element_text(colour="black", size = 12, face = "plain"))
    print(p1)

    # Figure 9
    dd <- data.frame(x=Xhat[,1], y=Xhat[,2], vdf)
    mytab <- with(dd,table(type,cluster))
    type2 <- factor(rownames(mytab)[apply(mytab,2,which.max)])
    dd$type2 <- type2[vdf$cluster]
    Khat <- max(as.numeric(dd$cluster))
#    p3 <- ggplot(dd,aes(x=x,y=y)) +
#        geom_point(aes(shape=cluster,color=type,fill=type),size=3,alpha=0.9) +
#        scale_shape_manual(values=as.character(1:Khat)) #+
    dd3 <- subset(dd, type2!="KC")    # ellipse
    dd4 <- data.frame(x=means[,1],y=means[,2],sdx=vars[,1],sdy=vars[,2])
    tmpcol <- gg_color_hue(2)

    type <- names(table(dd$type))[-1]
    means2 <- t(sapply(type, function(x) colMeans(dd[dd$type2==x,1:2])))
    means2 <- data.frame(x=means2[,1],y=means2[,2],col=factor(type))

 #   p3 <- p3 + stat_ellipse(data=dd3,geom="polygon",aes(fill=type2,color=type2),alpha=.2,show.legend=F) +
 #       geom_line(data=dd4,color="darkred",size=1.5) +
 #       geom_ribbon(data=dd4,aes(ymin=y-1.96*sdx,ymax=y+1.96*sdy),alpha=0.2,fill="darkred")

    layout <- data.frame(id=1:nrow(means), x=means[,1], y=means[,2], r=1.96*vars[,1])
    cir <- circlePlotData(layout, id.col=1, xyr.cols=2:4)
    p3 <- ggplot(dd,aes(x=x,y=y)) +
        geom_polygon(data=cir, aes(x, y, group=id), fill="rosybrown1", colour="rosybrown1", alpha=1) +
        geom_point(aes(shape=cluster,color=type,fill=type),size=3,alpha=0.9) +
        scale_shape_manual(values=as.character(1:Khat)) #+
    p3 <- p3 + stat_ellipse(data=dd3,geom="polygon",aes(fill=type2,color=type2),alpha=.2,show.legend=F) +
        geom_line(data=dd4,color="darkred",size=1.5) + coord_equal() +
        geom_point(data=means2, aes(x=x,y=y,color=col),size=5,show.legend=FALSE) +
        xlab("out 1") + ylab("out 2")
    print(p3)


    # Figure 10
    KC.info2 <- vdf[vdf$type=="KC",]
    KC.info2$claw <- factor(KC.info2$claw,
                            levels = c('1','2','3','4','5','6','0'),ordered = TRUE)

    p4 <- ggplot(KC.info2, aes(factor(claw),dist,color=factor(claw),fill=factor(claw)))
    p4 <- p4 + geom_boxplot(notch=FALSE,alpha=0.5) + geom_jitter(width = 0.2) +
        guides(fill=FALSE,color=FALSE) + xlab("number of claws") + ylab("distance to neuropile in nm")
    print(p4)

    # Figure 11
    out8 <- semiparYQ(Xhat4, K=8, verbose=FALSE)
    mean1 <- out8$mean1
    mean2 <- out8$mean2
    mean3 <- out8$mean3
    t_proj_ls <- rep(NA,n)
    proj_ls <- matrix(NA,n,p)
    for (i in 1:n)
    {
        t_ls <- seq(-0.2,1.2,0.0001)
        dist_ls=rep(NA,length(t_ls))
        for (i_t in 1:length(t_ls))
        {
            t <- t_ls[i_t]
            this_mean <-(1-t)^2*mean1+2*(1-t)*t*mean3+t^2*mean2
            dist_ls[i_t] <- sum((this_mean-Xhat4[i,])^2)
        }
        proj_t <- t_ls[which(dist_ls==min(dist_ls))[1]]
        t_proj_ls[i] <- proj_t
        proj_ls[i,] <- (1-proj_t)^2*mean1+2*(1-proj_t)*proj_t*mean3+proj_t^2*mean2
    }
    Xhat_dist <- vdf$dist[vdf$type=="KC"]

    Xhat5 <- rbind(Xhat4[,1:2],proj_ls[,1:2])
    df <- data.frame(x=Xhat5[,1],y=Xhat5[,2],idx=rep(1:nrow(Xhat4),times=2),type=rep(c("KC","ti"),each=nrow(Xhat4)))
    p5 <- ggplot(df, aes(x,y,group=idx,color=type)) + geom_point(size=2) + geom_line(color="red",linetype=3) + xlab("out 1") + ylab("out 2") +
        scale_colour_manual(values=c(tmpcol[1],"darkred"),labels=c("KC",expression(t[i])))
    print(p5)

    # Figure 12
    df <- data.frame(x=t_proj_ls,y=Xhat_dist,claw=vdf$claw[vdf$type=="KC"])
    p6 <- ggplot(df, aes(x, y)) +  geom_point(aes(size=claw),color=tmpcol[1],alpha=.5) +
        geom_smooth(method = "lm", se = TRUE) +
        xlab(expression(t[i])) + ylab(expression(delta[i]))
    print(p6)

    return(list(out100=out100,out8=out8))
}
