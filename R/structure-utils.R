plot_jpeg = function(path, add=FALSE)
{
    require('jpeg')
    jpg = readJPEG(path, native=T) # read the file
    res = dim(jpg)[1:2] # get the resolution
    if (!add) # initialize an empty plot area if add==FALSE
	plot(1,1,xlim=c(1,res[1]),ylim=c(1,res[2]),asp=1,type='n',xaxs='i',yaxs='i',xaxt='n',yaxt='n',xlab='',ylab='',bty='n')
    rasterImage(jpg,1,1,res[1],res[2])
}


nixNum <- function(x) {
    y <- x
    for (i in 1:length(x)) {
        tmp <- strsplit(x[i]," ")[[1]]
        tmp <- tmp[-length(tmp)]
        y[i] <- paste(tmp, collapse=" ")
    }
    return(y)
}


cleanNames <- function(dat1,dat2)
{
    tmp1 <- colnames(dat1)
    tmp2 <- colnames(dat2)

    if (grepl("\"",tmp1[1])) {
        tmp1 <- gsub("\"","",tmp1);
        tmp1 <- sapply(tmp1, function(x) substring(x,2))
        colnames(dat1) <- tmp1
    }
    if (grepl("\"",tmp2[1])) {
        tmp2 <- gsub("\"","",tmp2)
        tmp2 <- sapply(tmp2, function(x) substring(x,2))
        colnames(dat2) <- tmp2;
    }

    return(list(dat1=dat1,dat2=dat2))
}

gg_color_hue <- function(n,alpha=1) {
    hues = seq(15, 375, length = n + 1)
    hcl(h = hues, l = 65, c = 100, alpha = alpha)[1:n]
}

rect.dist <- function(X,Y){
    X <- as.matrix(X)
    Y <- as.matrix(Y)
    n <- nrow(X)
    m <- nrow(Y)
    tmp1 <- X%*%t(Y)
    tmp2 <- outer(rep(1, n), rowSums(Y^2))
    tmp3 <- outer(rowSums(X^2), rep(1,m))

    D <- tmp2 - 2*tmp1 + tmp3
    return(D)
}

kernel.stat <- function(X,Y,sigma=0.2){
    n <- nrow(X)
    m <- nrow(Y)

    tmpXX <- sum(exp(-(as.matrix(dist(X))^2)/(2*sigma^2))) - n
    tmpYY <- sum(exp(-(as.matrix(dist(Y))^2)/(2*sigma^2))) - m
    tmpXY <- sum(exp(-(rect.dist(X,Y))/(2*sigma^2)))

    tmp <- tmpXX/(n*(n-1)) + tmpYY/(m*(m-1)) - 2*tmpXY/(m*n)

    return((m+n)*tmp)
}
