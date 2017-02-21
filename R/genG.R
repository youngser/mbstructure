generate.graph <- function(dat,vdf)
{
    suppressMessages(library(igraph))

    (tab <- table(vdf$type))
    n <- sum(tab)
    cs <- c(0,cumsum(tab))
    A <- matrix(0,n,n)

    for (i in 1:length(dat)) {
        bname <- names(dat)[i]
        bname2 <- strsplit(bname, "-")[[1]]
        pos1 <- match(bname2[1], names(tab))
        pos2 <- match(bname2[3], names(tab))

        start1 <- cs[pos1]+1; end1 <- cs[pos1+1]
        start2 <- cs[pos2]+1; end2 <- cs[pos2+1]
        A[start1:end1, start2:end2] <- as.matrix(dat[[i]])
    }

    diag(A) <- 0
    rownames(A) <- colnames(A) <- vdf[,1]

    g <- graph.adjacency(A)
    # remove isolates => lcc
    iso <- which(degree(g)==0)
    vdf <- vdf[-iso,]
    g <- delete_vertices(g,iso);
    g.w <- g

    # remove self loop & multiple edges
    g <- simplify(g) # not working? try manually
    A <- as.matrix(g[]); A[A>0] <- 1
    g <- graph.adjacency(A)

    # add graph attributes
    data(claw)
    neuron.name <- sapply(vdf$v, function(x) gsub("#","",x))
    claw.name <- sapply(claw$Neuron, function(x) gsub("#","",x))
    claw.match <- match(neuron.name, claw.name)

    claw.type <- rep(0,nrow(vdf))
    vdf$claw <- claw[claw.match,"N.claws"]
    vdf$claw[vdf$type=="KC" & is.na(vdf$claw)] <- 1 # was 0 and it's wrong
    vdf$dist <- claw[claw.match,"distance_to_neuropile"]
    vdf$dist[vdf$type=="KC" & is.na(vdf$dist)] <- 0
    KC.age <- rep("mature",sum(vdf$type=="KC"))
    KC.age[grep("young",vdf$v)] <- "young"
    vdf$age <- NA
    vdf$age[vdf$type=="KC"] <- KC.age

    V(g)$type <- as.character(vdf$type)
    V(g)$claw <- vdf$claw
    V(g)$dist <- vdf$dist

    return(list(g=g, g.w=g.w, vdf=vdf))
}
