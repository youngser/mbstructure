#' ---
#' title: Spectral clustering
#' output:
#'    html_document:
#'      css: ~/RFolder/pandoc.css
#'      highlight: pygments
#'      fig_height: 6
#'      fig_width: 6
#' ---

library(mbstructure)
data(MBconnectome)

out <- generate.graph(newrdat, vdf.right)
g <- out$g
vdf <- out$vdf

dmax <- 50
Xhat <- doEmbed(g, dmax)

suppressMessages(library(mclust))
Kmax <- 19
mc <- Mclust(Xhat, 1:Kmax)
vdf$cluster <- factor(mc$class)
plotBIC(mc)

plotClustering(Xhat, mc, vdf)

table(type=vdf$type,Khat6=vdf$cluster)
table(claw=vdf$claw,Khat6=vdf$cluster)

