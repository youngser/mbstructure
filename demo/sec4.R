#' ---
#' title: Semiparametric spectral modeling
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
Xhat <- doEmbed(g, dmax, plotElbow=FALSE)

suppressMessages(library(mclust))
Kmax <- 19
mc <- Mclust(Xhat, 1:Kmax)
vdf$cluster <- factor(mc$class)

sout <- synthMB(g, vdf)
semiout <- plotMLE(Xhat, vdf)
