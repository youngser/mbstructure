#' ---
#' title: Hemispheric validation, right vs. left
#' output:
#'    html_document:
#'      css: ~/RFolder/pandoc.css
#'      highlight: pygments
#'      fig_height: 4
#'      fig_width: 8
#' ---

library(mbstructure)
data(MBconnectome)
suppressMessages(library(mclust))

out <- generate.graph(newrdat, vdf.right)
g <- out$g
vdf <- out$vdf

dmax <- 50
Xhat <- doEmbed(g, dmax, plotElbow = FALSE)

Kmax <- 19
mc <- Mclust(Xhat, 2:Kmax)
vdf$cluster <- factor(mc$class)

Xhat4 <- Xhat[vdf$type=="KC",]
out100 <- semiparYQ(Xhat4, K=100, verbose=FALSE)

right.vs.left(Xhat, vdf, out100)
