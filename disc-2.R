#' ---
#' title: Synthetic validation
#' output:
#'    html_document:
#'      css: ~/RFolder/pandoc.css
#'      highlight: pygments
#'      fig_height: 3.5
#'      fig_width: 8.5
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

syntheticValidation(g, Xhat, vdf)
