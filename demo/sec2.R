#' ---
#' title: The larval _Drosophila_ mushroom body connectome
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
plotConnections(g, vdf)
