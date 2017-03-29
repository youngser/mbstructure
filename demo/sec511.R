#' ---
#' title: Directed! Weighted?
#' output:
#'    html_document:
#'      css: ~/RFolder/pandoc.css
#'      highlight: pygments
#'      fig_height: 4
#'      fig_width: 8
#' ---

library(mbstructure)
data(MBconnectome)

out <- generate.graph(newrdat, vdf.right)
g.uw <- out$g
g.w <- out$g.w
vdf <- out$vdf

compareARI(g.uw, g.w, vdf)
