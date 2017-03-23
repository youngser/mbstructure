


## Semiparametric spectral modeling of the Drosophila connectome
**[Department of Applied Mathematics and Statistics](http://engineering.jhu.edu/ams/)**      
**[Center for Imaging Science](http://www.cis.jhu.edu)**  
**[Human Language Technology Center of Excellence](http://hltcoe.jhu.edu)**  
**[Johns Hopkins University](http://www.jhu.edu)**  
and  
**[University of Cincinnati](http://business.uc.edu)**  
and  
**[HHMI Janelia Research Campus](hhmi.org)**  

-----

> C.E. Priebe,  Y. Park, M. Tang, A, Athreya, V. Lyzinski, J. Vogelstein,
Y. Qin, B. Cocanougher, K. Eichler, M. Zlatic, A. Cardona,
"[Semiparametric spectral modeling of the Drosophila connectome](http://arxiv.org/abs/1502.03391)," _Journal of the American Statistical Association Application and Case Studies_, submitted, 2017.


## Abstract

We present semiparametric spectral modeling of the complete larval Drosophila mushroom body connectome. Motivated by a thorough exploratory data analysis of the network via Gaussian mixture modeling (GMM) in the adjacency spectral embedding (ASE) representation space, we introduce the stochastic structure model (SSM) for network modeling and inference. SSM is a generalization of the stochastic block model (SBM) and a special case of the random dot product graph (RDPG) latent position model, and is amenable to semiparametric GMM in the ASE representation space. The resulting connectome code derived via semiparametric GMM composed with ASE captures latent connectome structure and elucidates biologically relevant neuronal properties.

> **Keywords**: Connectome; Network; Graph; Spectral embedding; Mixture model; Clustering

<figure>
<img src="diagram-circuit.jpg" width="700px" />
  <figcaption>Illustration of the larval Drosophila mushroom body connectome as a directed graph on four neuron types.</figcaption>
</figure>

## Data

HHMI Janelia recently reconstructed the complete wiring diagram of the higher order parallel fiber system for associative learning in the larval Drosophila brain, the mushroom body (MB). Memories are thought to be stored as functional and structural changes in connections between neurons, but the complete circuit architecture of a higher-order learning center involved in memory formation or storage has not been known in any organism ... until now. This data set provides a real and important example for initial investigation into synapse-level structural connectome modeling.  

Our MB connectome was obtained via serial section transmission electron microscopy of an entire larval Drosophila nervous system. This connectome contains the entirety of MB intrinsic neurons called Kenyon cells and all of their pre- and post-synaptic partners.

The data, both right and left MB connectomes as well as their meta information, are included in this R package and can be loaded into `R` via `data(MBconnectome)`.

## Codes and Experiments

To run the experiemnts in the paper, please follow these steps.  
(NB: All the codes are in the `demo` folder at [github](https://github.com/youngser/mbstructure).)

### `R` Package

The latest `R` source package can be installed via:

```r
install.packages("http://www.cis.jhu.edu/~parky/MBstructure/mbstructure_0.1.0.tar.gz",type="source",method="wget")
```

or via `github`:


```r
require(devtools)
devtools::install_github("youngser/mbstructure")
```

### The larval _Drosophila_ mushroom body connectome

Output of this chunk (Figure 2) is shown [here](demo/sec2.html).


```r
library(mbstructure)
data(MBconnectome)

out <- generate.graph(newrdat, vdf.right)
g <- out$g
vdf <- out$vdf
plotConnections(g, vdf)
```

### Spectral clustering

Output of this chunk (Figures 3, 5, 6, 7, Tables 1, 7) is shown [here](demo/sec3.html).


```r
dmax <- 50
Xhat <- doEmbed(g, dmax)

Kmax <- 19
mc <- Mclust(Xhat, 2:Kmax)
vdf$cluster <- factor(mc$class)
plotBIC(mc)
plotClustering(Xhat, mc, vdf)
```

### Semiparametric spectral modeling

Output of this chunk (Figures 8, 9, 10, 11, 12, 13) is shown [here](demo/sec4.html).  
Warning: This takes several minutes to run on my laptop!


```r
sout4 <- synthMB(g, Xhat, vdf, labK=vdf$type, Khat=4, dtype="truth", doplot=TRUE)
semiout <- plotMLE(Xhat, vdf) 
```


### Discussion

#### Directed! Weighted?

Output of this chunk (Figure 14) is shown [here](demo/disc-1.html).  
Warning: This takes about _half an hour_ to run on my laptop!


```r
g.w <- out$g.w
compairARI(g, g.w, vdf)
```

#### Synthetic validation

Output of this chunk (Figure 15) is shown [here](demo/disc-2.html).  
Warning: This takes a few minutes to run on my laptop!


```r
syntheticValidation(g, Xhat, vdf)
```

#### Hemispheric validation: right vs. left

Output of this chunk (Figure 16) is shown [here](demo/disc-3.html).  
Warning: This takes a few minutes to run on my laptop!


```r
right.vs.left(Xhat, vdf, semiout$out100)
```

## Software and Hardware Information


```r
library(help='mbstructure')
```

```
## 		Information on package 'mbstructure'
## 
## Description:
## 
## Package:       mbstructure
## Type:          Package
## Title:         Semiparametric spectral modeling of the complete
##                larval Drosophila mushroom body connectome
## Version:       0.1.0
## Depends:       R (>= 3.0)
## Imports:       igraph, Matrix, lattice, ggplot2, mclust, irlba,
##                mvtnorm, RColorBrewer, packcircles
## Author:        Youngser Park, Yichen Qin
## Maintainer:    Youngser Park <youngser@jhu.edu>
## Description:   Routine to perform semiparametric spectral modeling
##                of the complete larval Drosophila mushroom body
##                connectome.
## License:       GPL (>= 2)
## URL:           http://www.cis.jhu.edu/~parky/MBstructure.html
## LazyData:      TRUE
## RoxygenNote:   5.0.1
## Built:         R 3.3.2; ; 2017-03-23 16:14:12 UTC; unix
```

```r
sessionInfo()
```

```
## R version 3.3.2 (2016-10-31)
## Platform: x86_64-apple-darwin13.4.0 (64-bit)
## Running under: macOS Sierra 10.12.3
## 
## locale:
## [1] en_US.UTF-8/en_US.UTF-8/en_US.UTF-8/C/en_US.UTF-8/en_US.UTF-8
## 
## attached base packages:
## [1] stats     graphics  grDevices utils     datasets  methods   base     
## 
## other attached packages:
##  [1] xtable_1.8-2       printr_0.0.6       mvtnorm_1.0-5     
##  [4] packcircles_0.1.1  RColorBrewer_1.1-2 mclust_5.2.2      
##  [7] lattice_0.20-34    Matrix_1.2-7.1     igraph_1.1.0      
## [10] ggplot2_2.2.1      knitr_1.15.1      
## 
## loaded via a namespace (and not attached):
##  [1] Rcpp_0.12.9      magrittr_1.5     munsell_0.4.3    colorspace_1.3-2
##  [5] stringr_1.1.0    plyr_1.8.4       tools_3.3.2      grid_3.3.2      
##  [9] gtable_0.2.0     htmltools_0.3.5  yaml_2.1.14      lazyeval_0.2.0  
## [13] rprojroot_1.2    digest_0.6.12    assertthat_0.1   tibble_1.2      
## [17] evaluate_0.10    rmarkdown_1.3    stringi_1.1.2    scales_0.4.1    
## [21] backports_1.0.4  pkgconfig_2.0.0
```

-----
*prepared by <youngser@jhu.edu> on Thu Mar 23 14:46:55 2017*
