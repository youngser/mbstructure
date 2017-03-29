# Semiparametric spectral modeling of the Drosophila connectome



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


# Abstract

We present semiparametric spectral modeling of the complete larval Drosophila mushroom body connectome. Motivated by a thorough exploratory data analysis of the network via Gaussian mixture modeling (GMM) in the adjacency spectral embedding (ASE) representation space, we introduce the stochastic structure model (SSM) for network modeling and inference. SSM is a generalization of the stochastic block model (SBM) and a special case of the random dot product graph (RDPG) latent position model, and is amenable to semiparametric GMM in the ASE representation space. The resulting connectome code derived via semiparametric GMM composed with ASE captures latent connectome structure and elucidates biologically relevant neuronal properties.

> **Keywords**: Connectome; Network; Graph; Spectral embedding; Mixture model; Clustering

<figure>
<img src="diagram-circuit.jpg" width="700px" />
  <figcaption>Illustration of the larval Drosophila mushroom body connectome as a directed graph on four neuron types.</figcaption>
</figure>

# Data

HHMI Janelia recently reconstructed the complete wiring diagram of the higher order parallel fiber system for associative learning in the larval Drosophila brain, the mushroom body (MB). Memories are thought to be stored as functional and structural changes in connections between neurons, but the complete circuit architecture of a higher-order learning center involved in memory formation or storage has not been known in any organism ... until now. This data set provides a real and important example for initial investigation into synapse-level structural connectome modeling.  

Our MB connectome was obtained via serial section transmission electron microscopy of an entire larval Drosophila nervous system. This connectome contains the entirety of MB intrinsic neurons called Kenyon cells and all of their pre- and post-synaptic partners.

The data, both right and left MB connectomes as well as their meta information, are included in this `R` package and can be loaded into `R` via `data(MBconnectome)`. See below demos for the details.

# Codes and Demos

To run the experiemnts in the paper, please follow these steps.  
(NB: All the codes are in the `demo` folder at [github](https://github.com/youngser/mbstructure).)

## `R` Package

The latest `R` source package can be installed via:

```r
install.packages("http://www.cis.jhu.edu/~parky/MBstructure/mbstructure_0.1.0.tar.gz",type="source",method="wget")
```

or through `github`:


```r
require(devtools)
devtools::install_github("youngser/mbstructure")
```

## Demos

To reproduce most of the Figures and Tables in the manuscript, please follow these steps:


```r
library(mbstructure)

# Figure 2 in Section 2
demo(sec2)

# Figures 3, 5, 6, 7 and Tables 1 & 7 in Section 3
demo(sec3)

# Figures 8, 9, 10, 11, 12, 13 in Section 4
demo(sec4) # Warning: This takes several minutes to run on my laptop!

# FIgure 14 in Section 5.1.1
demo(sec511) # Warning: This takes about half an hour to run on my laptop!

# Figure 15 in Section 5.1.2
demo(sec512) # Warning: This takes a few minutes to run on my laptop!

#Figure 16 in Section 5.1.4
demo(sec514) # Warning: This takes a few minutes to run on my laptop!
```

The outputs of the demos are here:

* [Section 2](demo/sec2.html)
* [Section 3](demo/sec3.html)
* [Section 4](demo/sec4.html)
* [Section 5.1.1](demo/sec511.html)
* [Section 5.1.2](demo/sec512.html)
* [Section 5.1.4](demo/sec514.html)

# Software and Hardware Information


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
## Built:         R 3.3.2; ; 2017-03-28 16:47:42 UTC; unix
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
*prepared by <youngser@jhu.edu> on Tue Mar 28 14:13:40 2017*
