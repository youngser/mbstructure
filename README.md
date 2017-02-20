
<style type="text/css">
.table {
    width: 40%;
}
</style>



## Semiparametric spectral modeling of the Drosophila connectome
**[Department of Applied Mathematics and Statistics](http://engineering.jhu.edu/ams/)**      
**[Center for Imaging Science](http://www.cis.jhu.edu)**  
**[Human Language Technology Center of Excellence](http://hltcoe.jhu.edu)**  
**[Johns Hopkins University](http://www.jhu.edu)**  
and  
**[University of Cincinati](http://business.uc.edu)**  
and  
**[HHMI Janelia Research Campus](hhmi.org)**  

-----

> C.E. Priebe,  Y. Park, M. Tang, A, Athreya, V. Lyzinski, J. Vogelstein,
Y. Qin, B. Cocanougher, K. Eichler, M. Zlatic, A. Cardona,
"[Semiparametric spectral modeling of the Drosophila connectome](http://arxiv.org/abs/1502.03391)," _Journal of the American Statistical Association Application and Case Studies_, submmitted, 2017.


## Abstract

> We present semiparametric spectral modeling of the complete larval Drosophila mushroom body connectome. The resulting connectome code derived via semiparametric Gaussian mixture mod- eling composed with adjacency spectral embedding captures biologically relevant neuronal prop- erties.

> **Keywords**: Connectome; Network; Graph; Spectral embedding; Mixture model; Clustering

<figure>
<img src="diagram-circuit.jpg" width="700px" />
  <figcaption>Illustration of the Drosophila Mushroom Body circuit as a (K = 4)-block directed stochastic block model. This first-order simplification of the structure of the circuit provides a framework for graph embedding inference, and for subsequent Knowledge Base interaction.</figcaption>
</figure>

## Codes and Experiments

To run the experiemnts in the paper, please follow these steps:

### The larval _Drosophila_ mushroom body connectome

Output of this chunk looks like [this](sec2.html).


```r
library(mbstructure)
data(MBconnectome)

out <- generate.graph(newrdat, vdf.right)
g <- out$g
vdf <- out$vdf
plotConnections(g, vdf)
```

### Spectral clustering

Output of this chunk looks like [this](sec3.html).


```r
dmax <- 50
Xhat <- doEmbed(g, dmax)

Kmax <- 19
mc <- Mclust(Xhat, 1:Kmax)
vdf$cluster <- factor(mc$class)
plotBIC(mc)
plotClustering(Xhat, mc, vdf)
```

### Semiparametric spectral modeling

Output of this chunk looks like [this](sec4.html).


```r
synthMB(g, vdf)
plotMLE(Xhat, vdf) 
```





## `R` Package

The latest `R` source package can be downloaded from [here](http://www.cis.jhu.edu/~parky/mbstructure_0.1.0.tar.gz).  
It can be installed via:

```r
install.packages("http://username:password@www.cis.jhu.edu/~parky/MBstructure/mbstructure_0.1.0.tar.gz",type="source",method="wget")
```

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
## Imports:       igraph, Matrix, lattice, ggplot2, mclust, mvtnorm,
##                RColorBrewer, packcircles
## Author:        Youngser Park, Yichen Qin
## Maintainer:    Youngser Park <youngser@jhu.edu>
## Description:   Routine to perform semiparametric spectral modeling
##                of the complete larval Drosophila mushroom body
##                connectome.
## License:       GPL (>= 2)
## URL:           http://www.cis.jhu.edu/~parky/MBstructure.html
## LazyData:      TRUE
## RoxygenNote:   5.0.1
## Built:         R 3.3.2; ; 2017-02-20 13:26:40 UTC; unix
## 
## Index:
## 
## hello                   Hello, World!
```

-----
*prepared by <youngser@jhu.edu> on Mon Feb 20 10:00:11 2017*
