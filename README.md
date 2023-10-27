Network-Based Clustering of Pan-Cancer Data Accounting for Clinical Covariates
-----------

[![License: GPL v3](https://img.shields.io/badge/License-GPLv3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0)

This repository contains the code to reproduce the results of the NeurIPS 2022 LMRL workshop paper "[Network-Based Clustering of Pan-Cancer Data Accounting for Clinical Covariates](https://openreview.net/pdf?id=mnvPgQTt2Xs)".

Installation
-----------

In order to install the package, it suffices to launch
`
R CMD INSTALL path/to/graphClust
`
from a terminal, or `make install` from within the package source folder.

Being hosted on GitHub, it is possible to use the `install_github`
tool from an R session:

```
if (!requireNamespace("BiocManager", quietly = TRUE)) install.packages("BiocManager")
BiocManager::install(c("Rgraphviz", "RBGL"))

library("devtools")
install_github("cbg-ethz/graphClust_NeurIPS")
```

`graphClust` requires R `>= 3.5`, and depends on 
`pcalg`, `reshape2`, `BiDAG` (>= 2.0.2),
`RBGL`, `clue` and `grDevices`.

Simulations
-----------

**Figure 2** can be reproduced by running the script `simulations/figure_2-simulation.R`. Analogously, **Figure 4** in the appendix can be reproduced by running the script `simulations/figure_4-simulation.R`. The simulations can be modified and executed in the `simulations/cluster-scripts` folder.

Pan-Cancer Data
-----------

**Figure 3** can be reproduced by runnign the script `tcga_analysis/figure_3-km_plot.R`. The results of **Table 1** can be reproduced by runnign the script `tcga_analysis/table_1-cox_analysis.R`. A reproducability analysis for a range of different seeds can be found in `tcga_analysis/reproducability_different_seeds`. The hyperparameters of the cluster algorithms can be modified and executed in the `tcga_analysis/clustering folder` folder.

Example
-------

```{r eval=FALSE}
library(graphClust)

# Simulate binary data from 3 clusters
k_clust <- 3
ss <- c(400, 500, 600) # samples in each cluster
simulation_data <- sampleData(k_clust = k_clust, n_vars = 20, n_samples = ss)
sampled_data <- simulation_data$sampled_data

# Network-based clustering
cluster_res <- get_clusters(sampled_data, k_clust = k_clust)

# Calculate the ARI 
library(mclust)
adjustedRandIndex(simulation_data$cluster_membership, cluster_res_t$clustermembership)

# Visualize the networks
library(ggplot2)
library(ggraph)
library(igraph)
library(ggpubr)

graphClust::plot_clusters(cluster_res_t)

# Visualize a single network
my_graph <- igraph::graph_from_adjacency_matrix(cluster_res_t$DAGs[[1]], mode="directed")
graphClust::nice_DAG_plot(my_graph)

```
