Network-Based Clustering of Pan-Cancer Data Accounting for Clinical Covariates
-----------

[![License: GPL v3](https://img.shields.io/badge/License-GPLv3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0)

This repository contains the code to reproduce the results of the NeurIPS 2022 LMRL workshop paper "[Network-Based Clustering of Pan-Cancer Data Accounting for Clinical Covariates](https://openreview.net/pdf?id=mnvPgQTt2Xs)".

![graphClust_img](https://user-images.githubusercontent.com/38718986/204546786-16147408-ef55-460b-abb2-3ee872f65c3d.png)

Installation
-----------

In order to install the package, it suffices to launch
`
R CMD INSTALL path/to/netClust
`
from a terminal, or `make install` from within the package source folder.

Being hosted on GitHub, it is possible to use the `install_github`
tool from an R session:

```
library("devtools")
install_github("cbg-ethz/graphClust_NeurIPS")
```

`netClust` requires R `>= 3.5`, and depends on 
`pcalg`, `reshape2`, `BiDAG` (>= 2.0.2),
`RBGL`, `clue` and `grDevices`.

Simulations
-----------

**Figure 2** can be reproduced by running the script `simulations/figure_2-simulation.R`. Analogously, **Figure 4** in the appendix can be reproduced by running the script `simulations/figure_4-simulation.R`. The simulations can be modified and executed in the `simulations/cluster-scripts` folder.

Pan-Cancer Data
-----------

**Figure 3** can be reproduced by runnign the script `tcga_analysis/figure_3-km_plot.R`. The results of **Table 1** can be reproduced by runnign the script `tcga_analysis/table_1-cox_analysis.R`. A reproducability analysis for a range of different seeds can be found in `tcga_analysis/reproducability_different_seeds`. The hyperparameters of the cluster algorithms can be modified and executed in the `tcga_analysis/clustering folder` folder.
