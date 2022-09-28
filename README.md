netClust: Network-based clustering with covariate adjustment
-----------

[![License: GPL v3](https://img.shields.io/badge/License-GPLv3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0)

This repository contains the code to reproduce the results of the NeurIPS 2022 LMRL workshop submission "Network-Based Clustering of Pan-Cancer Data Accounting for Clinical Covariates".

Installation
-----------

In order to install the package, it suffices to launch
`R CMD INSTALL path/to/netClust`
from a terminal, or `make install` from within the package source folder.

`netClust` requires R `>= 3.5`, and depends on 
`BiDAG` (>= 2.0.2), `reshape2`, `pcalg`,
`RBGL`, `clue` and `grDevices`.

