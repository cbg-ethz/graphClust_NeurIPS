#!/usr/bin/env Rscript

# Network-based clustering of mutational and covariate data
library(graphClust)
library(mclust)

# load data
mut_cov_data <- read.table("../data/categorical-mutationCovariate-matrix.txt")
string_edgepmat <- as.matrix(read.table("../data/string-edgepmat.txt"))
blacklist_edgepmat <- as.matrix(read.table("../data/blacklist-edgepmat.txt"))

# define covariates
n_cov <- 2

# clustering
itLim <- 300
set.seed(seedy) # set seed

clusterResPlain <- get_clusters(mut_cov_data, k_clust = 10,
                                n_bg = n_cov, itLim = itLim,
                                edgepmat = string_edgepmat,
                                blacklist = blacklist_edgepmat, categorical = TRUE)

saveRDS(clusterResPlain, paste0("memberships_seed_", seedy, ".rds"))


