#!/usr/bin/env Rscript

library(combinat)
library(netClust)
library(mclust)

# benchmark
results <- netClust:::benchmark_methods(k_clust = 2, n_vars = 20, n_bg = 5, n_it = 20, n_samples = NULL,
                             bgedges = "different", equal_cpt_bg = TRUE)

saveRDS(results, paste0("results--k_clust-", k_clust, "--n_vars-", n_vars, "--n_bg-", n_bg, "--n_it-", n_it, ".rds"))


# basename(this.path(verbose = getOption("verbose")))

