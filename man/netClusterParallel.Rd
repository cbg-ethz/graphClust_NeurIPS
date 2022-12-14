% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/clusterfns.R
\name{netClusterParallel}
\alias{netClusterParallel}
\title{netClusterParallel}
\usage{
netClusterParallel(
  myData,
  k_clust = 3,
  n_bg = 0,
  itLim = 20,
  EMseeds = 1:5,
  BBMMClust = TRUE,
  edgepmat = NULL,
  bdepar = list(chi = 0.5, edgepf = 16)
)
}
\arguments{
\item{myData}{Data to be clustered}

\item{k_clust}{Number of clusters}

\item{n_bg}{Number of covariates}

\item{itLim}{Maximum number of iterations}

\item{EMseeds}{seeds}

\item{BBMMClust}{binary clustering before network-based clustering (TRUE by default)}

\item{edgepmat}{a matrix of penalized edges in the search space}

\item{bdepar}{Hyperparameters for structure learning (BDE score)}
}
\value{
a list containing the clusterMemberships, DAGs, best seed and "assignprogress"
}
\description{
Network-based clustering of multiple seeds using parallel computing
}
