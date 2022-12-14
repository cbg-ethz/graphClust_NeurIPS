% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/generatebinaryBN.R
\name{sampleData}
\alias{sampleData}
\title{sampleData}
\usage{
sampleData(
  k_clust = 3,
  n_vars = 20,
  n_bg = 3,
  n_samples = NULL,
  bgedges = "different",
  equal_cpt_bg = TRUE
)
}
\arguments{
\item{k_clust}{Number of clusters}

\item{Nvars}{Number of variables}

\item{sseed}{Seed}

\item{samplesizes}{Sample sizes}
}
\value{
sampled binary data
}
\description{
Sample binary data from different Bayes nets
}
