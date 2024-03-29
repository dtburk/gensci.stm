% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/run_stm.R
\name{run_stm}
\alias{run_stm}
\title{Run STM analysis}
\usage{
run_stm(input_file, output_dir, k, split_docs = FALSE,
  representative_docs = TRUE)
}
\arguments{
\item{input_file}{Path to file containing objects created by function \code{create_stm_input}.}

\item{output_dir}{Path to directory in which to save model output.}

\item{k}{Number of topics for STM analysis.}

\item{split_docs}{Logical indicating whether documents have been split into 3,000 word chunks, in which case a flag will be added as a prevalence covariate to indicate which chunks come from the same full document.}

\item{representative_docs}{If TRUE (the default), save full texts of most representative documents. If FALSE, save only a list of titles of the most representative documents.}
}
\description{
Run STM analysis and save output to aid interpretation.
}
