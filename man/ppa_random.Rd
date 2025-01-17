% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ppa_random.R
\name{ppa_random}
\alias{ppa_random}
\alias{ppa_random.data.frame}
\title{Calculate Randomly Presences and Pseudo-absences from an Enviromental Space ES and Presences P}
\usage{
ppa_random(ES, P, ES_idx, remove_ES_idx)

\method{ppa_random}{data.frame}(ES, P = NULL, ES_idx = NULL, remove_ES_idx = T)
}
\arguments{
\item{ES}{A dataframe representing an environmental space}

\item{P}{A dataframe representing presences}

\item{ES_idx}{An index to geographical space}

\item{remove_ES_idx}{A flag indicating whether \code{ES_idx} must be removed in final
result.}
}
\description{
Calculate Randomly Presences and Pseudo-absences from an Enviromental Space ES and Presences P
}
