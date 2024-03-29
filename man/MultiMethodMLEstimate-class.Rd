% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/classes_and_methods.R
\docType{class}
\name{MultiMethodMLEstimate-class}
\alias{MultiMethodMLEstimate-class}
\title{S4 object containing the results of multi-method ML accuracy estimates}
\description{
S4 object containing the results of multi-method ML accuracy estimates
}
\section{Slots}{

\describe{
\item{\code{results}}{a list of estimated accuracy statistics}

\item{\code{names}}{a list containing vectors of names of various dimensions}

\item{\code{data}}{a copy of the data used to generate the estimated values}

\item{\code{iter}}{an integer number of iterations needed for the EM algorithm to converge}

\item{\code{prog}}{a list containing the values calculated during each iteration of
the EM algorithm}

\item{\code{type}}{a string describing the data type}
}}

