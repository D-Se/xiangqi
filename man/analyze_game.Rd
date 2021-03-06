% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/analyze.R
\name{analyze_game}
\alias{analyze_game}
\title{Analyze a xiangqi game with a UCCI-compliant engine}
\usage{
analyze_game(moves, engine, multipv = 1)
}
\arguments{
\item{moves}{chr vector of length n moves in lan format \strong{h2he2}}

\item{engine}{path to UCCI engine to use for obtaining evaluations}

\item{multipv}{int how many principle variations should be returned}
}
\description{
Analyze a xiangqi game with a UCCI-compliant engine
}
