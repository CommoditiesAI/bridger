% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/findCompositeHands.R
\name{find_weak1NT_LHOx}
\alias{find_weak1NT_LHOx}
\title{find_weak1NT_LHOx}
\usage{
find_weak1NT_LHOx(
  HC_low = 12,
  HC_high = 14,
  cardLen_low = 2,
  cardLen_high = 5,
  pointsForDouble = 15
)
}
\arguments{
\item{HC_low}{The minimum number of high-card points}

\item{HC_high}{The maximum number of high-card points}

\item{cardLen_low}{The minimum length of a suit}

\item{cardLen_high}{The maximum length of a suit}

\item{pointsForDouble}{Minimum number of points for West to double}
}
\value{
id and seat of a compliant hand
}
\description{
Find hands where South will open a weak 1NT and West will likely double
}
