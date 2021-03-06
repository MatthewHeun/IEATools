% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utilities.R
\name{extract_TK}
\alias{extract_TK}
\title{Extract temperatures (in Kelvin) from heat types}
\usage{
extract_TK(heat_types, sep = ".")
}
\arguments{
\item{heat_types}{a string vector of heat types to be converted to temperatures}

\item{sep}{the separator between parts of the \code{heat_types} string. Default is ".".}
}
\value{
a numeric vector of same length as \code{heat_types} containing temperatures in Kelvin.
}
\description{
In societal exergy analysis, converting heat to exergy requires knowledge
of the temperature of that heat.
This function converts heat types (e.g., "\code{HTH.600.C}")
to temperatures by extracting the temperature (in Kelvin) from the middle of the string.
}
\details{
It is assumed that the heat type has the following structure:
\itemize{
\item a single letter (typically, "H", "M", or "L" for high, medium, or low, although the character doesn't matter)
\item the string "TH." or "TC." (for "temperature heating" or "temperature cooling"),
\item the temperature value, and
\item unit (one of ".C", ".F", ".R", or ".K", indicating ° Celsius, ° Fahrenheit, rankine, or kelvin, respectively).
}

If \code{heat_type} does not conform to the pattern shown above, \code{NA} is the likely result.
}
\examples{
extract_TK(c("HTH.600.C", "LTH.-20.567.C", "LTH.-40.F", "LTH.-40.C"))
}
