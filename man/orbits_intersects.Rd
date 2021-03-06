% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fct_orbits_intersects.R
\name{orbits_intersects}
\alias{orbits_intersects}
\title{Select the orbit intersecting the extent}
\usage{
orbits_intersects(tiles, all = FALSE, out_format = "id", .s2orbits = NULL)
}
\arguments{
\item{tiles}{`sf` object with the tiles spatial extent.}

\item{all}{logical: if TRUE, all the orbits overlapping the extent are
provided;
if FALSE (default), unnecessary orbits are skipped.
Unnecessary orbits are orbits which overlaps the extent for an area already
covered by another orbit.
In case the extent is all included in an overlapping area, only one of the
two candidate orbits is returned (the first in alphabetical order).}

\item{out_format}{character: if "sf", the spatial object of the overlapping orbits
is returned; if "id" (default), a character vector with the orbit IDs.}

\item{.s2orbits}{output of [s2_orbits()] function (it is possible to pass it
in order to speed up the execution;
otherwise leave to NULL and it will be generated within the function).}
}
\value{
the orbits intersecting the extent (see argument `out_format`).
}
\description{
Function which returns the orbit IDs of the Sentinel-2 orbits
 which overlap a provided extent.
}
\note{
License: GPL 3.0
}
\author{
Pascal Obstetar (2020) \email{pascal.obstetar@gmail.com}
}
