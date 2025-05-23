% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/scale_color_pizza.R
\name{palette_pizza}
\alias{palette_pizza}
\title{Pizza color palette}
\usage{
palette_pizza(palette = "margherita", reverse = FALSE, ...)
}
\arguments{
\item{palette}{Pizza type. Can be \code{"margherita"} (default), \code{"margherita crust"},
\code{"diavola"} or \code{"diavola crust"}.}

\item{reverse}{Boolean indicating whether the palette should be reversed.}

\item{...}{Additional arguments to pass to \code{\link[=colorRampPalette]{colorRampPalette()}}.}
}
\description{
The palette based on authentic neapolitan pizzas.
}
\details{
This function is usually not called directly, but from within
\code{\link[=scale_color_pizza]{scale_color_pizza()}}.
}
