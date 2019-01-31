% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/TheiaCollection.R
\name{TheiaCollection}
\alias{TheiaCollection}
\title{A collection of tiles from Theia}
\description{
Generate and manage collection of tiles from Theia. This collection can be
created either from a cart file ('.meta4') downloaded from Theia website,
from a \code{\link{TheiaQuery}} object or from a list of
\code{\link{TheiaTile}} (not implemented yet).
}
\section{Usage}{

\preformatted{
   c <- TheiaCollection$new(cart.path = NULL,
                            tiles     = NULL,
                            query     = NULL,
                            dir.path  = NULL)

   c$download(override = FALSE)
   c$check()
}
}

\section{Arguments}{


\describe{
   \item{c:}{A \code{TheiaCollection} object}
   \item{dir.path:}{The path to the directory containing zip files}
   \item{tiles:}{A list of TheiaTile objects}
   \item{cart:}{An XML cart parsed from a 'meta4' file downloaded from Theia}
   website. Used only if Collection is created from a cart
   \item{query:}{A TheiaQuery object, used only if collection is created
   from a TheiaQuery object}
   \item{override:}{Override existing tiles (default to `FALSE`}
 }
}

\section{Details}{

   \code{$new()} Create a new instance of the class

   \code{$download(override = FALSE)} Download the tiles of the collection
   and check the resulting files

   \code{$check()} Check the tiles of the collection
}
