#' Class to represent a theia single tile
#'
#' @slot file.path Path of the tile
#' @slot file.hash md5 hash to check if the tile has been correctly downloaded
#' @slot url URL form which the tile has been downloaded
#' @slot status Status of the tile (whether it is present, checked)
#'
#' @rdname TheiaTile-class

TheiaTile <-
  setClass("TheiaTile",
           slots = c(file.path  = "character",
                     file.hash  = "character",
                     url        = "character",
                     status     = "list"))


#' Class to represent a theia cart
#'
#' @slot cart.file '.meta4' cart file downloaded from Theia
#' @slot cart.infos Informations about the cart
#' @slot tiles A list of tiles
#'
#' @rdname TheiaCart-class

TheiaCart <-
  setClass("TheiaCart",
           slots = c(cart.file  = "character",
                     cart.infos = "list",
                     tiles      = "list"))


# ------------------------------------------------------------------------------
# Constructors
# ------------------------------------------------------------------------------

#' Construct object of class TheiaTile
#'
#' Construct an object of class TheiaTile.
#'
#' @param file.path Path of file
#' @param url URL
#' @param file.hash file hash
#'
#' @rdname TheiaTile
#'
#' @return An object of class \linkS4class{TheiaTile}
#' 
#' @export

TheiaTile <- function(file.path, url, file.hash = NULL)
{
  status <- list(exists  = FALSE, checked = FALSE)

  if (file.exists(file.path)) {
    status$exists <- TRUE
  }

  new("TheiaTile",
      file.path = file.path,
      file.hash = file.hash,
      url       = url,
      status    = status)
}


#' Construct object of class TheiaCart
#'
#' Construct an object of class TheiaCart.
#'
#' @param cart.file '.meta4' cart file downloaded from Theia
#' @param des.dir Directory to store tiles
#'
#' @rdname TheiaCart
#'
#' @return An object of class \linkS4class{TheiaCart}
#' 
#' @export

TheiaCart <- function(cart.file, dest.dir)
{
  dest.dir <- check_dir(dest.dir)

  # parse xml data contained in '.meta4' cart file
  meta.xml <- XML::xmlToList(XML::xmlParse(cart.file))

  pub.date <- as.POSIXct(meta.xml$published, format = "%Y-%m-%dT%H:%M:%S")
  cart.age <- as.numeric(difftime(Sys.time(), pub.date, units = "hours"))

  if (cart.age > 2) {
    warning("Cart too old (>2h), token will be invalidated")
  }

  tiles <- lapply(meta.xml[-1],
                  function(x) {
                    TheiaTile(file.path = paste0(dest.dir, as.character(x$.attrs)),
                              file.hash = as.character(x$hash),
                              url       = as.character(x$url$text))
                  })
  names(tiles) <- rep("tile", length(tiles))

  new("TheiaCart",
      cart.file   = cart.file, 
      tiles       = tiles,
      cart.infos  = list(pub.date   = pub.date,
                         xml.data   = meta.xml))
}
