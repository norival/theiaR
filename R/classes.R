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
#'
#' @rdname TheiaCart-class

TheiaCart <-
  setClass("TheiaCart",
           slots = c(cart.file  = "character",
                     cart.infos = "list"))


#' Class to represent a theia collection
#'
#' @slot from Either a \code{character} giving the path to a cart file
#' ('.meta4' file) or an object of class \linkS4class{TheiaQuery}
#' @slot cart.infos Informations about the cart
#' @slot dest.dir Directory to store tiles
#'
#' @rdname TheiaCollection-class

TheiaCollection <-
  setClass("TheiaCollection",
           slots = c(from  = "ANY",
                     tiles = "list"))


#' Class to represent a theia query
#'
#' @slot credentials A list with login and password
#' @slot query A list of search terms. See Details.
#'
#' @rdname TheiaQuery-class

TheiaQuery <-
  setClass("TheiaQuery",
           slots = c(credentials  = "list",
                     query        = "list"))


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
#' @param dest.dir Directory to store tiles
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

  new("TheiaCart",
      cart.file   = cart.file, 
      cart.infos  = list(pub.date   = pub.date,
                         xml.data   = meta.xml))
}


#' Create a \code{TheiaCollection} object
#'
#' Create a \code{TheiaCollection} object from a cart file or a
#' \linkS4class{TheiaQuery} object (not implemented yet)
#'
#' @param from Either a \code{character} giving the path to a cart file
#' ('.meta4' file) or an object of class \linkS4class{TheiaQuery}
#' @param dest.dir Directory to store tiles
#'
#' @export

TheiaCollection <- function(from, dest.dir)
{
  # check arguments
  if (!(class(from) %in% c("character", "TheiaQuery"))) {
    stop("'from', must be either of class 'character' or 'TheiaQuery'")
  }

  # check dest.dir
  dest.dir <- check_dir(dest.dir)

  if (is.character(from)) {
    # create a 'TheiaCart' object
    origin <- TheiaCart(from, dest.dir)

    # initialize tiles
    tiles <- lapply(origin@cart.infos$xml.data[-1],
                    function(x) {
                      TheiaTile(file.path = paste0(dest.dir, as.character(x$.attrs)),
                                file.hash = as.character(x$hash),
                                url       = as.character(x$url$text))
                    })
    names(tiles) <- rep("tile", length(tiles))

  } else {
    ## NOT IMPLEMENTED YET #####################################################
    # reads TheiaQuery and initialize tiles objects
    ## NOT IMPLEMENTED YET #####################################################
    origin <- from
    tiles  <- list()
    warning("Not implemented yet for TheiaQuery objects")
  }

  # create new collection of tiles
  new("TheiaCollection",
      from  = origin,
      tiles = tiles)
}
