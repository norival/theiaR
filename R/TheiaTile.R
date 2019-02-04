#' A tile from Theia
#'
#' Generate and manage a tile from Theia (download, check, load).
#'
#' @name TheiaTile
#'
#' @section Usage:
#' \preformatted{
#'    t <- TheiaTile$new(file.path,
#'                       url,
#'                       file.hash)
#'
#'    t$download(overwrite = FALSE)
#'    t$check()
#'    t$get_bands()
#'    t$read(bands)
#' }
#'
#' @section Arguments:
#'
#' \describe{
#'    \item{t:}{A \code{TheiaTile} object}
#'    \item{file.path:}{The path to the zip file containing the tile}
#'    \item{url:}{The url to download the tile}
#'    \item{file.hash:}{The md5sum used to check the zip file}
#'    \item{auth:}{A `TheiaAuth` object, for identication to Theia website}
#'    \item{overwrite:}{Overwrite existing tiles (default to `FALSE`)}
#'  }
#'
#' @section Details:
#'    \code{TheiaTile$new(file.path, url, file.hash)} Create a new instance of
#'    the class
#'
#'    \code{t$download(auth, overwrite = FALSE)} Download the tiles of the collection
#'    and check the resulting files
#'
#'    \code{t$check()} Check the tiles of the collection
#'    \code{t$get_bands()} List bands available in the tile
#'    \code{t$read(bands)} Read band(s) from the zip file
#'
NULL


#' @export

TheiaTile <-
  R6Class("TheiaTile",
          # public -------------------------------------------------------------
          private =
            list(meta.data = NULL,

                 add_md = function()
                 {
                   # parse and add metadata from the zip archive
                   .TheiaTile_add_md(self, private)
                 }),

          # public -------------------------------------------------------------
          public =
            list(file.path  = NA,
                 file.hash  = NA,
                 url        = NA,
                 status     = list(exists  = FALSE,
                                   checked = FALSE,
                                   correct = FALSE),

                 initialize = function(file.path, url, file.hash)
                 {
                   .TheiaTile_initialize(self, private, file.path, url, file.hash)
                 },

                 print = function(...)
                 {
                   .TheiaTile_print(self)
                 },

                 check = function()
                 {
                   .TheiaTile_check(self)
                 },

                 download = function(auth, overwrite = FALSE)
                 {
                   .TheiaTile_download(self, private, auth, overwrite)
                 },
                  
                 get_bands = function()
                 {
                   .TheiaTile_get_bands(self, private)
                 },

                 read = function(bands)
                 {
                   .TheiaTile_read(self, private, bands)
                 })
          )


# Functions definitions --------------------------------------------------------

.TheiaTile_print <- function(self)
{
  # Special method to print
  # TODO: better method to print
  cat("An Tile from Theia\n")

  return(invisible(self))
}


.TheiaTile_initialize <- function(self, private, file.path, url, file.hash)
{
  # Fill fiedls of the object
  self$file.path <- file.path
  self$url       <- url
  self$file.hash <- file.hash

  # check the tile
  self$check()

  # adds meta data if file is present and correct
  if (self$status$correct == TRUE) {
    private$add_md()
  }

  return(invisible(self))
}


.TheiaTile_check <- function(self)
{
  # check the tile
  if (file.exists(self$file.path)) {
    # if the file exists, check it
    message("Checking downloaded file...")

    self$status$exists  <- TRUE
    self$status$checked <- TRUE

    if (is.na(self$file.hash)) {
      # if no hash is provided, assume the file is correct
      self$status$correct <- TRUE

      return(invisible(self))
    }

    # compute the md5 sum and compare it to the hash
    if (tools::md5sum(self$file.path) == self$file.hash) {
      self$status$correct <- TRUE
    } else {
      self$status$correct <- FALSE
    }
  }

  return(invisible(self))
}


.TheiaTile_download <- function(self, private, auth, overwrite = FALSE)
{
  if (!(self$status$correct) | overwrite == TRUE ) {
    # file does not exist, is not correct, or overwrite is TRUE

    # build the URL for the request: remove token if link has been created from
    # a cart file and add needed part
    url <- gsub("\\?_tk=.*$", "", self$url)
    url <- paste0(url, "/?issuerId=theia")

    # HTTP request
    req <- httr::GET(url,
                     add_headers(Authorization = paste("Bearer", auth$token)),
                     write_disk(self$file.path, overwrite = TRUE),
                     progress())
  } else {
    # The file already exists
    message("File ",
            self$file.path,
            " already exists. Use 'overwrite=TRUE' to ovewrite.")
  }

  # check the tile
  self$check()

  # adds meta data if file is present and correct
  if (self$status$correct == TRUE) {
    private$add_md()
  }

  return(invisible(self))
}


.TheiaTile_add_md <- function(self, private)
{
  message("Parsing meta data...")

  # create temporary directory
  tmp.dir <- paste0(tempdir(), "/")

  # get file name to extract
  file.name <- extraction_wrapper(self$file.path, args = list(list = TRUE))
  file.name <- file.name[grepl("xml$", file.name)]

  # extract and parse xml file
  extraction_wrapper(self$file.path, args = list(files = file.name, exdir = tmp.dir))
  private$meta.data <- xmlToList(xmlParse(paste0(tmp.dir, file.name)))

  # remove temporary file
  unlink(paste(tmp.dir, file.name, sep = "/"))

  return(invisible(self))
}


.TheiaTile_get_bands <- function(self, private)
{
  # get bands list from 
  bands <- lapply(private$meta.data$Product_Characteristics$Band_Group_List,
                  function(x) {
                    band.list <- unlist(x$Band_List[-(length(x$Band_List))])
                    band.id   <- unname(x$.attrs)

                    data.frame(band = band.list, band.id = band.id)
                  })

  bands <- do.call(rbind, bands)
  rownames(bands) <- NULL

  return(bands)
}


.TheiaTile_read <- function(self, private, bands)
{
  # check if requested bands are available
  avail.bands <- self$get_bands()
  if (any(!(bands %in% avail.bands$band))) {
    stop("Bands '",
         paste(bands[!(bands %in% avail.bands)], collapse = ", "),
         "' are not available!")
  }

  # check if requested bands hae the same ID
  bands.id <- avail.bands$band.id[avail.bands$band %in% bands]
  if (length(unique(bands.id)) > 1) {
    stop("Bands '",
         paste(bands[!(bands %in% avail.bands)], collapse = ", "),
         "' are not at the same resolution!")
  }

  # get file names to read from
  files   <- unzip(self$file.path, list = TRUE)$Name
  pattern <- paste(paste0("FRE_", bands, ".tif$"), collapse = "|")
  files   <- files[grepl(pattern, files)]

  # read tiles from zip file and create raster::rasterStack object
  raster::stack(lapply(files, read_tiff_from_zip, zip.file = self$file.path))
}
