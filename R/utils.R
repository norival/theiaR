# ------------------------------------------------------------------------------
# R/utils.R
# 
# Various utilities functions
# ------------------------------------------------------------------------------


# ------------------------------------------------------------------------------
# Check directory name, add trailing '/' if needed, and create it if needed.
# Returns the directory name.

check_dir <- function(dir.name)
{
  # add trailing '/' to dest.dir if needed
  if (!(grepl("/$", dir.name))) {
    dir.name <- paste0(dir.name, "/")
  }

  # create dest.dir if needed
  if (!(dir.exists(dir.name))) {
    dir.create(dir.name, recursive = TRUE)
    message(dir.name, "does not exist. Creating...")
  }

  return(dir.name)
}

read_tiff_from_zip <- function(file.name, zip.file)
{
  # Code encapsulated in a tryCatch because it might fail for some tiles due to
  # zip files seen as corrupted. It is bypassed by using system's 'unzip'
  # program to unzip file. But it cannot be specified in the 'unz' function.

  tile.tiff <-
    tryCatch({
      # open zip connection
      con <- unz(zip.file, file.name, open = "rb")

      # read connection into a raw vector
      file.raw <- readBin(con, "raw", n = 1e10)

      # read raw vector with readTIFF()
      tiff.tmp <- tiff::readTIFF(file.raw, as.is = TRUE)

      # close connection
      close(con)

      tiff.tmp
    },
    error = function(e) {
      # Executed if code has failed: the file is extracted in a temporary
      # directory and then read from it
      tmp.dir <- paste0(tempdir(), "/")
      unzip(zip.file, files = file.name, exdir = tmp.dir, unzip = getOption("unzip"))

      # read tiff file
      tiff.tmp <- tiff::readTIFF(paste0(tmp.dir, file.name), as.is = TRUE)

      # remove temporary file
      unlink(paste0(tmp.dir, file.name))

      tiff.tmp
    })

  # remove NA values: -10000
  tile.tiff[tile.tiff == -10000] <- NA

  # get reflectance value: divide by 10000
  tile.tiff <- tile.tiff / 10000

  # convert to raster::raster object
  return(raster::raster(tile.tiff))
}


# wrapper to extract files from different archive formats
extraction_wrapper <- function(path, args)
{
  # build arguments to function call
  args <- lapply(args, function(x)
                 if (is.character(x)) {
                   paste0("'", x, "'")
                 } else {
                   x
                 })

  .args <- paste(names(args), args, sep = " = ", collapse = ", ")
  .args <- paste0("'", path, "', ", .args)

  # build function call
  .call <- ifelse(grepl("tar.gz$", path),
                  paste0("untar(", .args, ")"),
                  paste0("unzip(", .args, ", unzip = getOption('unzip'))"))

  # eval function call
  result <- eval(parse(text = .call))

  # return value
  if (is.null(args$list) || args$list == FALSE) {
    return(invisible(result))
  } else {
    # return only a vector of file names
    if (grepl("^untar", .call)) {
      return(result)
    }

    if (grepl("^unzip", .call)) {
      return(result$Name)
    }
  }
}


parse_query <- function(q, q.name, type, choices = NULL)
{
  # if NULL, exit function
  if (is.null(q)) {
    return(NULL)
  }

  # if date, try to convert to a date
  if (grepl("date", q.name)) {
    q <- as.character(as.Date(q, format = "%Y-%m-%d"))

    if (is.na(q)) {
      stop("Invalid query: date. Date format: YYYY-MM-DD",
           call. = FALSE)
    }
  }

  # try to convert to needed mode
  tryCatch(mode(q) <- type,
           warning = function(w) {
             stop("Invalid query: ", q, ". ",
                  "Should be of type: ", type,
                  call. = F)
           },
           error = function(e) {
             stop("Invalid query: ", q, ". ",
                  "Should be of type: ", type,
                  call. = F)
           })

  # if clouds, remove digits
  if (grepl("clouds", q.name)) {
    q <- trunc(q)
  }

  # check if value is in available choices
  if (!(missing(choices)) && !(q %in% choices)) {
    stop("Invalid query: ", q.name, ". ",
         "Should be one of: ",
         paste0(choices, collapse = ", "),
         call. = F)
  }

  return(q)
}
