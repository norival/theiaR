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
  # generate filename uzing vsizip interface provided by GDAL
  file.name <- paste0("/vsizip/", zip.file, "/", file.name)

  # load the raster
  ras <- raster::raster(file.name)

  return(ras)
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


parse_query <- function(q, q.name, type, default = NULL, choices = NULL)
{
  # if NULL, exit function or return default value
  if (is.null(q)) {
    if (!(is.null(default))) {
      # return default value
      mode(default) <- type
      return(default)
    } else {
      return(NULL)
    }
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


# correction to sentinel tiles
correct_values <- function(ras)
{
  ras[ras == -10000] <- NA
  ras <- ras / 10000

  return(ras)
}
