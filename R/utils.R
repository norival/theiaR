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
