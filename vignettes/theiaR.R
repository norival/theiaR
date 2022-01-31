## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ---- eval=FALSE--------------------------------------------------------------
#  devtools::install_github('norival/theiaR')
#  
#  # or, to install the development version
#  devtools::install_github('norival/theiaR', 'devel')

## ---- eval=FALSE--------------------------------------------------------------
#  isntall.packages('theiaR')

## ---- eval=FALSE--------------------------------------------------------------
#  library(theiaR)
#  
#  # create a list containing the query
#  myquery <- list(collection = "SENTINEL2",
#                  town       = "Grenoble",
#                  start.date = "2018-07-01",
#                  end.date   = "2018-07-06")
#  
#  # create a collection from the query
#  mycollection <- TheiaCollection$new(query = myquery, dir.path = ".", check = TRUE)
#  
#  # check available tiles fro the query
#  mycollection$status
#  
#  # download the tiles into 'dir.path'
#  mycollection$download(auth = "path/to/auth/file.txt")

## ---- eval=TRUE---------------------------------------------------------------
library(theiaR)

## ---- eval=T------------------------------------------------------------------
myquery <- list(collection = "SENTINEL2",
                town       = "Grenoble",
                start.date = "2018-07-01",
                end.date   = "2018-07-06")

## ---- eval=FALSE--------------------------------------------------------------
#  mycollection <- TheiaCollection$new(query = myquery, dir.path = ".", check = TRUE)

## ---- eval=FALSE--------------------------------------------------------------
#  print(mycollection)

