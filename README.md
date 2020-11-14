[![Travis-CI Build Status](https://travis-ci.org/norival/theiaR.svg?branch=master)](https://travis-ci.org/norival/theiaR)
[![AppVeyor build status](https://ci.appveyor.com/api/projects/status/github/norival/theiaR?branch=master&svg=true)](https://ci.appveyor.com/project/norival/theiaR)
[![CRAN status](https://www.r-pkg.org/badges/version/theiaR)](https://cran.r-project.org/package=theiaR)
[![lifecycle](https://img.shields.io/badge/lifecycle-maturing-blue.svg)](https://www.tidyverse.org/lifecycle/#maturing)

# TheiaR: search, download and manage data from Theia

The TheiaR package provides an efficient and clean interface to search, download
and manage products from [Theia website](https://theia.cnes.fr/atdistrib/rocket/#/home).


## Overview

The basic functionalities are (for now):

- Search available tiles through Theia API
- Download tiles resulting from a search
- Download tiles contained in a cart (`.meta4` file) obtained from Theia
  website.
- Read band into `RasterStack` objects (with the `raster` library)
- Read band into `gdalcubes` objects (with the `gdalcubes` library)
- Extract archives (not recommended)

__NOTE:__ To search and download data from Theia, you will need to [register to
their website](https://sso.theia-land.fr/theia/register/register.xhtml).

__NOTE:__ In order to use _Landsat_ or _SpotWorldHeritage_ products, you'll need
to make a first manual download to agree to the license and validate your
account.


## Installation

You can install the latest development version by using:

``` r
devtools::install_github('norival/theiaR')
```

Or, you can install it from CRAN:

``` r
isntall.packages('theiaR')
```


## Complete example

A workflow to search and download tiles would be something like:

``` r
library(theiaR)

# create a list containing the query
myquery <- list(collection = "SENTINEL2",
                town       = "Grenoble",
                start.date = "2018-07-01",
                end.date   = "2018-07-06")

# create a collection from the query
mycollection <- TheiaCollection$new(query = myquery, dir.path = ".", check = TRUE)

# check available tiles fro the query
mycollection$status

# download the tiles into 'dir.path'
mycollection$download(auth = "path/to/auth/file.txt")


```


## Step-by-step guide


### Create a collection of tiles

The first step is to create a collection of tile(s). This can be done either
from a query or from a cart file (downloaded from Theia's website).


#### Create a collection from a query

A query is simply a named `list` of search terms. For example:

``` r
myquery <- list(collection = "SENTINEL2",
                town       = "Grenoble",
                start.date = "2018-07-01",
                end.date   = "2018-07-06")
```

will create a query to Theia database, looking for tiles from Sentinel2
satellite around Grenoble, between 2018-07-01 and 2018-07-31.

See the [vignette](https://norival.dev/) for all the available options.

You can then create your collection with:

``` r
mycollection <- TheiaCollection$new(query = myquery, dir.path = ".", check = TRUE)
```

where `dir.path` is the path you want your tiles to be further downloaded (This
only queries Theia's catalog for available tiles, nothing is downloaded). If
tiles are already present in `dir.path`, they will be checked by computing a
checksum and comparing it to the hash provided by Theia (only available for
Sentinel2 data, no hash is provided for other collections, and files are then
assumed to be correct). This ensures that the files have been correctly
downloaded. Set `check = FALSE` to skip file's check.


#### Create a collection from a cart file

Alternatively, you can download a cart from Theia. To create a cart, login to
Theia website, make a [search](https://theia.cnes.fr/atdistrib/rocket/#/home)
for tiles, and add wanted tiles to your cart. Then, download your cart and save
the resulting `.meta4` file to your disk.

You can then create your collection using this file:

``` r
mycollection <- TheiaCollection$new(cart.path = "path/to/cart/file.meta4",
                                    dir.path  = ".",
                                    check     = TRUE)
```

As above, it will check the hash of files if they are already present in
`dir.path`.


### Getting information on your collection

You can access the tiles from your collection using:

```
mycollection$tiles
```

which returns a `list` of tiles. You can also see the status of your collection
with:

``` r
mycollection$status
```


### Download your tiles

The next step is to download your collection. To download all tiles in a
collection, simply run:

``` r
mycollection$download(auth = "path/to/auth/file.txt")
```

where `path/to/auth/file.txt` is the path to a file storing your Theia
credentials. It is a simple text file with the Theia's account email on the
first line and the account's password on the second line:

``` txt
user@example.com
MyTheiaPassword
```

If it does not exist yet, you will be securely prompted for your login and
password, and the file will be created.

The `download()` memthod will check if files are present, check their hashes,
and download them if needed (if files do not exist or checksums are wrong). To
overwrite existing files, run:

``` r
mycollection$download(auth = "path/to/auth/file.txt", overwrite = TRUE)
```


### Read bands from zip files

You can then read bands directly from the zip archives (by using the `vsizip`
interface provided by GDAL). Use:

``` r
mytile$bands
```

to get a list of available bands. Then:

``` r
mybands <- mytile$read(bands = c("B5", "B6"))
```

to load the bands into memory (returns a `RasterStack` object). It performs the
necessary corrections on the values.

You can also read bands from a collection by running:

``` r
mybands <- mycollection$read(bands = c("B5", "B6"))
```

which returns a `list` of `RasterStack` objects.

_NOTE: Be careful when loading several tiles as it needs a lot of memory (~900MB/tile)_


### Create a `gdalcubes` collection

Alternatively, you can use the great [gdalcubes](https://github.com/appelmar/gdalcubes_R)
package to create a three dimensional representation of the tiles. Simply run:

``` r
library(gdalcubes)

gdalcubes <- mycollection$as_gdalcube("path/to/gdalcubes.sqlite")
```

where `path/to/gdalcubes.sqlite` is the path to store the gdalcubes object data.


### Extract tiles

If you want to extract full archives, you can run:

``` r
file.path <- mycollection$extract()
```

which will extract tiles into the same directory as the archives.

**This is not recommended, as this will take a large amount of disk space**


## Acknowledgments

Thanks to Olivier Hagolle for his work on `theia_download.py`
([github](https://github.com/olivierhagolle/theia_download)), which has inspired
this package.
