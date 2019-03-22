[![Travis-CI Build Status](https://travis-ci.org/norival/theiaR.svg?branch=master)](https://travis-ci.org/norival/theiaR)
[![AppVeyor build status](https://ci.appveyor.com/api/projects/status/github/norival/theiaR?branch=master&svg=true)](https://ci.appveyor.com/project/norival/theiaR)
[![Coverage status](https://codecov.io/gh/norival/theiaR/branch/master/graph/badge.svg)](https://codecov.io/github/norival/theiaR?branch=master)
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
- Extract archives


_NOTE: ongoing development, more functionalities shall be added in the future_


## Installation

You can install the latest development version by using:

```
devtools::install_github('norival/theiaR')
```


## Step-by-step guide

First, load the package.

```
library(theiaR)
```

To search and download data from Theia, you will need to [register to their
website](https://sso.theia-land.fr/theia/register/register.xhtml).

__NOTE:__ In order to use _Landsat_ or _SpotWorldHeritage_ products, you'll need
to make a first manual download to agree to the license and validate your
account.


### Create a collection of tiles

The first step is to create a collection of tile(s). This can be done either
from a query or from a cart file.


#### Create a collection from a query

A query is simply a named `list` of search terms. For example:

```
myquery <- list(collection  = "SENTINEL2",
                town        = "Grenoble",
                start.date  = "2018-07-01",
                end.date    = "2018-07-06")
```

will create a query to Theia database, looking for tiles from Sentinel2
satellite around Grenoble, between 2018-07-01 and 2018-07-31.

It accepts the following terms.

* __collection__: The collection to look for. Accepted values are: `SENTINEL2`,
  `Landsat`, `SpotWorldHeritage`, `Snow`. Defaults to `SENTINEL2`.

* __platform__: The platform to look for. Accepted values are: `LANDSAT5`,
  `LANDSAT7`, `LANDSAT8`, `SPOT1`, `SPOT2`, `SPOT3`, `SPOT4`, `SPOT5`,
  `SENTINEL2A`, `SENTINEL2B`.

* __level__: Processing level of products. Accepted values are: `LEVEL1C`,
  `LEVEL2A` and `LEVEL3A`. Defaults to `'LEVEL2A`.


To specify the location of the tiles, several alternatives are available.
You can specify the town around which you want your data with:

* __town__: The location to look for. Give a not too frequent town name.


You can specify directly the tile ID if you know it:

* __tile__: The tile identifier to retrieve (_e.g._ T31TGK)

You can specify a point by giving its x/y coordinates:

* __latitude__: The x coordinate of a point.

* __longitude__: The y coordinate of a point.


Or you can specify a rectangle by giving its min/max coordinates:

* __latmin__: The minimum latitude to search.

* __latmax__: The maximum latitude to search.

* __lonmin__: The minimum longitude to search.

* __lonmax__: The maximum longitude to search.


You can also look for a specific orbit number or relative orbit number:

* __orbit.number__: The orbit number

* __rel.orbit.number__: The relative orbit number


Finally, you can filter results by giving the date range, the maximum cloud
cover and the maximum of records:

* __max.clouds__: The maximum of cloud cover wanted (0-100).

* __start.date__: The first date to look for (format: `YYYY-MM-DD`).

* __end.date__: The last date to look for (format: `YYYY-MM-DD`).

* __max.records__: The maximum of tiles to search


You can then create your collection with:

```
mycollection <- TheiaCollection$new(query = myquery, dir.path = ".")
```

where `dir.path` is the path you want your tiles to be further downloaded. If
tiles are already present in `dir.path`, they will be checked by computing a
checksum and comparing it to the hash provided by Theia (only available for
Sentinel2 data, no hash is provided for other collections, and files are then
assumed to be correct). This ensures that the files have been correctly
downloaded.


#### Create a collection from a cart file

Alternatively, you can download a cart from Theia. To create a cart, login to
Theia website, make a [search](https://theia.cnes.fr/atdistrib/rocket/#/home)
for tiles, and add wanted tiles to your cart. Then, download your cart and save
the resulting `.meta4` file to your disk.

You can then create your collection using this file:

```
cart.path <- system.file("extdata", "cart.meta4", package = "theiaR")

mycollection <- TheiaCollection$new(cart.path = cart.path,
                                    dir.path  = ".")
```

As above, it will check the hash of files if they are already present in
`dir.path`.


#### Getting information on your collection

You can access the tiles from your collection using:

```
mycollection$tiles
```

which returns a `list` of tiles. You can also see the status of your collection
with:

```
mycollection$status
```


### Download your tiles

The next step is to download your collection. To download all tiles in a
collection, simply run:

```
myauth <- "path/to/auth/file.txt"

mycollection$download(auth = myauth)
```

where myauth is the path to file storing your Theia credentials. If it does not
exist yet, you will be securely prompted for your login and password, and the
file will be created.

This will check if files are present, check their hashes, and download them if
needed (if files do not exist or checksums are wrong). To overwrite existing
files, run:

```
mycollection$download(auth = myauth, overwrite = TRUE)
```


### Extract tiles

If you want to extract full archives, you can run:

```
file.path <- mycollection$extract()
```

which will extract tiles into the same directory as the archives.


## Acknowledgment

Thanks to Olivier Hagolle for his work on `theia_download.py`
([github](https://github.com/olivierhagolle/theia_download)), which has inspired
this package.
