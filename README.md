# TheiaR: search, download and manage data from Theia

The TheiaR package provides an efficient and clean interface to search, donwload
and manage products from [Theia website](https://theia.cnes.fr/atdistrib/rocket/#/home).


## Overview

The basic functionnalities are (for now):

- Search available tiles through Theia API
- Download tiles resulting from a search
- Download tiles contained in a cart (`.meta4` file) obtained from Theia
  website.
- Read images directly from the downloaded archives (without extracting the
  archives, to save a lot of disk space)
- Extract archives


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

It accepts the following terms. Terms with a `*` are mandatory.

* __collection*__: The collection to look for. Accepted values are: `SENTINEL2`,
  `Landsat`, `SpotWorldHeritage`, `Snow`, `VENUS`.

* __platform__: The platform to look for. Accepted values are: `LANDSAT5`,
  `LANDSAT7`, `LANDSAT8`, `SPOT1`, `SPOT2`, `SPOT3`, `SPOT4`, `SPOT5`,
  `SENTINEL2A`, `SENTINEL2B`, `VENUS`.


To specify the location of the tiles, several alternatives are available.
You can specify the town around which you want your data with:

* __town__: The location to look for. Give a not too frequent town name.


You can specify directly the tile ID if you know it:

* __tile__: The tile identifier to retrieve (_eg_ T31TGK)

You can specify a point by giving its x/y coordinates:

* __latitude__: The x coordinate of a point.

* __longitude__: The y coordinate of a point.


Or you can specify a rectangle by giving its min/max coordinates:

* __latmin__: The minimum latitude to search.

* __latmax__: The maximum latitude to search.

* __lonmin__: The minimum longitude to search.

* __lonmax__: The maximum longitude to search.


Finally, you can filter results by giving the date range and the maximum cloud
cover:

* __max.clouds__: The maximum of cloud cover wanted (0-100).

* __start.date__: The first date to look for (format: `YYYY-MM-DD`).

* __end.date__: The last date to look for (format: `YYYY-MM-DD`).


You can then create your collection with:

```
mycollection <- TheiaCollection$new(query = myquery, dir.path = ".")
```

where `dir.path` is the path you want your tiles to be downloaded. If tiles are
already present in `dir.path`, they will be checked by computing a checksum and
comparing it to the hash provided by Theia (only available for Sentinel2 data,
no hash is provided for other collections, and files are the nassumed to be
correct). This ensures that the files have been correctly downloaded.


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


### Download your tiles

The next step is to download your collection. To download all tiles in a
collection, simply run:

```
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

### Read your tiles

Once you have downloaded every tile (as an archive), you can read images
contained in the archive without extracting it. This allows to save a lot of
disk space.

First, get a list of bands available in the tiles by running:

```{r , eval=FALSE}
mycollection$get_bands()
```

Then you can read bands with:

```{r , eval=FALSE}
images.list <- mycollection$read(bands = c("B2", "B3")
```

where `bands` is a vector with bands names.

It will return a list of `RasterLayer` objects, that you can manipulate with
functions from the `raster` package.


### Extract tiles

If you want to extract full archives, you can run:

```
file.path <- mycollection$extract()
```

which will extract tiles into the same directory as the archives.


## Installation

You can install the latest developpment version by using:

```
devtools::install_github('norival/theiaR')
```


## Acknowledgement

Thanks to Olivier Hagolle for his work on `theia_download.py`
([github](https://theia.cnes.fr/atdistrib/rocket/#/home)), which has inspired
this package.
