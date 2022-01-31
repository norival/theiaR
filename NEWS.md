# theiaR 0.4.1

## Bug fixes

- Fix bad default processing level for some collections
- Fix cran checks note


# theiaR 0.4.0

## Minor changes

- Adds error when end.date is lower or equal to start.date

## Bug fixes

- Fix query metadata reading for Landsat57 format
- Fix query URL and level compatibility for Landsat57 collection
- Update README, vignette and documentation
- Fix error for new landsat metadata format


# theiaR 0.3.0

## New features

- Adds export to gdalcubes' image collection

## Bug fixes

- Fix error due to bad file path on Windows

## Minor changes

- Adds quiet arguments to control verbose output
- Adds 'check' argument to 'TheiaTile' and 'TheiaCollection' download methods
- Do not parse meta.data on tile creation, but do it on the fly


# theiaR 0.2.1

## Bug fixes

- Fix printing of TheiaQuery objects


# theiaR 0.2.0

## New features

- Adds orbit number and relative orbit number to queries
- Adds max records to queries
- Adds reading of bands from zip files
- Adds no check option for creation of Tiles


# theiaR 0.1.2

## Bug fixes

- Fix status dataframe for TheiaCollection objects


# theiaR 0.1.1

## Minor changes

- Adds processing level search term to queries
- Adds default values for collection and level to queries

## Bug fixes

- Fix metadata file detection


# theiaR 0.1.0

## Major changes

- First package release
