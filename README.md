R code to extract information about statistics-related conferences.

__Work in progress.__

## DSSV [2019](https://iasc-isi.org/dssv2019/)

- `panel` — panel in which the paper was presented
- `title` — paper title
- `affiliation` — university/organisation with which the author is affiliated
- `country` — country of affiliation

Notes:

- `author` is missing first name a handful of cases
- `author` has not been fully checked for consistency

The `affiliation` column has been geocoded using the [geocode.xyz](https://geocode.xyz/api) API (thanks!).

## Compstat [2016](http://www.compstat2016.org/) and [2018](http://www.compstat2018.org/)

- `year` — year of conference (2016 or 2018)
- `uid` — paper unique identifier (integer)
- `title` — paper title
- `author` — author (first and family) name
- `presenter` — whether the author presented the paper (0/1)
- `country` — country of affiliation
- `affiliation` — university/organisation with which the author is affiliated

Notes:

- `author` has not been fully checked for consistency
- `presenter` is equal to 0 for all authors in a handful of cases.
- `affiliation` is missing a few acronyms present in the original data

Data not geocoded yet.
