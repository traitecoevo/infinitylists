# Generate galah query for Global GBIF Node

Generate galah query for Global GBIF Node

## Usage

``` r
query_gbif_global(taxon, min_year, max_year, country_code = NULL)
```

## Arguments

- taxon:

  character, genus/family/kingdom

- min_year:

  numeric, year cut off for query, only records where year \>= min_year
  will be included

- max_year:

  numeric, year cut off for query, only records where year \<= max_year
  will be included

- country_code:

  character, code for country
