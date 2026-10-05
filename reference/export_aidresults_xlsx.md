# Export aidresults datasets to Excel files

Builds all aidresults datasets and writes them to Excel files in a
directory. This is the main entry point for publishing the current
aidresults files.

## Usage

``` r
export_aidresults_xlsx(path_dir, lang = "en")
```

## Arguments

- path_dir:

  Output directory.

- lang:

  Language for official column names. Defaults to "en".

## Value

Invisibly returns the built datasets.
