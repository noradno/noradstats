# Read Statsys data into R from user-specified CSV file

This function reads a Statsys CSV file, which should be delimited by
semicolons (;) and have commas (,) as decimal marks. It expects the CSV
file to be at the specified path and returns a tibble containing the
data. Make sure the path is correctly specified to avoid errors. The
function checks for valid path input before attempting to read the file.

## Usage

``` r
read_statsys_from_csv(path)
```

## Arguments

- path:

  Required path to CSV file of Statsys data.

## Value

Returns a tibble of Statsys data.

## Examples

``` r
df_statsys <- read_statsys_from_csv("path/to/your/statsys_file.csv")
#> Error in read_statsys_from_csv("path/to/your/statsys_file.csv"): File does not exist at the specified path: path/to/your/statsys_file.csv
```
