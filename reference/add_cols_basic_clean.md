# Add basic columns to data frame of Norwegian development assistance

Add basic columns to data frame of Norwegian development assistance

## Usage

``` r
add_cols_basic_clean(data)
```

## Arguments

- data:

  Input dataframe of Norwegian development assistance in snake_case
  column names

## Value

Returns dataframe with additional columns:

- target_area_no: Norwegian translation of column *Target area*.

- partner_group_visual_no: Visual grouping and Norwegian translation of
  column *Group of agreement partner*.

- partner_group_visual: Visual grouping of column *Group of agreement
  partner*.

- main_region_no: Norwegian translation of column *Main region*.

- income_category_no: Norwegain translation of column *Income category*.

- earmarked: Earmarked aid (bilateral, earmarked to multilaterals,
  triangular co-operation)

- countryspecified: Country specific aid (income category not
  unspecified)

- geographically_specified: Geographically specific aid (main region not
  unspecified)

- earmarked_subsaharan: Earmarked aid to Sub-Saharan Africa

- gross_disbursed_nok: Gross disbursed amount in Norwegian kroner.
  Converts negative amounts extended to zero.

- disbursed: Disbursed amount in US dollars. Based on column
  disbursed_1000.

## Examples

``` r
?add_cols_basic_clean()
```
