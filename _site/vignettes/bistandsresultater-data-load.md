# Preparing a data load for bistandsresultater.no


# Overview

This vignette describes how to prepare the main data files for a data
load to bistandsresultater.no.

The data load is based on three main datasets:

1.  Official aid statistics from Statsys
2.  Agreement totals from PTA
3.  Disbursement-level data from PTA

The workflow is:

**PTA / Statsys → CSV → DuckDB → `noradstats` → processed and anonymised
Excel files**

The raw extracts from PTA and Statsys are loaded into the DuckDB
database used by `noradstats`. The final files are then generated from
the database using functions in the package.

In addition to the three main files, a data load may include
supplementary register tables and supporting datasets. These normally
change less frequently and should mainly be reviewed when a new
statistical year is first published, usually in May. Later data loads
during the year are normally revisions of the same statistical year.

# 1. Create a folder for the data load

Create a new folder under:

`Norad-Avd-Kunnskap - Statistikk og analyse/10. Formidling - ekstern og intern/06. Bistandsresultater/`

Use the previous data load as a template for the folder and file
structure.

Create a `data_raw` subfolder for the source files downloaded from PTA
and Statsys.

# 2. Download Agreement totals from PTA

In PTA, select the report **Agreement totals**.

## Advanced criteria

- Phase: B, C, D
- Signed after: 01.01.1960
- Programme area: 03, 12

## Layout

- Remove **Responsible unit**
- Add **Public use**
- Flexi column: **Agreement period**

Click **View** and export the report to Excel.

Open the Excel file and save it as a CSV UTF-8 file without changing the
file name.

Save the CSV file in the `data_raw` folder for the current data load.

# 3. Download Disbursement level from PTA

In PTA, select the report **Disbursement level**.

## Advanced criteria

- Year: the year after the latest official statistical year in Statsys
- Agreement phase: B, C, D
- Programme area: 03, 12
- Disbursement status: A

For example, if 2025 is the latest official statistical year, select
2026.

## Layout

Include:

- Statistics
- Agreement description
- For public use
- Show miscellaneous income
- Show AIGO

Click **View** and export the report to Excel.

Open the Excel file and save it as a CSV UTF-8 file without changing the
file name.

Save the CSV file in the `data_raw` folder.

# 4. Extract the latest official Statsys data

Open Statsys and run the SQL query used to extract the latest official
statistics.

Update the final year in the query to the latest official statistical
year.

For example:

``` sql
select *
from [vOppsummering_2026 07 DAC/CRS-rapportering]
where [Year] between 1960 and 2025
```

Export the result as CSV and save it in the `data_raw` folder.

# 5. Load the source files into DuckDB

Load the PTA Agreement totals file:

``` r
noradstats::create_pta_agreement_totals_to_db(
  "path/to/Agreement totals.csv"
)
```

Load the PTA Disbursement level file:

``` r
noradstats::create_pta_disbursement_level_to_db(
  "path/to/Disbursement level.csv"
)
```

Load the latest official Statsys extract:

``` r
noradstats::create_statsys_data_to_db(
  "path/to/Statsys_extract.csv",
  version = "statsys_official"
)
```

These functions update the corresponding tables in the DuckDB database
used by `noradstats`.

After this step, the source data needed for the data load are available
through DuckDB and do not need to be read directly from the CSV files
during the remaining processing.

# 6. Generate the final files

Use `export_aidresults_xlsx()` to produce the three main files for
bistandsresultater.no.

``` r
filepath <- "path/to/where/you/want/the/files"

noradstats::export_aidresults_xlsx(
  path_dir = filepath,
  lang = "en"
)
```

This creates:

- `pta_agreements_total.xlsx`
- `pta_disbursement_level_BCD_actual.xlsx`
- `statsys_aktiv.xlsx`

The function retrieves the relevant data from DuckDB, applies the
required processing and anonymisation rules, and writes the final Excel
files to the selected folder.

Check that all three files have been created successfully.

# 7. Check supplementary files

The upload folder may also contain supplementary datasets and register
tables, for example:

- imputed multilateral data
- Norfund-related data
- bridge and register tables

These normally do not need to be updated for every data load.

As a general rule, they should be reviewed when a new statistical year
is first published, usually in May. Later data loads during the year are
primarily revisions of the already published statistical year.

Update supplementary files outside the annual publication cycle only
when there has been a relevant change.

# 8. Prepare the upload folder

Place the three generated Excel files and any required supplementary
files in the data load folder.

Compare the contents with the previous data load to make sure that all
required files are included.

# 9. Share the files for upload

Share the completed data load folder with Sopra Steria through
OneDrive/SharePoint for upload to bistandsresultater.no.
