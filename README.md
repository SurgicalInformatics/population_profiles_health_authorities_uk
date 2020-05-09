COVID19 Clinical Information Network (CO-CIN) / ISARIC-4C population profile to hospital mapping
==========

* [ISARIC COVID-19 Clincial Research Resources](https://isaric.tghn.org/covid-19-clinical-research-resources/)
* [ISARIC4C - Coronavirus Clinical Characterisation Consortium](https://isaric4c.net/)

These are scripts to pull and prepare data from the above REDCap database. This is an active project and scripts will change, so please always update to the latest version.

## Caution
### Data security

These are patient-level data that contain disclosive information. Only use in a secure environment and do not hold data on a removable device including laptops. 

### Always check the data

It is the end-users responsibility to understand the processes contained in these scripts, the assumptions that are used, and to check the data created conforms to their expectations. 

### Set environment variable with REDCap API token

Do not store the REDCap API token as plain text.

``` r
usethis::edit_r_environ()
# this opens up .Renviron, add your token, e.g. ccp_token = 2F3xxxxxxxxxxxxE0111
# Restart R
```

### `01_generate_ethnicities_mapping.R`

**Description**: Attaches ethnicity/ deprivation or other population level estimates from publicly available data.

Data can either be supplied from the CCP location lookup GitHub or from a user supplied CSV. Default is the CCP location lookup GitHub CSV.

This script outputs files to the **data_out_ccp_lookup_with_population_level_estimate** folder. The lite file provides only essential deprivation information. The full file provides the ethnicity estimates and full deprivation level estimates within CCG, hospital catchment or health board.
