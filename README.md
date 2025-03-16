# PEARL

Repo for the PEARL project

# Quick Start

1.  Install R, RStudio and `renv`

2.  Fork and clone this repo using the recommended method here:
    <https://happygitwithr.com/fork-and-clone>

3.  Initialize the environment using `renv::restore()`

4.  Store REDCap API keys in `.Renviron`

5.  Retrieve or export raw data from REDCap

6.  Use scripts to load, tidy and analyse data

# About PEARL

The PEARL+ Project is an ambitious, systematic mass screening, treatment
and prevention programme primarily focused on TB. In South Tarawa there
are extremely high levels of TB transmission in the community, and the
project aims to dramatically reduce this by visiting every household in
this urban area. In addition to TB, we are also checking for leprosy,
scabies and hepatitis B, making PEARL+ an integrated, community-based
intervention. Along with the flagship mass screening initiative, PEARL+
will also provide crucial support for strategic planning and
strengthening of existing disease programs.

The PEARL+ project is an Australian aid initiative implemented by the
University of Sydney on behalf of the Australian Government, and
delivered by a team of Kiribati national health professionals in
partnership with the Ministry of Health and Medical Services.

# PEARL data

Data is owned jointly by University of Sydney and Kiribati Ministry of
Health and Medical Services, according to a data sharing agreement
agreed by both parties.

Data is captured in REDCap, which is hosted by the University of Sydney.
<https://redcap.sydney.edu.au/>

There are three REDCap projects for primary data capture:

1.  Household enumeration project, for capturing household level data

2.  Screening project, for capturing individual level screening data

3.  Treatment project, for capturing individual level TB preventive
    treatment data

Additional REDCap projects are in use for aggregation, project
monitoring and other functions.

# Data pipeline

1.  Data capture in REDCap mobile app
2.  Upload to REDCap server at University of Sydney
3.  Access and operational usage at project level, primarily using
    REDCap reports
4.  Data available for manual analysis and reporting, through REDCap
    data export
5.  Data available for automated analysis and reporting, through REDCap
    APIs
6.  Analysis pipeline coded in R (this repo)

# Usage of this repo

In general, scripts can be run in order. Scripts are named and numbered
according to the schema:

01 - retrieve data using API key

02 - load data from raw data folders

03 - tidy, clean and prepare data for analysis

04 - helper scripts for specific use cases and processes

05 - individual working figures, plots, etc

06 - individual working tables

07 - shiny dashboards

## Expected directory structure

\~/data-processed

\~/data-raw

\~/data-raw/ea

\~/data-raw/dds

\~/data-raw/household

\~/data-raw/screening

\~/data-raw/treatment

\~/figures

\~/R

\~/reports

## Initialising the environment

This project uses the `renv` package to create a reproducible environment  
<https://rstudio.github.io/renv/articles/renv.html>

The `.Rprofile` file enables bootstrapping of renv, and ensures the
directory structure is present as above.

## 01 Raw data

There are two ways of loading data into the environment: 1) with an API
key and 1) manually.

### Storing API Keys and retrieving data

If you have an API key available as a user of the REDCap projects listed
above, then you can run the scripts in order, starting with
01_retrieve_data.R to retrieve and store raw data and data dictionaries
in the expected directories.

To retrieve data via the REDCap API, first [store your API
keys]{.underline} in the `.Renviron` file:

1.  Find your home directory (where `.Renviron` is located):

    ``` r
    Sys.getenv("HOME")
    ```

2.  Open and edit `.Renviron`:

    ``` r
    usethis::edit_r_environ()
    ```

3.  Add the following key-value pairs (replace `your-key` with your
    actual API keys):

    ``` r
    RCAPI_PEARL_ea = "your-key"
    RCAPI_PEARL_hh = "your-key"
    RCAPI_PEARL_screen = "your-key"
    RCAPI_PEARL_treat = "your-key"
    ```

4.  Save and restart R to apply the changes.

Your API keys are now [securely stored and ignored in version
control.]{.underline} The `.gitignore` file includes `.Renviron` to
ensure that keys are not exposed to version control.

### Manually downloading raw data

Data can be exported from a specific report in each of the three core
data capture projects, and a fourth project which contains data
aggregated at enumeration area level. Data should be exported in
labelled format, and saved with the standard filename output from REDCap
into the respective raw data folder.

1.  [EA project
    report](https://redcap.sydney.edu.au/redcap_v14.3.14/DataExport/index.php?pid=24148&report_id=54859){.uri}

2.  [Household project
    report](https://redcap.sydney.edu.au/redcap_v14.3.14/DataExport/index.php?pid=19007&report_id=50913){.uri}

3.  [Screening project
    report](https://redcap.sydney.edu.au/redcap_v14.3.14/DataExport/index.php?pid=19019&report_id=40643){.uri}

4.  [Treatment project
    report](https://redcap.sydney.edu.au/redcap_v14.3.14/DataExport/index.php?pid=19018&report_id=47495){.uri}

### Manually downloading raw data dictionaries

The current data dictionary for each REDCap project should be saved into
the \~/data-raw/dds folder using the following filenames:

1.  ea_dd.csv
2.  household_dd.csv
3.  screening_dd.csv
4.  treatment_dd.csv

### Raw GIS data

This is not included in REDCap, and is provided by the SPC Statistics
for Development Division, through their popGIS service. GIS analysis
scripts in this project expect a file named:

`data-raw/gis/KIR_EA_Census2020FINAL.geojson`

This can be provided upon request.

## 02 Loading data

Once raw data is present as .csv files in the \~/data-raw/ directories,
the script named 02_load_data.R will load data from the files, load the
data dictionaries, and then set column types and column names according
to the data dictionary for each project. This prepares a working data
environment for the next step.

## 03 Tidying, cleaning and preparing data

Once data has been loaded into the environment, the script named
03_tidy_data.R will create helper columns, clean data as necessary,
pivot across EA, household and individual level and perform
miscellaneous other functions as needed to create a tidy data set for
analysis.

## 04 Helper scripts

Additional scripts for operational and support procedures are labelled
starting "04\_". These are run by specific staff as part of project
operations.

> NOTE: THESE SCRIPTS INCLUDE REDCAP WRITE FUNCTIONS

Any write function will require a confirmation by the user.

## Data analysis and outputs

Scripts prefixed "05\_", "06\_", "07\_" are for specific types of
figure, plot, tables, dashboards and so on - code can be incorporated
into reports as needed.
