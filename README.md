# PEARL
Repo for the PEARL project

# About PEARL ----------------------------------------

The PEARL+ Project is an ambitious, systematic mass screening, treatment and prevention programme primarily focused on TB. In South Tarawa there are extremely high levels of TB transmission in the community, and the project aims to dramatically reduce this by visiting every household in this urban area. In addition to TB, we are also checking for leprosy, scabies and hepatitis B, making PEARL+ an integrated, community-based intervention. Along with the flagship mass screening initiative, PEARL+ will also provide crucial support for strategic planning and strengthening of existing disease programs.  
  
The PEARL+ project is an Australian aid initiative implemented by the University of Sydney on behalf of the Australian Government, and delivered by a team of Kiribati national health professionals in partnership with the Ministry of Health and Medical Services.  

# PEARL data --------------------------------------

Data is captured in REDCap, which hosted by the University of Sydney.
https://redcap.sydney.edu.au/

Data is owned jointly University of Sydney and Kiribati Ministry of Health and Medical Services, according to a data sharing agreement agreed by both parties.

# Data pipeline --------------------------------------

1. Data capture in REDCap mobile app
2. Upload to REDCap server at University of Sydney
3. Access and operational usage at project level, primarily using REDCap reports
4. Data available for manual analysis and reporting, through REDCap data export
5. Data available for automated analysis and reporting, through REDCap APIs
6. Analysis pipeline coded in R (this repo)

# Usage of this repo --------------------------------------

## Expected file structure ------------------------------------------

~/data-processed
~/data-raw
~/figures
~/R
~/reports

## Initialising the environment -----------------------------------

This project uses the renv package to create a reproducible environment
https://rstudio.github.io/renv/articles/renv.html

