---
title: "About"
---

# Overview

The purpose of this app is to investigate the Public Use Microdata Sample (PUMS) from the American Community Survey (ACS), which is conducted yearly by the U.S. Census Bureau. The app contains data from North Carolina for the year 2023. Summaries and plots, available on the Data Exploration tab, are the main analytical features of the app. Two categorical variables are available for subsetting via the first two widgets on the sidebar. The user may also select up to two numerical variables for subsetting using the third and fourth widgets. The specific groups of plots and summaries that are visible are controlled by the final widget on the sidebar.

## Data

The PUMS data is obtained from the American Community Survey (ACS), whose results comprise around 1% of the population in the US^1^.

- This app contains data for North Carolina, 2023
- Data were retrieved as zipped files downloaded from the FTP site [here](https://www2.census.gov/programs-surveys/acs/data/pums/2023/1-Year/)
- There is a Margin of Error (MOE) associated with PUMS, as PUMS data are produced via sampling from the full ACS, and some values have been anonymized^2^
  + Sampling error stats were calculated by following the guidelines in the PUMS User Guide. Formulas for variance, standard error, and margin of error have been translated to custom functions for use in this app
- PWGTP is the weighting variable for the person file that allows making estimates for the entire population based on the PUMS sample. This column is available in the csv file downloaded from the Data Download tab
  + Custom functions were written for this app to incorporate person-weight into the calculations  
- Please see the README in github for additional technical information on how data was handled
- For more information about PUMS data, please see the [U.S. Census Bureau's Documentation page](https://www.census.gov/programs-surveys/acs/microdata/documentation.html).



*According to the U.S. Census Bureau, "Only twenty questions were on the 1950 census form, which made it easier for this Virginia mother to respond to the enumerator's questions while at home with her young children."*^3^

![*Image credit: U.S. Census Bureau*](data/1950_census-hi-resized.png)

---
1. U.S. Census Bureau. *American Community Survey 2023 1-YEAR: PUMS User Guide and Overview* (2023), 4. 
2. Ibid.
3. U.S. Census Bureau. "1950 Census." Census.gov, January 7, 2022. https://www.census.gov/library/photos/1950_census.html.
