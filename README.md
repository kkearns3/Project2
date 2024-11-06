---
README
---

# Project2

## About the App

This app allows a user to access Public Use Microdata Sample (PUMS) data for the year 2023 in the state of North Carolina. PUMS is a sample of about 1% of the U.S. Population, whose data are derived from the U.S. Census Bureau's yearly American Community Survey (ACS).^1^

## Data

- Data were retrieved as zipped files downloaded from the FTP site [here](https://www2.census.gov/programs-surveys/acs/data/pums/2023/1-Year/)
  + Person-level file and Housing-level file were downloaded separately and joined on SERIALNO
- As the data comes from a prior year, the file includes a column for adjusting incomes and earnings for inflation (ADJINC). I elected not to incorporate this, so as to provide an unobscured view of the numerical estimates
- There is a Margin of Error (MOE) associated with PUMS, as PUMS data are produced via sampling from the full ACS, and some values have been anonymized^2^
- It should be noted that while SERIALNO are unique in Housing data, they are not in the Person file (presumably because many households have more than one person)
    + As such, all metrics are calculated based on individuals (using PWGTP), even if the characteristic chosen is measured at the housing level
    + Therefore, certain metrics may be overestimated if the numerical data point is measured at the household level (e.g. property value), and a significant number of SERIALNO have more than one member in the household
- The Census Bureau provides a Data Dictionary on their [PUMS documentation page](https://www.census.gov/programs-surveys/acs/microdata/documentation.html), which was used within the app to convert data values and plot labels to descriptive values more easily interpreted by the user
- I added a few grouped variables, either because they were numeric but make sense as a categorical variable when grouped into ranges (e.g. AGEP), or to reduce the number of levels in a categorical variable that doesn't necessarily reveal a ton when they have many different levels (e.g. YRBLT)
- Since the data is weighted by the person-weight variable (PWGTP), I wrote several functions to correctly calculate common statistics, namely mean, median, variance, standard error, and margin of error.
  + Mean and median were written with their standard mathematical definitions
  + All error statistics were written utilizing the formulas provided in the [User Guide](https://www2.census.gov/programs-surveys/acs/tech_docs/pums/2023ACS_PUMS_User_Guide.pdf)^3^. The variance is given by: $VAR(x) = \frac{4}{80} \sum_{k=1}^{80} (x_r - x)^2$, where $x_r$ are the 80 replicate person-weights (PWGTP1-PWGTP80), and $x$ is the value of the estimate (I elected to base the error on the mean)
  + Thereafter, the standard error is $SE = \sqrt{VAR(x)}$, and the margin of error using a 90% confidence leel is $MOE = SE * 1.645$


## Future Releases

While future releases are not planned, potential improvements can be made with a reasonable amount of edits. Such improvements are outside the scope of this project, but they include:
- Expanding the analysis to additional states and/or years
- Accessing data through an API call, allowing customization of which columns are included
- Designing more custom data fields from existing data, e.g. determining geographical movements of individuals through analysis of which PUMA they work in or previously lived in vs. where they live now
- Further customization of summaries and plots to give the user more control over the analyses they conduct with the data


---
1. U.S. Census Bureau. *American Community Survey 2023 1-YEAR: PUMS User Guide and Overview* (2023), 4. 
2. Ibid.
3. Ibid., 11.
