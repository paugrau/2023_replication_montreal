This document contains information on the replication files and software information for:

Leininger, Arndt, Marie-Lou Sohnius, Thorsten Faas, Sigrid Roßteutscher, and Armin Schäfer. 2022. “Temporary Disenfranchisement: Negative Side-Effects of Lowering the Voting
Age.” American Political Science Review.

Software information: The analysis was run using R version 4.1.2.  
The replication files include 5 coding files:

1. main.Rmd: R code to replicate all analyses reported in the article and corresponding on-line appendix. Refers to all further provided data and coding files and provides information about the packages needed to replicate the analyses. Can be run chunk by chunk for selected replication or entirely for generating all figures and tables. Run-time on Windows machine (8GB RAM) using one core and R version 4.1.2: 70 seconds.

2. data.R: R code to clean and recode the data. Creates all subsamples necessary for the analyses. Automatically sourced by main.Rmd.

3. functions.R: R code to define additional functions used for analyses. Automatically sourced by main.Rmd.

4. timetable.R: R code to replicate appendix figure B.1. Automatically sourced by main.Rmd.

5. replication_dataset.do: Filters original dataset to only keep variables necessary for presented analyses. The variables of the datset provided in this repository are already filtered using the code.



The replication files include 5 data files.  

For the main analysis of the article, 2 files are provided:

1. shpanel_20200908_v12.dta: Raw replication data. 

2. df.RData: Prepared dataset used in all further analyses. Additional information on the variables included can be found in the codebook.


For an additional visualization of the sampling strategy, an additional 1 files (1 excel file) is provided. 

3. gruppen.csv: Overview of birthdate cut-offs for group membership.


The repository further includes several compiled files providing an overview of the code and output and PDF and HTML format that can be produced using the main.Rmd script. Also, the folders figures and tables include presented results that are automatically re-produced with the main.Rmd.

DEMO CHANGE