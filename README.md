### Harvard Political Contributions

#### About

This repository contains data on the political contributions of Harvard faculty, administrators, and staff between 2001 and Election Day 2014. It is derived from FEC Individual Contribution [data disclosures](http://www.fec.gov/finance/disclosure/ftpdet.shtml) over that time period. 

A subset of contributors who gave between 2011 and 2014 have been manually verified as Harvard employees, and data on the gender, school, and position of those contributors are also provided. 

Analysis of the data can be found in ```analysis.R```.

#### Data

* ```harvard-contributions.csv```: all Harvard political contributions, 2001 - 2014. 

| Column Name   | Description |
| ------------- | ------------- |
|  NAME |  |
|  ZIP_CODE |  |
|  EMPLOYER |  |
|  OCCUPATION |  |
|  TRANSACTION_DT |  |
|  TRANSACTION_PGI |  |
| TRANSACTION_AMT |  |
|  CMTE_NM |  |
| CMTE_DSGN |  |
| PARTY |  |
|  CMTE_TP |  |

* ```harvard-people.csv```: unique verified Harvard contributors, with additional employement information, 2011 - 2014.  

| Column Name  | Description |
| ------------- | ------------- |
| NAME | | 
| GENDER | | 
| SCHOOL | | 
| TITLE | | 

* ```harvard-contributions-2011-2014-tagged.csv```: Harvard political contributions, tagged with employement information, 2011 - 2014. The column names for this file are just the union of those from ```harvard-people.csv``` and ```harvard-contributions.csv```.

#### Building From Source Data

For transparency's sake, I've included ```build.R```, the R script that were used to  merge the raw FEC individual contribution files with committee and candidate information. It assumes that the raw FEC data, which is released in two year chunks, is organized such that the data file for 2011-2012 can be found in ```data/raw_data/11-12```, for example, and it assumes that the header files provided by the FEC are in ```data/header```. 

You can download the raw individual contribution, committee, and candidate data [here](http://www.fec.gov/finance/disclosure/ftpdet.shtml). I didn't include it in the repository because it comprises at least 20 million rows of data spanning nearly 15 years.  
