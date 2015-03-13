### Harvard Political Contributions

![Harvard Political Contributions, Totals by Month](http://i.imgur.com/MBSzfe5.jpg)

#### About

This repository contains data on the political contributions of Harvard faculty, administrators, and staff between 2001 and Election Day 2014. It is derived from FEC Individual Contribution [data disclosures](http://www.fec.gov/finance/disclosure/ftpdet.shtml) over that time period. 

A subset of contributors who gave between 2011 and 2014 has been manually verified. Data on the gender, school, and position of those contributors are also provided. 

Analysis of the data can be found in ```analysis.R```.

#### Data

* ```harvard-contributions.csv```: all Harvard political contributions, 2001 - 2014. 

* ```harvard-people.csv```: unique verified Harvard contributors, with additional employement information, 2011 - 2014.  

* ```harvard-contributions-2011-2014-tagged.csv```: Harvard political contributions, tagged with employement information, 2011 - 2014. `.

#### Building From Source Data

For transparency's sake, I've included ```build.R```, the R script that were used to  merge the raw FEC individual contribution files with committee and candidate information. It assumes that the raw FEC data, which is released in two year chunks, is organized such that the data file for 2011-2012 can be found in ```data/raw_data/11-12```, for example, and it assumes that the header files provided by the FEC are in ```data/header```. 

You can download the raw individual contribution, committee, and candidate data [here](http://www.fec.gov/finance/disclosure/ftpdet.shtml). I didn't include it in the repository because it comprises at least 20 million rows of data spanning nearly 15 years. 

### Column Names and Descriptions

##### ```harvard-contributions.csv```

| Column Name   | Description |
| ------------- | ------------- |
|  NAME | Contributor Name |
|  ZIP_CODE | Contributor Zip Code |
|  EMPLOYER | Contributor Employer (Self-Reported) |
|  OCCUPATION | Contributor Occupation (Self-Reported |
|  TRANSACTION_DT | Transaction Date |
|  TRANSACTION_PGI | Primary-General Indicator: the election for which the contribution was made. EYYYY (election plus election year) |
| TRANSACTION_AMT | Transaction Amount |
|  CMTE_NM | Recipient (Committee) |
| CMTE_DSGN | Committee Designation (See [here](http://www.fec.gov/finance/disclosure/metadata/DataDictionaryCommitteeMaster.shtml)) |
| PARTY | Committee Party Affiliation |
|  CMTE_TP | Committee Type (See [here](http://www.fec.gov/finance/disclosure/metadata/CommitteeTypeCodes.shtml)|

##### ```harvard-people.csv```

| Column Name  | Description |
| ------------- | ------------- |
| NAME | Contributor Name | 
| GENDER |  Gender | 
| SCHOOL |  School of Employement (HBS, HKS, SPH, etc.)| 
| TITLE | Position Title (Professor, Administrator, etc.) | 

##### ```harvard-contributions-2011-2014-tagged.csv```

The column names for this file are just the union of those from ```harvard-people.csv``` and ```harvard-contributions.csv``
