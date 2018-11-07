# Table of Contents
1. [Overview](#Overview)
2. [The data](#The-data)
3. [The scripts](#The-scripts)
4. [To-do](#To-do)

# Overview

The 2006-2010 Census Transportation Planning Package (CTPP) is a special tabulation of five-year American Community Survey (ACS) data. It consists of three "parts." Understanding the data that underlie each part is key to understanding the CTPP: 
* Part 1 summarizes information about commuters at their place of residence (i.e. commute trip origins). It can answer questions like "What share of commuters originating in a particular county use public transit?"
* Part 2 summarizes information about commuters at their place of work (i.e. commute trip destinations). It can answer questions like "What share of commuters arriving in a particular county drive alone to work?"
* Part 3 summarizes flows of commuters between origins and destinations. 

Raw CTPP data can be downloaded here for the entire US or at the state-level: [ftp://data5.ctpp.transportation.org/](ftp://data5.ctpp.transportation.org/)

# Data

Documentation is sparse. I'll share what I've learned here. 

In general, the data files are in long format. Each row describes an observation (count or flow) for one geographic unit on one variable. The data files do not contain descriptive variable names or geographic information (aside from a single geoid); they must be joined with various lookup tables to be interpretable. 

For a single state (Texas, in this case), the directory CTPP structure is as follows:

```
TX
└───48
    │   TX_2006thru2010_A101100.csv
    │   TX_2006thru2010_A101101.csv
    │   TX_2006thru2010_A101102.csv
    │   ...
    │   TX_2006thru2010_B309201.csv

```

Understanding the contents of each table within a state's directory requires using the 2006-2010_ctpp_lookup.tar.gz file. It's quite important and contains a number of lookup tables, including:
* **ReadMe.xlsx**: 
* **2006-2010_CTPP_Documentation for AASHTO-Oct 22 2013.xlsx**: This file contains information identifying the content of each table listed within the state directory and provides further instructions for data access. In contains a number of worksheets:
  * Part1Tables: Describes all tables that contain Part 1 (place of residence) information.
  * Part2Tables: Describes all tables that contain Part 2 (place of work) information. 
  * Part3Tables: Describes all tables that contain Part 3 (flow) information.
  * Geog: Explains CTPP-specific summary levels, describing the geographic summary levels available for each table.
  * Geoids: The CTPP dataset uses different geographic identifiers than regular census products. These are defined here.
  * Universes: Describes possible universes (i.e. populations) from which CTPP data are drawn.
  * Variables: Describes the factor levels used for all categorical variables.
* **Keys to Map Lookup Files to Data.docx**
* **Formula to Calculate Margins of Error for Zero Estimates by DSSD.docx**
  
The data tables within each part contain consistent headings. 

The following text files are all pipe-delimited (y tho?):

* **acs_2006thru2010_ctpp_table_shell.txt**: This file includes variable names cross-referenced using the table name (TBLID) and line number (LINENO).
* **acs_2006thru2010_ctpp_flow_geo.txt**: This file includes a single GEOID column. Near as I can tell, it includes every *possible* flow between all units of geography for which flows are defined (see the "Geog" worksheet in 2006-2010_CTPP_Documentation for AASHTO-Oct 22 2013.xlsx). This means that a row is included even if there's no observed commuter flow.
* **acs_2006thru2010_ctpp_res_geo.txt**
* **acs_2006thru2010_ctpp_pow_geo.txt**

I'm still trying to work out what's included in the flow lookup table, but the place-of-work and place-of-residence lookups seem to be what you'd expect -- a GEOID cross-referenced with its higher-level geographic information. 

# Scripts
In general, the CTPP data tables are not that large, so a database probably isn't necessary for most analyses. But some of the flow tables are >2 gb, so to make the scripts general enough to cover all analytical possibilities, all data are read into a local MonetDBLite database in the first script. Later scripts read necessary subsets into R to perform analysis.

* **01-create-database.R**: Create a local MonetDB database and populate it with all necessary tables.
* **02-analysis-part1.R**: Toy analysis demonstrating how CTPP Part 1 data can be extracted from the database and visualized.
* **03-analysis-part3-1.R**: The CTPP Part 3 does not contain a flow table cross-tabbed by race and mode. This script reads the data necessary to create one from the CTPP into the local database.
* **04-analysis-part4-2.R**: Beginning with the Part 3 flow table by mode, and marginal totals from Part 1 and Part 2, use iterative proportional fitting to create synthetic tract-level flow tables stratified by mode and race/ethnicity. 
* **05-analysis-mapping.R**: Short examples for visualizing and mapping mode share.

# To-do
1. Consider uncertainty (collect and report margins of error)
