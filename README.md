# ctpp-analysis

The 2006-2010 Census Transportation Planning Package (CTPP) is a special tabulation of five-year American Community Survey (ACS) data. 

Raw CTPP data can be downloaded here for the entire US or at the state-level: [ftp://data5.ctpp.transportation.org/](ftp://data5.ctpp.transportation.org/)

Documentation is sparse. I'll share what I've learned here. 

In general, the data files are all in long format. Each row describes an observation (count or flow) for one geographic unit on one variable. The data files do not contain descriptive variable names or geographic information (aside from a single geoid); they must be joined with various lookup tables.

For a single state (Texas, in this case), the directory structure is as follows:

```
TX
└───48
│   │   TX_2006thru2010_A101100.csv
│   │   TX_2006thru2010_A101101.csv
│   │   TX_2006thru2010_A101102.csv
│   │   ...
│   │   TX_2006thru2010_B309201.csv

```

Understanding the contents of each table within the state directory requires using the 2006-2010_ctpp_lookup.tar.gz file. It's quite important and contains a number of lookup tables, including:
* ReadMe.xlsx
* 2006-2010_CTPP_Documentation for AASHTO-Oct 22 2013.xlsx: This file contains information identifying the content of each table listed within the state directory and providing further instructions for data access. In contains a number of worksheets:
  ** Part1Tables
  ** Part2Tables
  ** Part3Tables
  **Geog
  **Geoids
  **Universes
  **Variables
* Keys to Map Lookup Files to Data.docx
* Formula to Calculate Margins of Error for Zero Estimates by DSSD.docx

* acs_2006thru2010_ctpp_table_shell.txt: This file includes variable names cross-referenced using the table name (TBLID) and line number (LINENO).
* acs_2006thru2010_ctpp_flow_geo.txt: This file includes a single GEOID column. Near as I can tell, it includes every *possible* flow between all units of geography for which flows are defined  (see the "Geog" worksheet in 2006-2010_CTPP_Documentation for AASHTO-Oct 22 2013.xlsx). This means that a row is included even if there's no observed commuter flow.
* acs_2006thru2010_ctpp_res_geo.txt
* acs_2006thru2010_ctpp_pow_geo.txt

I'm still trying to work out what's included in the flow lookup table, but the place-of-work and place-of-residence lookups seem to be what you'd expect -- a GEOID cross-referenced with its higher-level geographic information. 

# CTPP structure

The CTPP consists of three "parts." Understanding the data that underlie each part is key to understanding the CTPP: 
* Part 1 summarizes information about commuters at their place of residence (i.e. commute trip origins). It can answer questions like "What share of commuters ?"
* Part 2 summarizes information about commuters at their place of work (i.e. commute trip destinations). It can answer questions like "What share of commuters arriving in a particular county drive alone to work?"
* Part 3 summarizes flows of commuters between oriigns and destinations. 

Different cross-tabulations of 