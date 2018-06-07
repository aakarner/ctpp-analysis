# ctpp-analysis

The 2006-2010 Census Transportation Planning Package (CTPP) is a special tabulation of five-year American Community Survey (ACS) data. 

Raw CTPP data can be downloaded here for the entire US or at the state-level: [ftp://data5.ctpp.transportation.org/]

Documentation is sparse. I'll share what I've learned here. 

In general, the data files are all in long format. Each row describes an observation (count or flow) for one geographic unit on one variable. The data files do not contain descriptive variable names or geographic information (aside from a single geoid); they must be joined with various lookup tables 

The 2006-2010_ctpp_lookup.tar.gz file is important. It contains a number of lookup tables, including:
* acs_2006thru2010_ctpp_table_shell.txt: This file includes variable names cross-referenced using the table name (TBLID) and line number (LINENO).
* acs_2006thru2010_ctpp_flow_geo.txt
* acs_2006thru2010_ctpp_res_geo.txt
* acs_2006thru2010_ctpp_pow_geo.txt

I'm still trying to work out what's included in the flow lookup table, but the place-of-work and place-of-residence lookups seem to be what you'd expect -- a GEOID cross-referenced with its higher-level geographic information. 