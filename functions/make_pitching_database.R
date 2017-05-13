###############################################
#
# Create Pitching Database:
# Script queries MLB baseball info and saves
# data into SQL database
#
# Only needs to be run once to get database
#
###############################################

###### Load needed libraries

library(pitchRx)
library(dplyr)
filter = dplyr::filter
setwd('/Volumes/TRANSCEND/MLB')

###### Create database and scrape

db = src_sqlite("pitchfx.sqlite3.2012_on", create=T)
scrape(start="2012-03-31", end=Sys.Date(), connect=db$con)

