# load utility functions
source("UtilityFunctions.R")

# assign data file names
fn_blogs<- "./final/en_US/en_US.blogs.txt"
fn_twitter<- "./final/en_US/en_US.twitter.txt"
fn_news<- "./final/en_US/en_US.news.txt"

# read words
ws_blogs<- obtainWordsFast(fn_blogs)

# build multiple words
system.time(ws2_blogs<- build2Words(ws_blogs[1:1e4]))
system.time(ws3_blogs<- build3Words(ws_blogs[1:1e4]))
