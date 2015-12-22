# load utility functions
source("task1_tokenize.R")
source("task2_exploratory.R")

# assign data file names
fn_blogs<- "./final/en_US/en_US.blogs.txt"
fn_twitter<- "./final/en_US/en_US.twitter.txt"
fn_news<- "./final/en_US/en_US.news.txt"

# read lines
lines_blogs<- obtainLinesFast(fn_blogs)
lines_twitter<- obtainLinesFast(fn_twitter)
lines_news<- obtainLinesFast(fn_news)

# tokenize from lines
tkn_blogs<- obtainWordsFast(fn_blogs)
tkn_twitter<- obtainWordsFast(fn_twitter)
tkn_news<- obtainWordsFast(fn_news)

# build 2 gram vector
ng2_blogs<- buildNGramVector(tkn_blogs,n=2)
ng2_twitter<- buildNGramVector(tkn_twitter,n=2)
ng2_news<- buildNGramVector(tkn_news,n=2)

# build 2 gram counts
library(tau)

ng2C_blogs<- textcnt(tkn_blogs, methods="string", n=2)
