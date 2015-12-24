# load utility functions
source("UtilityFunctions.R")

# assign data file names
fn_blogs<- "./final/en_US/en_US.blogs.txt"
fn_twitter<- "./final/en_US/en_US.twitter.txt"
fn_news<- "./final/en_US/en_US.news.txt"

# read lines vector
lines_blogs<- obtainLinesFast(fn_blogs)
lines_twitter<- obtainLinesFast(fn_twitter)
lines_news<- obtainLinesFast(fn_news)

# read words vector
words_blogs<- obtainWordsFast(fn_blogs)
words_twitter<- obtainWordsFast(fn_twitter)
words_news<- obtainWordsFast(fn_news)

# ========== sample datas ===============
# set seed
set.seed(1234)

# compute number of lines to be sampled
samplePct<- 6  # 6% sample rate
nls_blogs<- samplePct/100* length(lines_blogs)
nls_twitter<- samplePct/100* length(lines_twitter)
nls_news<- samplePct/100*length(lines_news)

# stratified sampling now
ls_blogs<- striatifiedSelectLines(lines_blogs,n = nls_blogs)
ls_twitter<- striatifiedSelectLines(lines_twitter,n = nls_twitter)
ls_news<- striatifiedSelectLines(lines_blogs,n = nls_news)

ls_all<- c(ls_blogs,ls_twitter,ls_news)

# =========== divide data sets =============


