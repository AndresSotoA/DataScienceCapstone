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

# ========== sample training datas ===============
# set seed
set.seed(1345)

# compute number of lines to be sampled
samplePct<- 6  # 6% sample rate
nls_blogs<- samplePct/100* length(lines_blogs)
nls_twitter<- samplePct/100* length(lines_twitter)
nls_news<- samplePct/100*length(lines_news)

# stratified sampling now
ls_blogs<- striatifiedSelectLines(lines_blogs,n = nls_blogs)
ls_twitter<- striatifiedSelectLines(lines_twitter,n = nls_twitter)
ls_news<- striatifiedSelectLines(lines_blogs,n = nls_news)

ls_train<- c(ls_blogs,ls_twitter,ls_news)

# =========== sample cv sets =============
# compute number of lines to be sampled
samplePct<- 2  # 2% sample rate
nls_blogs<- samplePct/100* length(lines_blogs)
nls_twitter<- samplePct/100* length(lines_twitter)
nls_news<- samplePct/100*length(lines_news)

# stratified sampling now
ls_blogs<- striatifiedSelectLines(lines_blogs,n = nls_blogs)
ls_twitter<- striatifiedSelectLines(lines_twitter,n = nls_twitter)
ls_news<- striatifiedSelectLines(lines_blogs,n = nls_news)

ls_cvn<- c(ls_blogs,ls_twitter,ls_news)

# ========= sample test sets ============
# compute number of lines to be sampled
samplePct<- 2  # 2% sample rate
nls_blogs<- samplePct/100* length(lines_blogs)
nls_twitter<- samplePct/100* length(lines_twitter)
nls_news<- samplePct/100*length(lines_news)

# stratified sampling now
ls_blogs<- striatifiedSelectLines(lines_blogs,n = nls_blogs)
ls_twitter<- striatifiedSelectLines(lines_twitter,n = nls_twitter)
ls_news<- striatifiedSelectLines(lines_blogs,n = nls_news)

ls_test<- c(ls_blogs,ls_twitter,ls_news)

# =========== build n-gram ===========
library(tau)

# pretreat the lines
ltmp1<- addSentenceSymbol(ls_train, n=1)
ltmp2<- addSentenceSymbol(ls_train, n=2)
ltmp3<- addSentenceSymbol(ls_train, n=3)
ltmp4<- addSentenceSymbol(ls_train, n=4)
ltmp5<- addSentenceSymbol(ls_train, n=5)

# count ngram
ng1<- textcnt(ltmp1, method= "string", n= 1, useBytes = TRUE)
ng2<- textcnt(ltmp2, method= "string", n= 2, useBytes = TRUE)
ng3<- textcnt(ltmp3, method= "string", n= 3, useBytes = TRUE)
ng4<- textcnt(ltmp4, method= "string", n= 4, useBytes = TRUE)
ng5<- textcnt(ltmp5, method= "string", n= 5, useBytes = TRUE)




# ======== save data to files ==========
save(ls_train, ls_cvn, ls_test,
     ng1,ng2,ng3,ng4,ng5, file="sampledData.RData")



