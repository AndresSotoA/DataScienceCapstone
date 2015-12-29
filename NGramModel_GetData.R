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
set.seed(2345)

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

# ========= build n-gram ==========
# pretreat the lines
ltmp1<- addSentenceSymbol(ls_train, n=1)
ltmp2<- addSentenceSymbol(ls_train, n=2)
ltmp3<- addSentenceSymbol(ls_train, n=3)
ltmp4<- addSentenceSymbol(ls_train, n=4)

# get raw words
ws1<- unlist(strsplit(ltmp1, split="[[:punct:][:space:]]+"
                      , useBytes = TRUE))
ws2<- unlist(strsplit(ltmp2, split="[[:punct:][:space:]]+"
                      , useBytes = TRUE))
ws3<- unlist(strsplit(ltmp3, split="[[:punct:][:space:]]+"
                      , useBytes = TRUE))
ws4<- unlist(strsplit(ltmp4, split="[[:punct:][:space:]]+"
                      , useBytes = TRUE))

# get n-words
nws2<- build2Words(ws2)
nws3<- build3Words(ws3)
nws4<- build4Words(ws4)

# count n-gram
ng1<- table(ws1)
ng2<- table(nws2)
ng3<- table(nws3)
ng4<- table(nws4)


# =========== build n-gram ===========
#library(tau)

#ltmp5<- addSentenceSymbol(ls_train, n=5)

# count ngram
#ng1<- textcnt(ltmp1, method= "string", n= 1, useBytes = TRUE)
#ng2<- textcnt(ltmp2, method= "string", n= 2, useBytes = TRUE)
#ng3<- textcnt(ltmp3, method= "string", n= 3, useBytes = TRUE)
#ng4<- textcnt(ltmp4, method= "string", n= 4, useBytes = TRUE)
#ng5<- textcnt(ltmp5, method= "string", n= 5, useBytes = TRUE)


# ======== save data to files ==========
save(ls_train, ls_cvn, ls_test,
     ws1, nws2, nws3, nws4,
     ng1,ng2,ng3,ng4, file="sampledData.RData")


# ===== keep only most freq n-gram ======






