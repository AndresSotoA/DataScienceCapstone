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
samplePct<- 1  # 2% sample rate
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
library(tau)
# pretreat the lines
# 1. remove last punctuation in a paragraph
ltmp<- gsub("[[:punct:]]$","", ls_train)
# 2. remove first punctuation in a paragraph
ltmp<- gsub("^[[:punct:]]","", ltmp)
# 3. remove punctuations that is not in between words
ltmp<- gsub("[[:punct:]]+[[:space:]]+"," ",ltmp)  # trailing punct
ltmp<- gsub("[[:space:]]+[[:punct:]]+"," ",ltmp)
# replace ’ to '
ltmp<- gsub("’","'",ltmp)
# 4. keep alphabets -, and ' 
ltmp<- gsub("[^a-zA-Z\\-\\']"," ",ltmp)
# 5. remove three dots
ltmp<- gsub("[...]"," ",ltmp)


# count ngram
ng1<- textcnt(ltmp, method= "string", n= 1, split= "[[:space:][:digit:]]+",useBytes = TRUE)
ng2<- textcnt(ltmp, method= "string", n= 2, split= "[[:space:][:digit:]]+",useBytes = TRUE)
ng3<- textcnt(ltmp, method= "string", n= 3, split= "[[:space:][:digit:]]+",useBytes = TRUE)
ng4<- textcnt(ltmp, method= "string", n= 4, split= "[[:space:][:digit:]]+",useBytes = TRUE)


# ====== Convert to Data Frame ======
ng1df<- data.frame(name= names(ng1), value= as.vector(ng1))
ng2df<- data.frame(name= names(ng2), value= as.vector(ng2))
ng3df<- data.frame(name= names(ng3), value= as.vector(ng3))
ng4df<- data.frame(name= names(ng4), value= as.vector(ng4))

# ===== prep data frame to prediction format =====
library(tidyr)
df1<- prep_ngHigh(ng1df)
df2<- prep_ngHigh(ng2df)
df3<- prep_ngHigh(ng3df)
df4<- prep_ngHigh(ng4df)



# ======== save data to files ==========
save(df1,df2,df3,df4, file="ngramDF.RData")

#save(ls_train, ls_cvn, ls_test,
#     ws1, nws2, nws3, nws4,
#     ng1,ng2,ng3,ng4, file="./sampledData/sampledData.RData")

# ====== post proc =====
df2a<- df2 %>% 
    filter(w1 %in% df1$w1[1:5000]) %>%
    filter(w2 %in% df1$w1[1:5000]) %>%
    filter(count>2)
df3a<- df3 %>% 
    filter(w1 %in% df1$w1[1:5000]) %>%
    filter(w2 %in% df1$w1[1:5000]) %>%
    filter(w3 %in% df1$w1[1:5000]) %>%
    filter(count>2)
df4a<- df4 %>% 
    filter(w1 %in% df1$w1[1:5000]) %>%
    filter(w2 %in% df1$w1[1:5000]) %>%
    filter(w3 %in% df1$w1[1:5000]) %>%
    filter(w4 %in% df1$w1[1:5000]) %>%
    filter(count>2)










