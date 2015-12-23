# load utility functions
source("task1_tokenize.R")
source("task2_exploratory.R")

# assign data file names
fn_blogs<- "./final/en_US/en_US.blogs.txt"
fn_twitter<- "./final/en_US/en_US.twitter.txt"
fn_news<- "./final/en_US/en_US.news.txt"

# ======= determine sample size using blogs data  =======
library(tau)
lines_blogs<- obtainLinesFast(fn_blogs)
words_blogs<- obtainWordsFast(fn_blogs)

# compute total vocabulary size
nTotalWords<- length(words_blogs)
voc_blogs<- unique(words_blogs)
nVoc<- length(voc_blogs) # vocabulary size

# compute sample vocaburary
nSamplePool<- seq(1e2,1e5,length.out = 10)

vocRatio<- vector()
freqWordCnt<- vector()
Coverage90_vocRatio<- vector()
for (nSampleLines in nSamplePool)
{
    lnow<- randomSelectLines(lines_blogs,n = nSampleLines)
    ng1<- textcnt(lnow, method= "string",n=1)
    nSampleVoc<- length(ng1)
    
    ng1s<- sort(ng1, decreasing = TRUE)
    cnt<- 1
    while (TRUE)
    {
        cnt<- cnt+1 
        coverage<- sum(ng1s[1:cnt])/sum(ng1s)
        if (coverage > 0.9)
        {
            Coverage90_vocRatio<- c(Coverage90_vocRatio, 
                                    cnt/length(ng1s))
            break
        }
    }
    
    # sample to total ratio
    vocRatio<- c(vocRatio, nSampleVoc/nVoc)
    
    # frequent word counts
    freq<- ng1/sum(ng1)
    freqWordCnt<- c(freqWordCnt, length(ng1[freq>1e-3]))
}
# =======================================================



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
ng1C_blogs<- textcnt(tkn_blogs[1:1e5], method="string", n=1)
ng2C_blogs<- textcnt(tkn_blogs, method="string", n=2)

# get unique words in ngram
ut1_blogs<- unique(tkn_blogs)
ut2_blogs<- unique(ng2_blogs)

# calculate frequency
ng1C_blogs<- sort(ng1C_blogs, decreasing = TRUE)
cover1<- sum(ng1C_blogs[1:10000])/length(tkn_blogs)

ng2C_blogs<- sort(ng2C_blogs, decreasing = TRUE)
cover2<- sum(ng2C_blogs[1:10000])/length(ng2_blogs)
