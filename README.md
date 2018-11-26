# Coursera-SwiftKey Milestone Report
####
####
#### Author: JiaHsuan Lo

## Introduction

The purpose of this Cousera Capsotne project is to apply Natural Language Programming techniques to contruct a prediction model that is able to predict upcoming words based on previous words input by user. The training and test data is from a corpus called  HC Corpora [www.corpora.heliohost.org](www.corpora.heliohost.org). 

This milestone report will first demonstrates the findings in the initial exploratory data analysis. Then the goals and strategies of building the prediction model will be proposed. 

## Data Reading

The data was obtained from the Coursera site: [https://d396qusza40orc.cloudfront.net/dsscapstone/dataset/Coursera-SwiftKey.zip](https://d396qusza40orc.cloudfront.net/dsscapstone/dataset/Coursera-SwiftKey.zip)

Some utility functions was constructed in  **UtilityFunctions.R** to read the data. In the study, only the English library was used. Therefore, the text data in **en_US.blogs.txt**, **en_US.twitter.txt**, and **en_US.news.txt** were read into memory. :

``` {r readData, cache= TRUE}
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

```

The lines and words counts are summarized in the following table:

File Name         |   Number of Lines          |    Number of Words  
------------------|----------------------------|-------------------------
en_US.blogs.txt   |  `r length(lines_blogs)`   | `r length(words_blogs)`
en_US.twitter.txt |  `r length(lines_twitter)` | `r length(words_twitter)`   
en_US.news.txt    |  `r length(lines_news)`    | `r length(words_news)` 


## Sampling Scheme

As can be seen in the previous section, the amount of data is huge and can not be efficiently processed by general PC and mobile device. Sampling the data is necessary. Several possible metrics were calculated to determine the sample size, including:

1. vocabulary ratio: vocabulary size in sampled data / vocubulary size in full data
2. frequent word count: number of words that has a greater than 1e-3 probability
3. 90% coverage word count ratio: how many unique words (in terms of percentage of sampled vocabulary) are needed in a frequency sorted dictionary to cover 90% of all word instances in the language? 

In the following calculation, sentences were selected out of the data file using stratified random sampling method. The sample size (number of sentences) was gradually increased from 1e2 to 1e5 sentences. Item 1-3 mentioned above were computed for each of the sample size. To simplify the process, only one file, **en_US.blogs.txt**, was used in this analysis. The Library **tau** was used here for calculating the word frequency.  

``` {r sampling, cache= TRUE}
library(tau)
source("UtilityFunctions.R")
# compute total vocabulary size
nTotalWords<- length(words_blogs)
voc_blogs<- unique(words_blogs)
nVoc<- length(voc_blogs) # vocabulary size

# compute sample vocaburary
nSamplePool<- seq(1e2,1e5,length.out = 10)

set.seed(314)
vocRatio<- vector()
freqWordCnt<- vector()
Coverage90_vocRatio<- vector()
for (nSampleLines in nSamplePool)
{
    # randomly select samples
    lnow<- striatifiedSelectLines(lines_blogs,n = nSampleLines)
    # compute word counts
    ng1<- textcnt(lnow, method= "string",n=1)
    # compute size of sample vocabulary (unique words)
    nSampleVoc<- length(ng1)
    
    # sort by frequency
    ng1s<- sort(ng1, decreasing = TRUE)
    
    # obtain 90% coverage vocabulary ratio
    Coverage90_vocRatio<- c(Coverage90_vocRatio,
                            VocabularyCoverage(ng1s,90))
    
    # sample to total ratio
    vocRatio<- c(vocRatio, nSampleVoc/nVoc)
    
    # frequent word counts
    freq<- ng1/sum(ng1)
    freqWordCnt<- c(freqWordCnt, length(ng1[freq>1e-4]))
}

```

Now plot the results

``` {r plotsample, message=FALSE, fig.width= 12, fig.height= 12}
library(ggplot2)
library(tidyr)

df<- data.frame(Sample.Size= nSamplePool,
                Vocabulary.Ratio= vocRatio,
                Frequent.Word.Count= freqWordCnt,
                Cov.90pct.Vocabulary.Ratio= Coverage90_vocRatio*100)
df1<- df %>%
    gather(key,value,-Sample.Size) 

g<- ggplot(data= df1, aes(x= Sample.Size, y= value))
g<- g+ geom_point() + geom_line()
g<- g+ facet_grid(key~., scale="free")
g<- g+ theme(strip.text= element_text(size= 16))
g

```

The bottom figure shows the percentage of words within vocabulary that cover 90% of all word instances. As can be seen, this percentage decreased as the sample size grew and started to remained stable at around 5% level. That means it required **only 5%** of the most frequent words in the vocabulary to represent **90%** of the word instances in the dataset.

The top figure shows the sampled vocabulary to total vocabulary ratio. As shown in the figure, this ratio increased with increasing sample size, and a sample size of **1e5** sentences already covers **30%** of the total vocabulary. 

The middle figure shows that, except for small sample size, the number of most frequent words remain similar with increasing sample size. That indicates it does not require a large sample size to capture most frequent words.

Based on the above results, a sample size of 5e4 words, which is about **6%** of the en_US.blogs data file, will used for further analysis and predictive model construction. This **6%** sampling size rule will also be applied to en_US.twitter and en_US.news data sets.

## Frequency of N-grams, N=1...5

To investigate the frequency and distribution of n-grams in the data, the three data files were first sampled using the aforementioned **6%** sample size.

``` {r samplingFiles, cache= TRUE}
# compute number of lines to be sampled
samplePct<- 6  # 6% sample rate
nls_blogs<- samplePct/100* length(lines_blogs)
nls_twitter<- samplePct/100* length(lines_twitter)
nls_news<- samplePct/100*length(lines_news)

# stratified sampling now
ls_blogs<- striatifiedSelectLines(lines_blogs,n = nls_blogs)
ls_twitter<- striatifiedSelectLines(lines_twitter,n = nls_twitter)
ls_news<- striatifiedSelectLines(lines_blogs,n = nls_news)

```


Next the n-gram (n=1,2,3,4,5) vectors were build. Again the **tau** library was used in this task. 

``` {r buildNGram, cache= TRUE}
# construct n-grams, n=1:5

ng_blogs<- list()
ng_twitter<- list()
ng_news<- list()
for (i in seq(1,5))
{
    ng_blogs[[i]]<- textcnt(ls_blogs, method= "string",n= i)
    ng_twitter[[i]]<- textcnt(ls_twitter, method= "string",n= i)
    ng_news[[i]]<- textcnt(ls_news, method= "string",n= i)
    
}
```

After n-grams were obtained, the probability of appearance for each n-gram was estimated by calculating this ratio:

(count of the n-gram) / (total count)

Since the probability numbers are generally very small, log probabilities were computed and displayed in histograms.

``` {r logprob, cache=TRUE}

ng_logp_blogs<- list()
ng_logp_twitter<- list()
ng_logp_news<- list()

for (i in seq(1,5))
{
    # probabilities
    ng_p_blogs<- ng_blogs[[i]]/sum(ng_blogs[[i]]) 
    ng_p_twitter<- ng_twitter[[i]]/sum(ng_twitter[[i]]) 
    ng_p_news<- ng_news[[i]]/sum(ng_news[[i]]) 
    
    # log probablities
    ng_logp_blogs[[i]]<- log(ng_p_blogs)
    ng_logp_twitter[[i]]<- log(ng_p_twitter)
    ng_logp_news[[i]]<- log(ng_p_news)
    
}

```

Here are the some sample log probability histgram plots

``` {r histogram, fig.width=12, cache= TRUE}
hist(ng_logp_blogs[[2]], main= "Blogs data 2-gram Frequency" )
hist(ng_logp_twitter[[2]], main= "Twitter data 2-gram Frequency")
hist(ng_logp_news[[2]], main= "News data 2-gram Frequency")
```

As shown in these 2-gram histograms, most of the words has very low probability. Although not plotted here, this phonomenon is generaly true for other n-gram data. This log probability information will be used for the n-gram model construction.

The n-gram frequency information is summarized below:
``` {r ngramFreq, cache=TRUE, fig.width=12, fig.height= 16}
cntVec<- c(2,3,5,10,50,100,300)

appearRatio<- function(ng,x){sum(ng>=x)/length(ng)}

dfng<- data.frame()

for (i in seq(1,5))
{
    for (cntU in cntVec)
    {
        ft_blogs<- appearRatio(ng_blogs[[i]], cntU)*100
        ft_twitter<- appearRatio(ng_twitter[[i]], cntU)*100
        ft_news<- appearRatio(ng_news[[i]], cntU)*100
        
        df1<- data.frame(Dataset= "blogs", ngram= paste0(i,"-gram"),
                         Percent= ft_blogs, AppearMoreThan= cntU)
        df2<- data.frame(Dataset= "twitter", ngram= paste0(i,"-gram"),
                         Percent= ft_twitter, AppearMoreThan= cntU)
        df3<- data.frame(Dataset= "news", ngram= paste0(i,"-gram"),
                         Percent= ft_news, AppearMoreThan= cntU)
        dfng<- rbind(dfng,df1,df2,df3)
        
    }
}

gf<- ggplot(data= dfng, aes(x= AppearMoreThan, y= Percent))
gf<- gf + geom_line()
gf<- gf + facet_grid(ngram~Dataset, scales="free", labeller= "label_both")
gf<- gf + theme(strip.text= element_text(size=14))
gf

```


These plots show that percentage of n-grams that appears more than x-times. As can be seen most of the n-grams appears only one or two time. For example, for one-grams in dataset blogs, only around 17% of the words appear more than twice, which means 83% of the words appear only twice or once. As the parameter n (for n-gram) increased, this percentage increased drastically. This means at 4- or 5-gram levels, most of the n-gram item appear only once, which may make 4- or 5-gram prediction model less capable.          


## Plans for Creating a Predictiion Algorithm

Based on the finding in this exploratory analysis, the plans for creating a prediction algorithm would be:

1. Data will be divided into training, cross-validation, and test sets. 
2. Data will be stratified sampled with a rate of 6% (temporarily) from each set.
3. N-Gram model will be created using sampled data.
    + Parameter n will be determined based on n-gram frequency info. as well as the model fit.
    + The strategy will be to construct 2-gram model first and then keep increasing  n to see if there is any performance change.  
    + Probability smoothing methods will be evaluated. 
4. Parameters will be optimized iteratively using cross-validation dataset to balance the prediction accuracy and runtime.

## Final Project Link

The final project was posted on the shinyapps.io and the link is:
https://jiahsuanlo.shinyapps.io/NextWord/
