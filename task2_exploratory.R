library(hash)

buildNGramCountHash<- function(wordVector)
{
    # find unique words  
    uniqueWords<- unique(wordVector)
    
    freqHT<- hash(uniqueWords,0)
    
    for (wd in wordVector)
    {
        freqHT[[wd]]<- freqHT[[wd]]+1
    }
    
    # return hash table
    freqHT
}

buildNGramCountList<- function(wordVector)
{
    # find unique words  
    uniqueWords<- unique(wordVector)
    
    freqHT<- hash(uniqueWords,0)
    
    for (wd in wordVector)
    {
        freqHT[[wd]]<- freqHT[[wd]]+1
    }
    
    # return hash table
    freqHT
}


# ngram building funciton 
buildNGramVector <- function(tokens, n = 2, concatenator = "_", include.all = FALSE) {
    
    if (length(tokens) < n) 
        return(NULL)
    
    # start with lower ngrams, or just the specified size if include.all = FALSE
    start <- ifelse(include.all, 
                    1, 
                    ifelse(length(tokens) < n, 1, n))
    
    # set max size of ngram at max length of tokens
    end <- ifelse(length(tokens) < n, length(tokens), n)
    
    all_ngrams <- c()
    # outer loop for all ngrams down to 1
    for (width in start:end) {
        new_ngrams <- tokens[1:(length(tokens) - width + 1)]
        # inner loop for ngrams of width > 1
        if (width > 1) {
            for (i in 1:(width - 1)) 
                new_ngrams <- paste(new_ngrams, 
                                    tokens[(i + 1):(length(tokens) - width + 1 + i)], 
                                    sep = concatenator)
        }
        # paste onto previous results and continue
        all_ngrams <- c(all_ngrams, new_ngrams)
    }
    
    all_ngrams
}


# randomly select lines
randomSelectLines<- function(lines, nSample=1e3)
{
    nL<- length(lines)
    ivec<- round(runif(nSample,min=1,max=nL))
    lines[ivec]
}

randomSelectLines1<- function(lines, nSample=1e3)
{
    nL<- length(lines)
    delta<- round(nL/nSample)
    
    istart<- runif(1,min=1,max= delta)
    
    ivec<- seq.int(istart,nL,length.out = nSample)
    lines[ivec]
    
}