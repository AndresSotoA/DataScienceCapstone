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



saveNGramCountHash<- function(words,fname)
{
    nw<- length(words)
    istart<- 1
    chunk<- 1e5
    
    ct<-1
    while (1)
    {
        iend<- min(istart+chunk, nw)
        
        print(sprintf("processing words %d to %d now...\n",istart, iend))
        
        # build hash table
        ht<- buildNGramCountHash(words[istart:iend])
        
        hk<- keys(ht)
        hv<- values(ht)
        
        # wirte to file
        fnow<- sprintf("%s_%d.txt",fname,ct)
        fid<- file(fnow,"w")
        for (i in seq(length(hk)))
        {
            writeLines(paste(hk[i],hv[i]), fid);
        }
        close(fid)
        
        # update index
        istart<- iend + 1
        ct<- ct + 1
        
        # termination condition
        if (iend>= nw)
            break
    }
}

# need to work on this
retrieveNGramCountHash<- function(fname)
{
    
}

