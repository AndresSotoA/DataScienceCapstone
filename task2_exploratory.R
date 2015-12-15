library(hash)

build1GramHash<- function(wordVector)
{
    freqHT<- hash()
    for (wd in wordVector)
    {
        if (has.key(wd,freqHT))
            freqHT[[wd]]<- freqHT[[wd]]+1
        else
            freqHT[[wd]]<- 1
    }
    
    # return hash table
    freqHT
}

# n-gram hash table generation
# wordVector: a vector of words
# n: find n-gram
# return n-gram hash table
buildNGramHash<- function(wordVector,n)
{
    freqHT<- hash()
    wdPrev<- ""
    ct<- 0
    for (wd in wordVector)
    {
        ct<- ct+1
        if (ct< n)
        {
            wdPrev<- paste(wdPrev,wd)
            # remove first space if any
            wdPrev<- gsub("^(\\s)(.*)$","\\2",wdPrev)
            next
        }
        # build word pair
        nGram<- paste(wdPrev,wd)
        # remove first space if any
        nGram<- gsub("^(\\s)(.*)$","\\2",nGram)
        
        # put in hash
        if (has.key(nGram,freqHT))
            freqHT[[nGram]]<- freqHT[[nGram]]+1
        else
            freqHT[[nGram]]<- 1
        
        # update previous wd
        wdPrev<- gsub("^(.*)\\s(.*)$","\\2",nGram)
    }
    # return hash
    freqHT
}

