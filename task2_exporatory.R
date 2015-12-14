library(hash)

buildFreqHash<- function(wordVector)
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