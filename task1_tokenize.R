tokenize<- function(fname)
{
    fid<- file(fname,"r")
    
    tokenList<- vector()
    pSample<- 0.1  # sampling probability 
    # read into rawdata
    ct<- 0
    while (length(oneLine <- readLines(fid, n = 1, warn = FALSE)) > 0) 
    {
        # sampling switch
        sampleYes<- rbinom(1,1,pSample)
        
        if (sampleYes==0) 
            next
        
        # process line    
        oneVec <- (strsplit(oneLine, " "))
        tokenList <- c(tokenList,unlist(oneVec))
        
        ct<- ct+1
        if(ct> 20)
            break
    } 
    close(fid)
    
    # remove non-alphabetic ending 
    tokenList<- gsub("[^[:alpha:]]+$","",tokenList)
    # remove non-alphanumeric beginning 
    tokenList<- gsub("^[^[:alnum:]]","",tokenList)
    
    # remove empty items
    ind<- grep("^$",tokenList)
    tokenList<- tokenList[-ind]
    
    # change to lower case
    tokenList<- tolower(tokenList)
    
    # return
    tokenList
}