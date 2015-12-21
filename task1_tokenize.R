# read lines from file
obtainLines <- function(fname)
{
    fid<- file(fname,"r")
    lines<- readLines(fid,warn=FALSE)
    close(fid)
    
    # return
    lines
}

# read lines using readChar
obtainLinesFast<- function(fname) 
{
  s = file.info( fname )$size 
  buf = readChar( fname, s, useBytes=T)
  strsplit( buf,"\r\n",fixed=T,useBytes=T)[[1]]
}

# cleaning words vector
# 
# tokenList: words vector
# return cleaned tokenList
tokenTreatment<- function(tokenList)
{
    # remove non-alphabetic ending 
    tokenList<- gsub("[^[:alpha:]]+$","",tokenList)
    # remove non-alphanumeric beginning 
    tokenList<- gsub("^[^[:alnum:]]","",tokenList)
    
    # remove empty items
    ind<- grep("^$",tokenList)
    if (length(ind)!=0)
        tokenList<- tokenList[-ind]
    
    # change to lower case
    tokenList<- tolower(tokenList)
    
    # return
    tokenList
}

# tokenize one line 
#
# oneLine: one line of words
tokenizeOneLine<- function(oneLine)
{
    wordList<- strsplit(oneLine," ")
    wordList<- tokenTreatment(unlist(wordList))
    # add line start and line end
    wordList<- c("<s>",wordList,"</s>")
    # return
    wordList
}

# tokenize lines
tokenizeFromLines<- function(lines)
{
    tokenList<- vector()
    for (oneLine in lines)
    {
        lst<- tokenizeOneLine(oneLine)
        tokenList<- c(tokenList,lst)
    }
    # return
    tokenList
}

# tokenize from reading a file
tokenizeFromFile<- function(fname)
{
    fid<- file(fname,"r")
    
    tokenList<- vector()
    pSample<- 0.1  # sampling probability 
    # read into rawdata
    while (length(oneLine <- readLines(fid, n = 1, warn = FALSE)) > 0) 
    {
        # sampling switch
        sampleYes<- 1 #rbinom(1,1,pSample)
        
        if (sampleYes==0) 
            next
        
        # process line    
        oneLineToken<- tokenizeOneLine(oneLine)
        tokenList <- c(tokenList,oneLineToken)
    } 
    close(fid)
    
    
    # return
    tokenList
}

