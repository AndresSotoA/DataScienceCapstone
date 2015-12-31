# read whole file buffer using readChar
obtainBuffFast<- function(fname) 
{
    s = file.info( fname )$size 
    buf = readChar( fname, s, useBytes=T)
}

# read lines using readChar
obtainLinesFast<- function(fname) 
{
    s = file.info( fname )$size 
    buf = readChar( fname, s, useBytes=T)
    strsplit( buf,"\r\n",fixed=T,useBytes=T)[[1]]
}

# get word vector
obtainWordsFast<- function(fname) 
{
    s = file.info( fname )$size 
    buf = readChar( fname, s, useBytes=T)
    buf1<- gsub("[^a-zA-Z0-9]+|[[:space:]\r\n]+"," ",buf)
    
    words<- strsplit(buf1," ",fixed= T, useBytes=T)[[1]]
}

# randomly select lines
randomSelectLines<- function(lines, nSample=1e3)
{
    nL<- length(lines)
    ivec<- round(runif(nSample,min=1,max=nL))
    lines[ivec]
}

# striatified sampling
striatifiedSelectLines<- function(lines, nSample=1e3)
{
    nL<- length(lines)
    delta<- round(nL/nSample)
    
    istart<- runif(1,min=1,max= delta)
    
    ivec<- seq.int(istart,nL,length.out = nSample)
    lines[ivec]
    
}

# obtain xx% coverage
# input coverage percentage and return the the ratio
# (number of words/vocabulary) that 
# can achieve this coverage
VocabularyCoverage<- function(ngram, coverage= 90)
{
    ngs<- sort(ngram, decreasing = TRUE)
    
    tol = 0.01 # 1% tolerance
    # obtain 90% coverage vocabulary ratio
    cntUB<- length(ngs)
    cntLB<- 1
    iter<- 1
    while (TRUE)
    {
        iter<- iter+1 
        
        cnt<- cntLB+ round((cntUB-cntLB)/2)
        cover<- sum(ngs[1:cnt])/sum(ngs)
        if (cover > coverage/100+tol)
        {
            cntUB<- cnt
            
        }
        else if (cover < coverage/100-tol)
        {
            cntLB<- cnt
        }
        else
        {
            CoverageVocRatio<-  cnt/length(ngs)
            break
        }
        
        if (iter> 1e3)
        {
            CoverageVocRatio<-  cnt/length(ngs)
            break
        }
    }
    # return 
    CoverageVocRatio
}

## add sentence start and end symbol
# n is for n-gram
# if n=1, no pre and postfix
# if n=2, prefix is sstrt
# if n=3, prefix is sstrt sstrt
# postfix is strmnt
addSentenceSymbol<- function(lines, n=1)
{

    ct<- n-1
    pre<-""
    while ((ct)>0)
    {
        pre<- paste(pre,"sstrt")
        ct<- ct-1
    }
    
    
    # remove punctuation first
    lout<- lines
    
    # add symbol at beginning and end
    if (n>1)
    {
        lout<- paste(pre,lout,"strmnt")
        lout<- gsub("([.]{1}[[:space:]]+)([[:upper:]]{1})"
                ,paste(" strmnt",pre,"\\2")
                ,lout)
    }
    # return 
    lout
}
     

removeLastWord<- function(sentence)
{
    ws<- strsplit(sentence, split=" ", useBytes = TRUE)[[1]]
    paste(ws[1:(length(ws)-1)], collapse = " ")
}                        

# build n-word vector
build2Words<- function(ws)
{
    # number of words
    nw<- length(ws)
    wsa<- ws[1:(nw-1)]
    wsb<- ws[2:nw]
    # return
    paste(wsa,wsb)
}
build3Words<- function(ws)
{
    # number of words
    nw<- length(ws)
    wsa<- ws[1:(nw-2)]
    wsb<- ws[2:(nw-1)]
    wsc<- ws[3:nw]
    # return
    paste(wsa,wsb,wsc)
}
build4Words<- function(ws)
{
    # number of words
    nw<- length(ws)
    wsa<- ws[1:(nw-3)]
    wsb<- ws[2:(nw-2)]
    wsc<- ws[3:(nw-1)]
    wsd<- ws[4:nw]
    # return
    paste(wsa,wsb,wsc,wsd)
}

# ======= data prep  ===========
# convert into the format that can be accepted by glm_predict function
# [w1 w2 w3 w4 count appear]
# only take data with count >= countThreshold, default is 1 (take everything)
prep_ngHigh<- function(ngdf, countThreshold=1)
{
    # subsetting
    df<- ngdf[ngdf$value> countThreshold, ]
    
    # change name
    colnames(df)[2]<- "count"
    
    # tolower
    df$name<- tolower(df$name)
    
    # --- split words
    aa<- strsplit(as.character(df$name[1]), split=" ")[[1]]
    nw<- length(aa)  # get number of grams (n-gram)
    # create words name vector w1- w4..
    wvec<- sapply(1:nw, function(x) paste0("w",x)) 
    # separate then mutate with appear 
    df<- df %>%
        separate(name, into= wvec) %>%
        mutate(appear= 1)
    
    # return
    df
}
