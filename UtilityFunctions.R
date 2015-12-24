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
