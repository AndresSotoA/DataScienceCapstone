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