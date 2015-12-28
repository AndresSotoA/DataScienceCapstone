# =========== 2-gram model ===========
predict2<- function(ng2,sentence)
{
    s1<- tolower(sentence)
    #s1<- addSentenceSymbol(s1,n=2)
    ws<- strsplit(s1, split=" ", useBytes = TRUE)[[1]]
    
    nWords<- length(ws)
    
    # get last words
    n<- 2
    wlast<- paste(ws[(nWords-n+2):nWords], collapse=" ")
    
    # find index that starts with wlast
    expr<- paste0("^",wlast," ")
    ind<- grep(expr,names(ng2))
    
    # find most count item
    nmax<- which.max(ng2[ind])
    
    # return last word
    wmax<- names(nmax)
    
    wmax
}

# =========== 3-gram model ===========
predict3<- function(ng3,sentence)
{
    s1<- tolower(sentence)
    #s1<- addSentenceSymbol(s1,n=2)
    ws<- strsplit(s1, split=" ", useBytes = TRUE)[[1]]
    
    nWords<- length(ws)
    
    # get last words
    n<- 3
    wlast<- paste(ws[(nWords-n+2):nWords], collapse=" ")
    
    # find index that starts with wlast
    expr<- paste0("^",wlast," ")
    ind<- grep(expr,names(ng3))
    
    # find most count item
    nmax<- which.max(ng3[ind])
    
    # return last word
    wmax<- names(nmax)
    
    wmax
}

# =========== 4-gram model ===========
predict4<- function(ng4,sentence)
{
    s1<- tolower(sentence)
    #s1<- addSentenceSymbol(s1,n=2)
    ws<- strsplit(s1, split=" ", useBytes = TRUE)[[1]]
    
    nWords<- length(ws)
    
    # get last words
    n<- 4
    wlast<- paste(ws[(nWords-n+2):nWords], collapse=" ")
    
    # find index that starts with wlast
    expr<- paste0("^",wlast)
    ind<- grep(expr,names(ng4))
    
    # find most count item
    nmax<- which.max(ng4[ind])
    
    # return last word
    wmax<- names(nmax)
    
    wmax
}

