# =========== calculate pkn ========
computePKN<- function(i, ng4, c_m, lamb)
{
    cnow<- ng4[i]
    
    # P continuation
    # w(i-1): c(w(i-1)wi)>0
    ws<- strsplit(names(ng4[i]), split=" ", useBytes = TRUE)[[1]]
    str1<- paste0(ws[length(ws)],"$")
    itmp<- grep(str1, names(ng4))
    w_minus<- sum(ng4[itmp]>0)
    
    # sigma_wi w(i-1): c(w(i-1)wi)>0
    wall<- sum(ng4[itmp])
    pcont<- w_minus/wall
    
    # calculate pkn
    (max(cnow,0)/c_m) + (lamb*pcont)
    
}


# =========== Predict KN 4 gram ==========
predictKN <- function (ng2,ng3,ng4,sentence)
{
    # get words from sentence
    s1<- tolower(sentence)
    #s1<- addSentenceSymbol(s1,n=2)
    ws<- strsplit(s1, split=" ", useBytes = TRUE)[[1]]
    
    nWords<- length(ws)
    
    # extract last three words
    n<- 4
    wlast<- paste(ws[(nWords-n+2):nWords], collapse=" ")
    
    # get all possible 4-words
    str1<- paste0("^",wlast)
    ind4<- grep(str1, names(ng4))
    
    # set w:c(wi-1,w)>0
    wc <- sum(ng4[ind4]>0)
    
    # set parameters
    ind3<- match(wlast, names(ng3))
    c_minus<- ng3[ind3]
    
    # compute lambda
    d<- 0.75 # fixed discount
    lambda<- d/c_minus*wc
    
    
    
    # calculate KN probability for each 4-gram
    pkn<- sapply(ind4, function(i){computePKN(i,ng4,c_minus,lambda)})
    
    
    itmp<- which.max(pkn)
    imax<- ind4[itmp]
    
    # return
    names(ng4[imax])
    
    
}



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
    expr<- paste0("^",wlast," .*$")
    ind<- grep(expr,names(ng4))
    
    # find most count item
    nmax<- which.max(ng4[ind])
    
    # return last word
    wmax<- names(nmax)
    
    wmax
}

# =========== 5-gram model ===========
predict5<- function(ng5,sentence)
{
    s1<- tolower(sentence)
    #s1<- addSentenceSymbol(s1,n=2)
    ws<- strsplit(s1, split=" ", useBytes = TRUE)[[1]]
    
    nWords<- length(ws)
    
    # get last words
    n<- 5
    wlast<- paste(ws[(nWords-n+2):nWords], collapse=" ")
    
    # find index that starts with wlast
    expr<- paste0("^",wlast)
    ind<- grep(expr,names(ng5))
    
    # find most count item
    nmax<- which.max(ng5[ind])
    
    # return last word
    wmax<- names(nmax)
    
    wmax
}

