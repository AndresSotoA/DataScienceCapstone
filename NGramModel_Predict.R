library(dplyr)
library(tidyr)
source("UtilityFunctions.R")

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
predictKN <- function (df1,df2,df3,df4,sentence)
{
    # get words from sentence
    s1<- tolower(sentence)
    #s1<- addSentenceSymbol(s1,n=2)
    ws<- strsplit(s1, split=" ", useBytes = TRUE)[[1]]
    
    nWords<- length(ws)
    
    # extract last three words
    n<- 4
    wPrev<- ws[(nWords-n+2):nWords]
    
    # last word candidate
    wLast<- as.character(ng1$name[1:50])
    
    
    # count n-1 (3)
    ind<- grep(wPrev, ng3$name)
    c_minus<- ng3df[ind,2] + 1
    
    # get count now
    c_now<- sapply(wLast, function(w, wPrev)
        {
            str1<- paste0("^",wPrev," ",w, "$")
            ind<- grep(str1, ng4$name)
            if (length(ind)>0)
                cc<- sum(ng4[ind,2])
            else
                cc<- 0
            max((cc-0.75),0)  # return
        }, wPrev)
    
    # get the set w:c(wi-1,w)>0
    w<- sapply(wLast, function(w)
    {
        str1<- paste0(" ",w, "$")
        sum(grep(str1, ng4$name))
    })
    
    
    # compute lambda
    d<- 0.75 # fixed discount
    lambda<- d/c_minus*w
    
    
    # extract last three words
    n<- 3
    wPrev2<- paste(ws[(nWords-n+2):nWords], collapse=" ")
    
    # calculate KN probability for 3-gram
    pkn_minus<- sapply(wLast, function(w, wPrev2)
        {
            str1<- paste0("^",wPrev2,w,"$")
            wn<- sum(grep(str1, ng3$name)) + 1
            str1<- paste0("^",wPrev2," ")
            wd<- sum(grep(str1, ng3$name)) + 1
            wn/wd
        }, wPrev2)
    
    pkn_now<- (cnow/cminus) + lambda*pkn_minus 
        
    imax<- which.max(pkn_now)
    
    # return
    wLast[imax]
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

