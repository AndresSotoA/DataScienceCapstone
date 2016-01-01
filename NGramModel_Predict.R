library(dplyr)
library(tidyr)
source("UtilityFunctions.R")

# ========== Predict interpolation ========
predictInterp <- function (df1,df2,df3,df4,sentence)
{
    # get words from sentence
    s1<- tolower(sentence)
    #s1<- addSentenceSymbol(s1,n=2)
    ws<- strsplit(s1, split=" ", useBytes = TRUE)[[1]]
    
    nWords<- length(ws)
    
    # extract last three words
    if (nWords>=3)
    {
        n<- 4
        wPrev<- ws[(nWords-n+2):nWords]
    }
    else
    {
        wPrev<- c(rep("sstrt",3-nWords),ws)
    }
    
    
    
    # ====== Interpolate by two gram ======
    # last word candidate
    wLast<- data.frame(w2= df1$w1, flag=1)
    # find matching count with last word candidates
    dftmp<- df2 %>% 
        filter(w1==wPrev[3]) %>%
        select(-w1) %>%
        mutate(ckn=count)
    
    # get total count
    count2<- sum(dftmp$count)
    
    c2_w4<- merge(wLast,dftmp, by.x = "w2", all.x = TRUE, all.y= FALSE, sort=FALSE) 
    # make NA words 0 count
    c2_w4[is.na(c2_w4$ckn),5]=0
    
    # ====== Interpolate by three gram ======
    # last word candidate
    wLast<- data.frame(w3= df1$w1, flag=1)
    # find matching count with last word candidates
    dftmp<- df3 %>% 
        filter(w1==wPrev[2],w2==wPrev[3]) %>%
        select(-w1,-w2) %>%
        mutate(ckn=count)
    
    # get total count
    count3<- sum(dftmp$count)
    
    c3_w4<- merge(wLast,dftmp, by.x = "w3", all.x = TRUE, all.y= FALSE, sort=FALSE) 
    # make NA words 0 count
    c3_w4[is.na(c3_w4$ckn),5]=0
    
    # ====== Interpolate by four gram ======
    # last word candidate
    wLast<- data.frame(w4= df1$w1, flag=1)
    # find matching count with last word candidates
    dftmp<- df4 %>% 
        filter(w1==wPrev[1],w2==wPrev[2],w3==wPrev[3]) %>%
        select(-w1,-w2, -w3) %>%
        mutate(ckn=count)
    
    # get total count
    count4<- sum(dftmp$count)
    
    c4_w4<- merge(wLast,dftmp, by.x = "w4", all.x = TRUE, all.y= FALSE, sort=FALSE) 
    # make NA words 0 count
    c4_w4[is.na(c4_w4$ckn),5]=0
    
    
    # ==== compute p-value
    V= nrow(df1)
    p1<- (df1$count+1)/(sum(df1$count)+V)
    p2<- (c2_w4$ckn+1)/(count2+V)
    p3<- (c3_w4$ckn+1)/(count3+V)
    p4<- (c4_w4$ckn+1)/(count4+V)
    
    pv<- (0.2*p1/(max(p1)+1e-20))+(0.3*p2/(max(p2)+1e-20)) 
    +(0.3*p3/(max(p3)+1e-20)) +(0.2*p4/(max(p4)+1e-20))
    
    outdf<- data.frame(kw= c2_w4$w2, pkn= pv) %>%
        arrange(desc(pkn))
    
    # return
    as.character(outdf$kw[1:8])
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
    if (nWords>=3)
    {
        n<- 4
        wPrev<- ws[(nWords-n+2):nWords]
    }
    else
    {
        wPrev<- c(rep("sstrt",3-nWords),ws)
    }
    
    # ====== Scalar parameter calculation ======
    d= 0.75
    
    size1gram<- nrow(df1)
    size2gram<- nrow(df2)
    
    count1gram<- sum(df1$count)
    count2gram<- sum(df2$count)
    count3gram<- sum(df3$count)  
    
    # cp appear
    cp_w3<- sum(df2$w1==wPrev[3])
    cp_w23<- sum((df3$w1==wPrev[2])&(df3$w2==wPrev[3]))
    cp_w123<- sum((df4$w1==wPrev[1])&(df4$w2==wPrev[2])&(df4$w3==wPrev[3]))
    
    
    # ======  N=1 & 2  ==========
    # last word candidate
    wLast<- data.frame(w2= df1$w1, flag=1)
    # find appear size ending with last word candidates
    dftmp<- df2 %>% group_by(w2) %>%
        summarise(ckn= sum(appear))
    ckn2_w4<- merge(wLast,dftmp, by.x = "w2", all.x = TRUE, all.y= FALSE) 
    # make NA words 0 count
    ckn2_w4[is.na(ckn2_w4$ckn),3]=0

    # get total count
    count2<- sum(dftmp$count)
    
    # ======  N=3  ==========
    # last word candidate
    wLast<- data.frame(w3= df1$w1, flag=1)
    # find appear size ending with last word candidates
    dftmp<- df3 %>% group_by(w3) %>%
        summarise(ckn= sum(appear))
    ckn3_w4<- merge(wLast,dftmp, by.x = "w3", all.x = TRUE, all.y= FALSE) 
    # make NA words 0 count
    ckn3_w4[is.na(ckn3_w4$ckn),3]=0
    
    # get total count
    count3<- sum(dftmp$count)
    
    # ======  N=4  ==========
    # last word candidate
    wLast<- data.frame(w4= df1$w1, flag=1)
    # find matching count with last word candidates
    dftmp<- df4 %>% 
        filter(w1==wPrev[1], w2==wPrev[2], w3== wPrev[3]) %>%
        select(-w1,-w2,-w3) %>%
        mutate(ckn=count)
    
    ckn4_w4<- merge(wLast,dftmp, by.x = "w4", all.x = TRUE, all.y= FALSE) 
    # make NA words 0 count
    ckn4_w4[is.na(ckn4_w4$ckn),5]=0
    
    # ====== Interpolate by two gram ======
    # last word candidate
    wLast<- data.frame(w2= df1$w1, flag=1)
    # find matching count with last word candidates
    dftmp<- df2 %>% 
        filter(w1==wPrev[3]) %>%
        select(-w1) %>%
        mutate(ckn=count)
    
    c2_w4<- merge(wLast,dftmp, by.x = "w2", all.x = TRUE, all.y= FALSE) 
    # make NA words 0 count
    c2_w4[is.na(c2_w4$ckn),5]=0
    
    # ====== Interpolate by three gram ======
    # last word candidate
    wLast<- data.frame(w3= df1$w1, flag=1)
    # find matching count with last word candidates
    dftmp<- df3 %>% 
        filter(w1==wPrev[2],w2==wPrev[3]) %>%
        select(-w1,-w2) %>%
        mutate(ckn=count)
    
    c3_w4<- merge(wLast,dftmp, by.x = "w3", all.x = TRUE, all.y= FALSE) 
    # make NA words 0 count
    c3_w4[is.na(c3_w4$ckn),5]=0
    
    
    # ===== Recursively combine =======
    pkn1<- ckn2_w4$ckn/size2gram
    pkn2<- (sapply(ckn2_w4$ckn, function(x){max(x-d,0)})/size1gram) + (d/size1gram*cp_w3*pkn1)
    pkn3<- (sapply(ckn3_w4$ckn, function(x){max(x-d,0)})/size2gram) + (d/size2gram*cp_w23*pkn2)
    pkn4<- (sapply(ckn4_w4$ckn, function(x){max(x-d,0)})/count3gram) + (d/count3gram*cp_w123*pkn3)
    
    #pkndf<- data.frame(kw= ckn2_w4$w2, pkn= pkn4) %>%
    #    arrange(desc(pkn))
    V= nrow(df1)
    p2<- (c2_w4$ckn+1)/(count2+V)
    p3<- (c3_w4$ckn+1)/(count3+V)
    pv<- (0.6*pkn4/(max(pkn4)+1e-10))+(0.3*p3/(max(p3)+1e-10)) +(0.1*p2/(max(p2)+1e-10))
    pkndf<- data.frame(kw= ckn2_w4$w2, pkn= pv) %>%
        arrange(desc(pkn))
    
    # return
    as.character(pkndf$kw[1:8])
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

