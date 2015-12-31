library(dplyr)
library(tidyr)

source("UtilityFunctions.R")

# ====== glm predict ===========
glm_predict<- function(df1,df2,df3,df4,sentence)
{
    # get words from sentence
    s1<- tolower(sentence)
    #s1<- addSentenceSymbol(s1,n=2)
    ws<- strsplit(s1, split=" ", useBytes = TRUE)[[1]]
    
    nWords<- length(ws)
    
    # extract last three words
    wPrev<- ws[(nWords-2):nWords]
    
    
    # === 4-words sequence t4 [1001] ===
    # last word candidate
    wLast<- data.frame(w4= df1$w1, flag=1)
    wLast<- wLast[duplicated(wLast),]  # make it unique
    
    # form t4 df
    t4df<- df4 %>%
        group_by(w1,w4) %>%
        summarise(count= sum(count), appear= sum(appear)) %>%
        filter(w1== wPrev[1])

    # use candidate word to merge
    t4df<-  merge(wLast,t4df, by.x = "w4", all.x = TRUE, all.y= FALSE)
    # make NA words 0 count
    t4df[is.na(t4df$w1),4:5]=0
    
    # === 3-words sequence t3 [101] ===
    # last word candidate
    wLast<- data.frame(w3= df1$w1, flag=1)
    wLast<- wLast[duplicated(wLast),]  # make it unique
    
    # form t3 df
    t3df<- df3 %>%
        group_by(w1,w3) %>%
        summarise(count= sum(count), appear= sum(appear)) %>%
        filter(w1== wPrev[2])
    
    # use candidate word to merge
    t3df<-  merge(wLast,t3df, by.x = "w3", all.x = TRUE, all.y= FALSE)
    # make NA words 0 count
    t3df[is.na(t3df$w1),4:5]=0
    
    # === 2-words sequence t2 [11] ===
    # last word candidate
    wLast<- data.frame(w2= df1$w1, flag=1)
    wLast<- wLast[duplicated(wLast),]  # make it unique
    
    # form t2 df
    t2df<- df2 %>%
        group_by(w1,w2) %>%
        summarise(count= sum(count), appear= sum(appear)) %>%
        filter(w1== wPrev[3])
    # use candidate word to merge
    t2df<- merge(wLast,t2df, by.x = "w2", all.x = TRUE, all.y= FALSE)
    # make NA words 0 count
    t2df[is.na(t2df$w1),4:5]=0
    
    # ========= counts with input starting words =====
    # 3-gram with starting words
    c3df<- df3 %>%
        group_by(w1) %>%
        summarise(count= sum(count), appear= sum(appear)) %>%
        filter(w1== wPrev[1])
    # 2-gram with starting words
    c2df<- df2 %>%
        group_by(w1) %>%
        summarise(count= sum(count), appear= sum(appear)) %>%
        filter(w1== wPrev[2])
    # 1-gram with starting words
    c1df<- df1 %>%
        group_by(w1) %>%
        summarise(count= sum(count), appear= sum(appear)) %>%
        filter(w1== wPrev[3])
    
    # ======== calculate the probability now =======
    # counts
    c1<- c1df$count[1]
    c2<- c2df$count[1]
    c3<- c3df$count[1]
    
    # compute p, add a flag, and remove end word column
    t2df<- mutate(t2df, p= log(count/c1), kw= w2, flag=2) %>% select(-w2)
    t3df<- mutate(t3df, p= log(count/c2), kw= w3, flag=3) %>% select(-w3)
    t4df<- mutate(t4df, p= log(count/c3), kw= w4, flag=4) %>% select(-w4)
    # combine and average
    tall<- rbind(t2df,t3df,t4df)
    ansdf<- tall %>%
        group_by(kw) %>%
        summarise(pAvg= mean(p))
    
    
    # ======== get candidate ==========
    ansdf <- arrange(ansdf, desc(pAvg))
    
    # return 
    as.character(ansdf$kw[1:8])
    
}