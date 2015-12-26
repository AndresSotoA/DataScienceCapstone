#============= load data =============
#load("sampledData.RData")

# =========== 2-gram model ===========
predict2<- function(ng2,sentence)
{
    s1<- tolower(sentence)
    s1<- addSentenceSymbol(s1,n=2)
    ws<- strsplit(s1, split=" ", useBytes = TRUE)
    
}
