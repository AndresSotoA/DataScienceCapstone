#============= load data =============
#load("sampledData.RData")

source("UtilityFunctions.R")

removeLastWord<- function(sentence)
{
    ws<- strsplit(sentence, split=" ", useBytes = TRUE)[[1]]
    paste(ws[1:(length(ws)-1)], collapse = " ")
}

#======== train 2 gram model ========

# get names without the last word
ng2name_minus<- sapply(names(ng2),removeLastWord)
# find index of ng1 
ind<- match(ng2name_minus, names(ng1))
# get previous ng1 count
ng2_minus<- ng1[ind]

# compute log probablity
ng2_p<- log10(ng2/ng2_minus)


