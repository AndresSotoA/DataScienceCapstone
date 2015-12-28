#============= load data =============
load("sampledData.RData")

#source("UtilityFunctions.R")

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

# ========= train 3-gram model ==========

# get names without the last word
ng3name_minus<- sapply(names(ng3), removeLastWord)
# find index of ng2 
ind<- match(ng3name_minus, names(ng2))
# get previous ng2 count
ng3_minus<- ng2[ind]

# compute log probablity
ng3_p<- log10(ng3/ng3_minus)

# ========= train 4-gram model ==========

# get names without the last word
ng4name_minus<- sapply(names(ng4), removeLastWord)
# find index of ng3 
ind<- match(ng4name_minus, names(ng3))
# get previous ng3 count
ng4_minus<- ng3[ind]

# compute log probablity
ng4_p<- log10(ng4/ng4_minus)

# ========= train 5-gram model ==========

# get names without the last word
ng5name_minus<- sapply(names(ng5), removeLastWord)
# find index of ng4 
ind<- match(ng5name_minus, names(ng4))
# get previous ng3 count
ng5_minus<- ng4[ind]

# compute log probablity
ng5_p<- log10(ng5/ng5_minus)

# ======== save data to files ==========
save(ng2_p,ng3_p,ng4_p,ng5_p
     ,file="trainedData.RData")

