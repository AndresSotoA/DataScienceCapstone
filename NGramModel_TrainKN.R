# Train with Kneser-Ney interpolation method

#============= load data =============
load("sampledData.RData")

source("UtilityFunctions.R")

# ======= set up parameters =========
d<- 0.75


#======== train 2 gram model ========

# get names without the last word
ng2name_minus<- sapply(names(ng2),removeLastWord)
# find index of ng1 
ind<- match(ng2name_minus, names(ng1))
# get previous ng1 count
ng2_minus<- ng1[ind]

# compute lambda
lambda<- 0.1/ng2



# compute log probablity
V<- length(ng1)
ng2_p<- log10((ng2+1)/(ng2_minus+V))

# ========= train 3-gram model ==========

# get names without the last word
ng3name_minus<- sapply(names(ng3), removeLastWord)
# find index of ng2 
ind<- match(ng3name_minus, names(ng2))
# get previous ng2 count
ng3_minus<- ng2[ind]

# compute log probablity
V<- length(ng2)
ng3_p<- log10((ng3+1)/(ng3_minus+V))

# ========= train 4-gram model ==========

# get names without the last word
ng4name_minus<- sapply(names(ng4), removeLastWord)
# find index of ng3 
ind<- match(ng4name_minus, names(ng3))
# get previous ng3 count
ng4_minus<- ng3[ind]

# compute log probablity
V<- length(ng3)
ng4_p<- log10((ng4+1)/(ng4_minus+V))

# ========= train 5-gram model ==========

# get names without the last word
#ng5name_minus<- sapply(names(ng5), removeLastWord)
# find index of ng4 
#ind<- match(ng5name_minus, names(ng4))
# get previous ng3 count
#ng5_minus<- ng4[ind]

# compute log probablity
#V<- length(ng4)
#ng5_p<- log10((ng5+1)/(ng5_minus+V))


# ======== save data to files ==========
save(ng2_p,ng3_p,ng4_p
     ,file="trainedData.RData")

