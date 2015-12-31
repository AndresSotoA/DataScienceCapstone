# load utility functions
source("UtilityFunctions.R")
library(dplyr)

# reduce data function
# keep data that their value >= threshold
reduceData<- function(df, threshold=1)
{
    df<- df %>%
        group_by(name) %>%
        summarise(value= sum(value)) %>%
        arrange(desc(value)) %>%
        filter(value >= threshold)
    
}


# compute number of lines to be sampled
samplePct<- 1  # 1% sample rate

nIter<- 100/ samplePct

# output file name
fout<- "./sampledData/AllData.RData"


filect<- 0  # file read counter

ng1df<- data.frame()
ng2df<- data.frame()
ng3df<- data.frame()
ng4df<- data.frame()


for (i in c(25,75))#seq(1,nIter)) 
{
    # file name
    fname<- sprintf('./sampledData/sampleData%d.RData',i)
    
    if (!file.exists(fname))
        next
    
    sout<- sprintf('Reading Iter %d of %d...',i,nIter)
    print(sout)
    
    # get sampling now
    filect<- filect+1
    load(fname)
    
    # remove non english and number character
    ind<- grep("[^a-zA-Z0-9[:punct:][:space:]]", names(ng1))
    ng1a<- ng1[-ind]
    ind<- grep("[^a-zA-Z0-9[:punct:][:space:]]", names(ng2))
    ng2a<- ng2[-ind]
    ind<- grep("[^a-zA-Z0-9[:punct:][:space:]]", names(ng3))
    ng3a<- ng3[-ind]
    ind<- grep("[^a-zA-Z0-9[:punct:][:space:]]", names(ng4))
    ng4a<- ng4[-ind]
    
    # combine data now
    df1<- data.frame(name= names(ng1a), value= ng1a)
    df2<- data.frame(name= names(ng2a), value= ng2a)
    df3<- data.frame(name= names(ng3a), value= ng3a)
    df4<- data.frame(name= names(ng4a), value= ng4a)
    
    ng1df<- rbind(ng1df,df1)
    ng2df<- rbind(ng2df,df2)
    ng3df<- rbind(ng3df,df3)
    ng4df<- rbind(ng4df,df4)
    
    # make names unique
    if ((filect%%5==0) || (filect==98))
    {
        sout<- sprintf('Combine data now...')
        print(sout)
        th<- round(filect/5)
        ng1df<- reduceData(ng1df,th)
        print("unigram...")
        ng2df<- reduceData(ng2df,th)
        print("2-gram...")
        ng3df<- reduceData(ng3df,th)
        print("3-gram...")
        ng4df<- reduceData(ng4df,th)
        print("4-gram...")
        # ======== save data to files ==========
        print("save file now...")
        save(ng1df,ng2df,ng3df,ng4df, file= fout)
    }
    
    

}
