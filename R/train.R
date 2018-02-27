train <-
function (trnZ, trnT, train.methods) 
{
    rval = list()
    for (i in 1:length(train.methods)) rval[[i]] = train.methods[[i]](trnZ, 
        trnT)
    return(rval)
}
