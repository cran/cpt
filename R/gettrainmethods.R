gettrainmethods <-
function (class.methods) 
{
    train.methods = list()
    for (i in 1:length(class.methods)) train.methods[[i]] = gettrainmethod(class.methods[i])
    return(train.methods)
}
