gettestmethods <-
function (class.methods) 
{
    test.methods = list()
    for (i in 1:length(class.methods)) test.methods[[i]] = gettestmethod(class.methods[i])
    return(test.methods)
}
