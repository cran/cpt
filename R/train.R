train <-
function (trnZ, trnT, train.methods) 
{
    classifiers = list()
    for (i in 1:length(train.methods)) {
        classifiers[[i]] = train.methods[[i]](trnZ, trnT)
    }
    return(classifiers)
}
