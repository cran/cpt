applyclassifiers <-
function (tstZ, tstT, classifiers, test.methods, metric, ensemble.metric, 
    testistrain = FALSE) 
{
    rval = rep(NA, length(classifiers) + 1)
    if (nrow(tstZ) == 1) {
        class.output = rep(NA, length(classifiers) * length(levels(tstT)))
        dim(class.output) = c(length(classifiers), length(levels(tstT)))
        for (i in 1:length(classifiers)) class.output[i, ] = test.methods[[i]](tstZ, 
            classifiers[[i]], testistrain = testistrain)
        for (i in 1:length(classifiers)) rval[i] = metric(class.output[i, 
            , drop = FALSE], tstT)
        dim(class.output) = c(1, dim(class.output))
        rval[length(classifiers) + 1] = ensemble.metric(class.output, 
            tstT)
    }
    else {
        class.output = rep(NA, nrow(tstZ) * length(classifiers) * 
            length(levels(tstT)))
        dim(class.output) = c(nrow(tstZ), length(classifiers), 
            length(levels(tstT)))
        for (i in 1:length(classifiers)) class.output[, i, ] = test.methods[[i]](tstZ, 
            classifiers[[i]], testistrain = testistrain)
        for (i in 1:length(classifiers)) rval[i] = metric(class.output[, 
            i, ], tstT)
        rval[length(classifiers) + 1] = ensemble.metric(class.output, 
            tstT)
    }
    return(rval)
}
