getteststat <-
function (Z, T, leaveout, train.methods, test.methods, metric, 
    ensemble.metric, leaveout.N) 
{
    if (leaveout == 0) {
        classifiers = train(Z, T, train.methods)
        return(applyclassifiers(Z, T, classifiers, test.methods, 
            metric, ensemble.metric, testistrain = TRUE))
    }
    else if ((leaveout == 1) & (leaveout.N == nrow(Z))) {
        metric.mat = matrix(NA, leaveout.N, length(train.methods) + 
            1)
        for (leaveout.i in 1:leaveout.N) {
            trnZ = Z[-leaveout.i, , drop = FALSE]
            trnT = T[-leaveout.i]
            tstZ = Z[leaveout.i, , drop = FALSE]
            tstT = T[leaveout.i]
            classifiers = train(trnZ, trnT, train.methods)
            metric.mat[leaveout.i, ] = applyclassifiers(tstZ, 
                tstT, classifiers, test.methods, metric, ensemble.metric)
        }
        return(apply(metric.mat, 2, mean))
    }
    else {
        metric.mat = matrix(NA, leaveout.N, length(train.methods) + 
            1)
        for (leaveout.i in 1:leaveout.N) {
            testset = rep(FALSE, length(T))
            for (i in 1:length(levels(T))) testset[sample(which(levels(T)[i] == 
                T), leaveout)] = TRUE
            trnZ = Z[!testset, , drop = FALSE]
            trnT = T[!testset]
            tstZ = Z[testset, , drop = FALSE]
            tstT = T[testset]
            classifiers = train(trnZ, trnT, train.methods)
            metric.mat[leaveout.i, ] = applyclassifiers(tstZ, 
                tstT, classifiers, test.methods, metric, ensemble.metric)
        }
        return(apply(metric.mat, 2, mean))
    }
}
