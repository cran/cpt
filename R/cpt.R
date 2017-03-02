cpt <-
function (Z, T, leaveout = 0, class.methods = "forest", metric = "rate", 
    paired = FALSE, perm.N = 1000, leaveout.N = 100) 
{
    T = as.factor(T)
    train.methods = gettrainmethods(class.methods)
    test.methods = gettestmethods(class.methods)
    if (is.character(metric)) 
        metric = getmetric(metric)
    if ((leaveout > 0) && (leaveout < 1)) 
        leaveout = ceiling(min(table(T)) * leaveout)
    nulldist = rep(0, perm.N)
    teststat = getteststat(Z, T, leaveout, train.methods, test.methods, 
        metric, leaveout.N)
    if (paired) {
        for (i in 1:perm.N) {
            newT = as.numeric(T)
            newT[T == 0] = rbinom(length(T)/2, 1, 0.5)
            newT[T == 1] = 1 - newT[T == 0]
            newT = as.factor(newT)
            nulldist[i] = getteststat(Z, newT, leaveout, train.methods, 
                test.methods, metric, leaveout.N)
        }
    }
    else {
        for (i in 1:perm.N) {
            Z = Z[sample(nrow(Z)), , drop = FALSE]
            nulldist[i] = getteststat(Z, T, leaveout, train.methods, 
                test.methods, metric, leaveout.N)
        }
    }
    pval = sum(nulldist >= teststat)/perm.N
    return(list(pval = pval, teststat = teststat, nulldist = nulldist))
}
