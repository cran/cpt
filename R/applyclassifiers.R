applyclassifiers <-
function (tstZ, tstT, classifiers, test.methods, metric) 
{
    metrics = rep(0, length(classifiers))
    for (i in 1:length(classifiers)) {
        metrics[i] = metric(test.methods[[i]](tstZ, classifiers[[i]]), 
            tstT)
    }
    return(metrics)
}
