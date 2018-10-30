getensemble.metric <-
function (ensemble.metric) 
{
    if (ensemble.metric == "vote") {
        rval = function(class.output, tstT) {
            temp = (class.output - as.vector(apply(class.output, 
                c(1, 2), max))) == 0
            temp = temp/as.vector(apply(temp, c(1, 2), sum))
            votemat = apply(temp, c(1, 3), sum)
            indexmat = cbind(1:nrow(votemat), tstT)
            temp = votemat - apply(votemat, 1, max) == 0
            temp = temp/apply(temp, 1, sum)
            return(mean(votemat[indexmat]))
        }
    }
    if (ensemble.metric == "mean.prob") {
        rval = function(class.output, tstT) {
            meanprob = apply(class.output, c(1, 3), mean)
            indexmat = cbind(1:nrow(meanprob), tstT)
            return(mean(meanprob[indexmat]))
        }
    }
    if (ensemble.metric == "mean.log") {
        rval = function(class.output, tstT) {
            meanprob = apply(class.output, c(1, 3), mean)
            indexmat = cbind(1:nrow(meanprob), tstT)
            return(mean(log(meanprob[indexmat] + 1e-04)))
        }
    }
    if (ensemble.metric == "mean.mse") {
        rval = function(class.output, tstT) {
            meanprob = apply(class.output, c(1, 3), mean)
            indexmat = cbind(1:nrow(meanprob), tstT)
            meanprob[indexmat] = 1 - meanprob[indexmat]
            return(-mean(meanprob^2))
        }
    }
    return(rval)
}
