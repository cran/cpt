getmetric <-
function (metric) 
{
    if (metric == "probability") {
        rval = function(prob, tstT) {
            indexmat = cbind(1:nrow(prob), tstT)
            return(mean(prob[indexmat]))
        }
    }
    else if (metric == "rate") {
        rval = function(prob, tstT) {
            indexmat = cbind(1:nrow(prob), tstT)
            temp = prob - apply(prob, 1, max) == 0
            temp = temp/apply(temp, 1, sum)
            return(mean(prob[indexmat]))
        }
    }
    else if (metric == "mse") {
        rval = function(prob, tstT) {
            indexmat = cbind(1:nrow(prob), tstT)
            prob[indexmat] = 1 - prob[indexmat]
            return(-mean(prob^2))
        }
    }
    return(rval)
}
