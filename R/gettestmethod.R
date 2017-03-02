gettestmethod <-
function (method) 
{
    if (method == "logistic") {
        rval = function(Z, classifier) {
            tmp = exp(cbind(0, cbind(1, Z) %*% classifier))
            return(tmp/apply(tmp, 1, sum))
        }
    }
    else if (method == "logistic2") {
        rval = function(Z, classifier) {
            tmp = exp(cbind(0, model.matrix(~.^2, data = data.frame(Z)) %*% 
                classifier))
            return(tmp/apply(tmp, 1, sum))
        }
    }
    else if (method == "lda") {
        rval = function(Z, classifier) {
            return(predict(classifier, Z)$posterior)
        }
    }
    else if (method == "forest") {
        rval = function(Z, classifier) {
            return(predict(classifier, Z, type = "prob"))
        }
    }
}
