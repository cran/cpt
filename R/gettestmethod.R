gettestmethod <-
function (method) 
{
    if (method == "logistic") {
        rval = function(Z, classifier, testistrain = FALSE) {
            tmp = exp(cbind(0, cbind(1, Z) %*% classifier))
            return(tmp/apply(tmp, 1, sum))
        }
    }
    else if (method == "logistic2") {
        rval = function(Z, classifier, testistrain = FALSE) {
            tmp = exp(cbind(0, model.matrix(~.^2, data = data.frame(Z)) %*% 
                classifier))
            return(tmp/apply(tmp, 1, sum))
        }
    }
    else if (method == "lda") {
        rval = function(Z, classifier, testistrain = FALSE) {
            return(predict(classifier, Z)$posterior)
        }
    }
    else if (method == "forest") {
        rval = function(Z, classifier, testistrain = FALSE) {
            if (testistrain) 
                return(predict(classifier, type = "prob"))
            else return(predict(classifier, Z, type = "prob"))
        }
    }
    else if (method == "glmnet") {
        rval = function(Z, classifier, testistrain = FALSE) {
            return(predict(classifier, newx = Z, type = "response"))
        }
    }
    else if (method == "glmnet2") {
        rval = function(Z, classifier, testistrain = FALSE) {
            Z = scale(Z, center = classifier[[1]], scale = classifier[[2]])
            Z = model.matrix(~.^2, data = data.frame(Z))[, -1, 
                drop = FALSE]
            return(predict(classifier[[3]], newx = Z, type = "response"))
        }
    }
}
