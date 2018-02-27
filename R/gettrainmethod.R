gettrainmethod <-
function (method) 
{
    if (method == "logistic") {
        rval = function(Z, T) {
            if (length(levels(T)) == 2) 
                return(matrix(coefficients(multinom(T ~ Z, trace = FALSE))))
            else return(t(coefficients(multinom(T ~ Z, trace = FALSE))))
        }
    }
    else if (method == "logistic2") {
        rval = function(Z, T) {
            if (length(levels(T)) == 2) 
                return(matrix(coefficients(multinom(T ~ .^2, 
                  data = data.frame(Z), trace = FALSE))))
            else return(t(coefficients(multinom(T ~ .^2, data = data.frame(Z), 
                trace = FALSE))))
        }
    }
    else if (method == "lda") {
        rval = function(Z, T) {
            gn = length(levels(T))
            return(lda(Z, T, prior = rep(1/gn, gn), tol = 1e-05))
        }
    }
    else if (method == "forest") {
        rval = function(Z, T) {
            return(randomForest(Z, T))
        }
    }
    else if (method == "glmnet") {
        rval = function(Z, T) {
            return(cv.glmnet(Z, T, family = "multinomial"))
        }
    }
    else if (method == "glmnet2") {
        rval = function(Z, T) {
            means = apply(Z, 2, mean)
            sds = apply(Z, 2, sd)
            Z = scale(Z)
            Z = as.matrix(model.matrix(~.^2, data = data.frame(Z))[, 
                -1])
            return(list(means, sds, cv.glmnet(Z, T, family = "multinomial")))
        }
    }
    return(rval)
}
