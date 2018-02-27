getcombmethod <-
function (comb.method) 
{
    if (comb.method == "fisher") {
        rval = function(x) {
            return(mean(log(x)))
        }
    }
    if (comb.method == "min") {
        rval = min
    }
    return(rval)
}
