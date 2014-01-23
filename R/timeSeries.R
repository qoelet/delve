# Some functions for time series calculations

dp = function(x) {
    d <- diff(x)
    acf(d)
    print("Mean:")
    print(mean(d))
    print("Interval:")
    print(mean(d, na.rm=TRUE) + c(-2,2) * (sd(d, na.rm=TRUE)/sqrt(length(d))))
    return(mean(d, na.rm=TRUE))
}