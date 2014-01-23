# Some functions for time series calculations

dp = function(x) {
    d <- diff(x)
    acf(d)
    print("Mean:")
    print(mean(d))
    print("Interval:")
    print(mean(d) + c(-2,2) * (sd(d)/sqrt(length(d))))
    return(d)
}