# Collection of outlier detection functions
# The below functions are written based on: http://exploringdatablog.blogspot.sg/2013/02/finding-outliers-in-numerical-data.html
# All functions takes a dataframe, a column name and optionally a threshold value (see resource above for explanation on default threshold values)

# Simple implementation of Hampel identifier 
hampel_id = function(df, x, threshold = 3) {
    x_mad <- mad(df[[x]], na.rm=TRUE)
    x_median <- median(df[[x]], na.rm=TRUE)
    lower <- x_median - (threshold * x_mad)
    upper <- x_median + (threshold * x_mad)
    res <- df[df[[x]] > upper | df[[x]] < lower,]
    return(res)
}

# Extreme Studentized deviation
esd = function(df, x, threshold = 3) {
    x_mean <- mean(df[[x]], na.rm=TRUE)
    x_sd <- sd(df[[x]], na.rm=TRUE)
    lower <- x_mean - (threshold * x_sd)
    upper <- x_mean + (threshold * x_sd)
    res <- df[df[[x]] > upper | df[[x]] < lower,]
    return(res)
}

# Boxplot rule detection
boxplot_detect = function(df, x, threshold = 1.5) {
    quartiles <- quantile(df[[x]])
    q1 <- quartiles["25%"]
    q3 <- quartiles["75%"]
    iqd <- q3 - q1
    lower <- q1 - (threshold * iqd)
    upper <- q3 + (threshold * iqd)
    res <- df[df[[x]] > upper | df[[x]] < lower,]
    return(res)
}

# Extended boxplot rule detection with asymmetry measure
boxplot_extended_detect = function(df, x, threshold = 1.5, a = -4, b = 3) {
    quartiles <- quantile(df[[x]])
    q1 <- quartiles["25%"]
    q3 <- quartiles["75%"]
    iqd <- q3 - q1
    medc <- mc(df[[x]])
    if(medc >= 0) {
        lower <- q1 - (threshold * exp(a * medc) * iqd)
        upper <- q3 + (threshold * exp(b * medc) * iqd)
    } else {
        lower <- q1 - (threshold * exp((-b) * medc) * iqd)
        upper <- q3 + (threshold * exp((-a) * medc) * iqd)
    }
    
    res <- df[df[[x]] > upper | df[[x]] < lower,]
    return(res)
}