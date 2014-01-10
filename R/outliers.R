# Collection of outlier detection functions

# Simple implementation of Hampel identifier 
# Takes a dataframe, a column name and optionally a threshold value
hampel_id = function(df, x, threshold = 3) {
    x_mad <- mad(df[[x]], na.rm=TRUE)
    x_median <- median(df[[x]], na.rm=TRUE)
    lower <- x_median - (threshold * x_mad)
    upper <- x_median + (threshold * x_mad)
    res <- df[df[[x]] > upper | df[[x]] < lower,]
    return(res)
}