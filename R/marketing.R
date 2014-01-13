# Measures for online marketing campaigns

# Effectiveness measures
## click through rate
ctr = function(df, clicks="Clicks", impressions="Impressions") {
    df$ctr <- apply(df, 1, function(row) df[[clicks]]/df[[impressions]])
    return(res)
}
##conversion rate
cr = function(df, goals="Orders", visits="Visits") {
    df$ctr <- apply(df, 1, function(row) df[[goals]]/df[[visits]])
    return(res)
}

# Efficiency measures
## cost per mile
cpm = function(df, costs="Costs", impressions="Impressions") {
    df$cpm <- apply(df, 1, function(row) (df[[costs]]/df[[impressions]] * 1000))
    return(res)
}
## cost per impressions
cpi = function(df, costs="Costs", impressions="Impressions") {
    df$cpi <- apply(df, 1, function(row) df[[costs]]/df[[impressions]])
    return(res)
}
## cost per click
cpc = function(df, costs="Costs", clicks="Clicks") {
    df$cpc <- apply(df, 1, function(row) df[[costs]]/df[[clicks]])
    return(res)
}
## cost per action/acquisition
cpa = function(df, costs="Costs", action="Orders") {
    df$cpa <- apply(df, 1, function(row) df[[costs]]/df[[action]])
    return(res)
}