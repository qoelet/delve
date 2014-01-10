\name{hampel_id}
\alias{hampel_id}
\title{Applies the Hampel identifier function to a given dataframe and column name}
\usage{
hampel_id(df, x, threshold = 3)
}
\arguments{
    \item{df}{dataframe}

    \item{x}{column name of the dataframe to run detection off}

    \item{threshold}{by default 3}
}
\description{
Returns rows of the dataframe that counts as an outlier
}