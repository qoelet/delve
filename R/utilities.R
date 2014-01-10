# Utilities

# Reverses a data frame
reverse_df <- function(df) {
    col_names <- names(df)
    for(col_idx in 1:length(col_names)) {
        df[[col_idx]] <- rev(df[[col_idx]])
    }
    return(df)
}