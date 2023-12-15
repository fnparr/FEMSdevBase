library(TAF)
# function to convert Excel generated csv contracts file into valid df
inputfn <- "/Users/francisnparr/ActusSupport/Test/b7xPortfolio.csv"
csv2ptfdf <- function (srcfn) {
dos2unix(srcfn)
df1 <-read.csv(srcfn)
df2 <-data.frame(nrow=nrow(df1))
for (col in colnames(df1)) {df2[col] <- rmLeadSpace(df1[col])}
return (df2)
}
# convention is that all contract dates are in " yyyy-mm-dd" format
# this function will remove leading space from column of dates
rmLeadSpace <- function(svec) {
  return (sapply(svec, function(s)
    if ( (substring(s,1,1) == " ")&&(nchar(s) > 1 ))
      return (substring(s,2, nchar(s)))
    else return(s)
    ))
}

