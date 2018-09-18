#04/09/18 -updated code to return date without being converted to number later on

convert_date <- function(x) {
  no_space = gsub('\\s+', '', x)
  formatted= as.Date(no_space,format='%d%b%Y',tz="UTC")
  return(as.character(formatted))
}