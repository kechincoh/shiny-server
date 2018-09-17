# function to convert to date type

convert_date <- function(x) {
  no_space = gsub('\\s+', '', x)
  formatted= as.Date(no_space,format='%d%b%Y',tz="UTC")
  return(as.character(formatted))
}