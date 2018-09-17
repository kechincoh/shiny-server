#function to melt progressionyn,relapseyn,progressiondate,relapsedate

comb_columns <- function(dataset)
{
  #extract progression column
  one = data.frame(dataset$Subject,dataset$PROGRESSIONYN,dataset$PROGRESSIONDATE_FORMAT)
  
  #extract relapse column
  two = data.frame(dataset$Subject,dataset$FURELAPSEYN,dataset$FURELAPSEDATE_FORMAT)
  
  #change columns' name to match first dataset
  names(two) = c("dataset.Subject","dataset.PROGRESSIONYN","dataset.PROGRESSIONDATE_FORMAT")
  
  #merge the two tables
  final =rbind(one,two)
  
  final = data.frame(final,dataset$LASTCONTACTDATE_FORMAT)
}