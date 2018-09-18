# function to extract columns from a dataset
# If there is multiple cycle (multiple=TRUE), the function chooses the information from the first cycle. Otherwise, it just extracts desired column

extract_col <- function(dataset,columns,multiple=FALSE)
{
  #Active records
  active = dataset$RecordActive==1 | dataset$RecordActive==TRUE
  if(multiple == TRUE)
  {
    dataset[which(active & dataset$InstanceName=="Cycle 01"),columns]

  }
  else
  {
    dataset[which(active),columns]
  }
}