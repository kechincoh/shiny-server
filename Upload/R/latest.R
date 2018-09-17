#finding the latest information for each subject

latest <- function(dataset,dcolumns)
{
  #extract desired columns
  fu_sub = extract_col(dataset,dcolumns)
  
  #split by subject
  split_latest_by_subject = split(fu_sub,fu_sub$Subject)
  
  #remove "NA" values if more than one rows, else do nothing
  split_noNA = lapply(split_latest_by_subject, function(x) if(nrow(x)>1){x[!is.na(x$LASTCONTACTDATE_FORMAT),]} else({x}))
  
  #sort in decreasing order for each subject
  split_order = lapply(split_noNA, function(x) x[rev(order(x$LASTCONTACTDATE_FORMAT)),])
  
  #Remove duplicated record
  split_latest = lapply(split_order, function(x) x[!duplicated(x$Subject),])
  
  #convert list to dataframe
  data.frame(Reduce(rbind,split_latest),stringsAsFactors=FALSE)
  
}