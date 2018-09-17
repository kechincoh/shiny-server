#function to check the correct date format
#output: FALSE=data is in correct format;otherwise, change and return fixed date format

check_date_format <- function(dataset)
{
  #extract columns with "RAW" ending
  columns_raw = grep("RAW$",names(dataset),value=TRUE)
  
  #finding columns with "RAW" ending
  check_dates = dataset[,columns_raw]
  
  #check all columns with "RAW" ending to find which column dates are in the wrong format
  fix_format = sapply(check_dates, function(x)  !all(is.na(as.Date(as.character(x),
                                                             format = c("%d/%m/%Y", "%Y/%m/%d","%d-%b-%y" ,"%Y-%m-%d","%d/%m/%Y","%m/%d/%Y"))))
  )
  
  #fix if at least one true
  if(any(fix_format))
  {
    #find the names of the columns with wrong date format
    wrong_column_format = names(fix_format[which(fix_format==TRUE)])
  
    dataset[wrong_column_format][is.na(dataset[wrong_column_format])]<-"NA"
  
    #optional check-convert to character type
    listing = lapply(dataset[,wrong_column_format], as.character)
    row.names(listing)<-NULL
  
    #if there is one column, use rbind
    if (length(wrong_column_format)>0 & length(wrong_column_format)<2)
    {
      dataset[,wrong_column_format] = data.frame(Reduce(rbind,listing),stringsAsFactors=FALSE,row.names=NULL)
    }else
    {
      dataset[,wrong_column_format] = data.frame(Reduce(cbind,listing),stringsAsFactors=FALSE)
    }

    #transpose column for guess_formats function
    dataset_transpose = t(dataset[,wrong_column_format])

    #Guess the format of each column to be changed and then convert to date type
    for(i in 1:nrow(dataset_transpose))
    {
        guess = guess_formats(dataset_transpose[!is.na(dataset_transpose)], c("mdY", "BdY", "Bdy", "bdY", "bdy","mdy","dby"))
        colnames(guess)<-NULL
        guess_format=guess[length(guess)]
        dataset[gsub("RAW","FORMAT",wrong_column_format[i])] = as.Date(dataset_transpose[i,],format=guess_format)
    }
    output=list(fix_format,dataset)
    return(output)
  }else
  {
    output=list(FALSE,dataset)
    return (output)
  }
}