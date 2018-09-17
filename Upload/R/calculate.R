#function to calculate number of days between two time points

calculate <- function (mtable,x)
{
  mtable["DAYS_NUMBER"] <- NULL
  mtable["OFFTXDATE_FORMAT"]<-convert_date(mtable$OFFTXDATE_RAW)
  for(d in 1:length(unique(mtable$Subject)))
  {
    #if offtreatment and progression is na
    if(!(is.na(mtable$OFFTXDATE_RAW[d])) & is.na(mtable$PROGRESSIONYN[d]) ) 
    {
      mtable[d,"DAYS_NUMBER"] = round(difftime(mtable$OFFTXDATE_FORMAT[d], x[d],units = c("days")))
    }
    
    #if offtreatment and progression
    else if(!(is.na(mtable$OFFTXDATE_RAW[d])) & tolower(mtable$PROGRESSIONYN[d])=='yes' )
    { 
      mtable[d,"DAYS_NUMBER"] = round(difftime(mtable$PROGRESSIONDATE_FORMAT[d], x[d], units = "days"))
    }
    #if offtreatment and no progression 
    else if(!(is.na(mtable$OFFTXDATE_RAW[d])) & tolower(mtable$PROGRESSIONYN[d])=='no' ) 
    {
      mtable[d,"DAYS_NUMBER"] = round(difftime(mtable$OFFTXDATE_FORMAT[d], x[d],units = c("days")))
    }
    else
    {
      mtable[d,"DAYS_NUMBER"] = round(difftime(mtable$LASTCONTACTDATE_FORMAT[d], x[d],units = c("days")))
    }
  }
  mtable
}