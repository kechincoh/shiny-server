Report_13401nodb <- function(datasets)
{
  ####################  Data cleanup
  #select the desired columns from each csv files
  offs_date_reason = datasets$offstudy.CSV%>% rename_all(tolower) %>% dplyr::select(subject,offstudydate_raw,offstudyreason)
  offtx_date_reason = datasets$offtx.CSV %>% rename_all(tolower) %>% dplyr::select(subject,offtxdate_raw,offtxreason)
  studydrug_date_first_second =datasets$studydrug.CSV%>%rename_all(tolower)%>%filter(foldername=="Cycle 1 (Surgery/Biopsy)")%>%dplyr::select(subject,d1totaldose)
  onstudy_drug = datasets$onstudy.CSV %>% rename_all(tolower) %>% dplyr::select(subject,nscadmindate_raw,x_5fcadmindate_raw)
  leucovorin = datasets$studydrug.CSV %>% rename_all(tolower) %>% dplyr::select(subject,instancename,leucovoringivenyn,leucovoringivenyn_std)
  ################### Remove duplicated rows in fu.CSV file by selecting only rows with latest contact
 
  #check luecovorinyn matches in two method
  one = leucovorin[order(leucovorin$leucovoringivenyn_std),]
  one_unique = one[!duplicated(one$subject),]
  
  two = leucovorin[which(tolower(leucovorin$instancename)==grep("^cycle.*1",tolower(leucovorin$instancename),value=TRUE)[1]),]
  two_order = two[order(two$subject),]
  two_unique = two[!duplicated(two$subject),]
  
  #compare
  if(all(one_unique$leucovoringivenyn%in% two_unique$leucovoringivenyn))
  {
    leucovorin_table = one_unique[c("subject","leucovoringivenyn")]
  }else
  {
    print("Check Leucovorin table for inconsistency in answers")
  }
  
  
  ########################## Check the date format for fu form ##########################
  #If true: the date format should be changed; otherwise is ok
  #print results
  fu.CSV = datasets$fu.CSV
  print(check_date_format(fu.CSV)[[1]])
  fu_format = check_date_format(fu.CSV)[[2]] 
  
  #filter only active records
  fu_active = fu_format
  
  #Convert to date type if it's in the correct date format at the beginning
  if(!any(check_date_format(fu.CSV)[[1]]))
  {
    fu_active["LASTCONTACTDATE_FORMAT"] = as.Date(convert_date(fu_active$LASTCONTACTDATE_RAW))
    fu_active["PROGRESSIONDATE_FORMAT"] = as.Date(convert_date(fu_active$PROGRESSIONDATE_RAW))
    fu_active["DEATHDATE_FORMAT"] = as.Date(convert_date(fu_active$DEATHDATE_RAW))
  }
  
  desired_columns = c("Subject","PROGRESSIONYN","PROGRESSIONDATE_FORMAT","BESTRESPFU2","BESTRESPFU2_STD","DEATHDATE_RAW","LASTCONTACTDATE_FORMAT","VITALSTATUS")

  #find latest information in fu.CSV
  new_fu = latest(fu_active,desired_columns)
  new_fu = new_fu%>%rename_all(tolower)

  #merge all tables together
  mytable = Reduce(function(x, y) merge(x, y, all=TRUE),  list(offs_date_reason ,onstudy_drug,studydrug_date_first_second ,offtx_date_reason,new_fu,leucovorin_table))

  #Find unique subjects
  mytable =mytable[!duplicated(mytable$subject),]


  ########################## Check the date format ##########################
  #If true: the date format should be changed; otherwise is ok
  #print results
  print(check_date_format(mytable)[[1]])
  fu_format = check_date_format(mytable)[[2]]

  #convert column names to upper case
  mytable = mytable %>%rename_all(toupper)
  #Subset with best response, progression, progression date
  latest.record = aggregate(fu_active$LASTCONTACTDATE_FORMAT,by=list(Subj=fu_active$Subject),FUN=max,na.rm=TRUE)
  bestres = fu_active[,c("Subject","BESTRESPFU2","BESTRESPFU2_STD","PROGRESSIONYN","PROGRESSIONDATE_FORMAT","LASTCONTACTDATE_FORMAT")]

  #change "subject" to "Subject"
  colnames(mytable)[which(names(mytable) == "SUBJECT")] <- "Subject"

  # Use check function to check correctness of best response, progression date, progression, and last contact date
   mytable = check_nodb(mytable,bestres,latest.record)

  #Calculate # Days Since HSCT until progression date if yes, otherwise days since HSCT until present day
  if (!("DATE_INFUSION" %in% names(mytable)))
  {
    mytable["DATE_INFUSION"] = as.Date(convert_date(mytable$NSCADMINDATE_RAW))
  }

  mytable = calculate(mytable,mytable$DATE_INFUSION)

  ############### Formatting table
  #reorder columns
  mytable = mytable[c("Subject","NSCADMINDATE_RAW","D1TOTALDOSE","LEUCOVORINGIVENYN","DAYS_NUMBER", "PROGRESSIONYN","PROGRESSIONDATE_FORMAT","BESTRESPFU2","LASTCONTACTDATE_FORMAT","OFFSTUDYDATE_RAW","OFFSTUDYREASON","OFFTXDATE_RAW","OFFTXREASON","VITALSTATUS","DEATHDATE_RAW")]

  #rename columns
  mytable = rename(mytable, "Date of NSC"="NSCADMINDATE_RAW","Actual First Dose of NSC"="D1TOTALDOSE","Leucovorin?"="LEUCOVORINGIVENYN","Days from NSCs to progression \n or if not progressed the last contact date"="DAYS_NUMBER", "Progression?"="PROGRESSIONYN","Progression Date"="PROGRESSIONDATE_FORMAT","Overall Best Response (RANO Criteria)"="BESTRESPFU2",
                   "Last Contact Date"="LASTCONTACTDATE_FORMAT","Off Study Date"="OFFSTUDYDATE_RAW","Off Study Reason"="OFFSTUDYREASON","Off Treatment Date"="OFFTXDATE_RAW","Off Treatment Reason"="OFFTXREASON","Vital Status"="VITALSTATUS","Date of Death"="DEATHDATE_RAW"  )


  #change the date format from yy/m/d to mm/dd/yy
  #columns with dates
  dates_col = c("Date of NSC", "Off Study Date","Off Treatment Date","Date of Death" )

  #convert to date type
  mytable_date = as.data.frame(lapply(mytable[dates_col],convert_date))
  mytable_date = as.data.frame(lapply(mytable_date,as.Date))

  #change format
  mytable_date_format = format(mytable_date,"%m/%d/%y")
  mytable_date_format_last = format(mytable$`Last Contact Date`, "%m/%d/%y")
  mytable_date_format_progdate = format(mytable$`Progression Date`,"%m/%d/%y" )

  #replace old table date format(yy-mm-dd) with new date format (mm/dd/yy)
  mytable[,dates_col]<-mytable_date_format
  mytable[,"Last Contact Date"]<-mytable_date_format_last
  mytable[,"Progression Date"]<-mytable_date_format_progdate

  rownames(mytable)<-c()

  #mytable13401nodb_report = mytable
  mytable
  # # Write CSV 
  # #write.csv(mytable, file= "Monitoring_Report_13401_updated_120717.csv",row.names=F)
}