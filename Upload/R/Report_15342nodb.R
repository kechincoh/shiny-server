Report_15342nodb<-function(datasets)
{
    ####################  Data cleanup
    #select the desired columns from each csv files
    offs_date_reason = datasets$offstudy.CSV %>% rename_all(tolower)%>%dplyr::select(subject,offstudydate_raw,offstudyreason)
    offtx_date_reason = datasets$offtx.CSV %>% rename_all(tolower)%>%dplyr::select(subject,offtxdate_raw,offtxreason)
    onstudy_tx = datasets$onstudy.CSV %>% rename_all(tolower)%>%dplyr::select(subject,initialtxdate_raw)
    study_dose = datasets$studydrug.CSV%>% rename_all(tolower)%>%filter(instancerepeatnumber==0)%>%dplyr::select(subject,totaldose)
                                 
    
    ################### Count number of cycle per subject 
    cycle_count = datasets$studydrug.CSV %>% rename_all(tolower)%>%dplyr::select(subject,agent)
    number_cycle= aggregate(cycle_count$agent,by=list(subject=cycle_count$subject,Agent = cycle_count$agent),FUN=length)
    number_cycle_nodup = number_cycle[!duplicated(number_cycle$subject),]
    names(number_cycle_nodup) = c("subject","Agent","Number of Treatment")
    
    ################### Remove duplicated rows in fu.CSV file by selecting only rows with latest contact
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
    
    desired_columns = c("Subject","BESTRESPFU_STD","PROGRESSIONYN","PROGRESSIONDATE_FORMAT","BESTRESPFU","DEATHDATE_FORMAT","LASTCONTACTDATE_FORMAT","VITALSTATUS")
    
    #find latest information in fu.CSV
    new_fu = latest(fu_active,desired_columns)
    
    #change all column names to lowercase
    new_fu = new_fu%>%rename_all(tolower)
    
    #merge all tables together
    mytable = Reduce(function(x, y) merge(x, y, all=TRUE), list(onstudy_tx,number_cycle_nodup,offs_date_reason ,offtx_date_reason,study_dose,new_fu))
    
    #Find unique subjects in mytable
    mytable =mytable[!duplicated(mytable$subject),]
    
    ########################## Check the date format after merging ##########################
    #If true: the date format should be changed; otherwise is ok
    #print results
    print(check_date_format(mytable)[[1]])
    fu_format = check_date_format(mytable)[[2]] 
    
    #convert column names to upper case
    mytable = mytable %>%rename_all(toupper)
    
    ##############  Check each subject's best response from mytable to see if it's the correct best response based on unique best response table (bestresp_unique)
    latest.record = aggregate(fu_active$LASTCONTACTDATE_FORMAT,by=list(Subj=fu_active$Subject),FUN=max,na.rm=TRUE)
    bestres = fu_active[,c("Subject","BESTRESPFU","BESTRESPFU_STD","PROGRESSIONYN","PROGRESSIONDATE_FORMAT","LASTCONTACTDATE_FORMAT")]
    
    
    #change "subject" to "Subject"
    colnames(mytable)[which(names(mytable) == "SUBJECT")] <- "Subject"
    
    #check correctness of best response, progression date, progression, and last contact date
    mytable = check_nodb(mytable,bestres,latest.record)
    #bestres_unique = check(mytable,bestres)
    
    
    #Calculate # Days Since Treatment until progression date if yes, otherwise days since Treatment until last contact date
    if (!("DATE_TX" %in% names(mytable)))
    {
      mytable["DATE_TX"] = as.Date(convert_date(mytable$INITIALTXDATE_RAW))
    }
    
    mytable = calculate(mytable,mytable$DATE_TX)
    ############### Formatting table
    
    #reorder columns
    mytable = mytable[c("Subject","INITIALTXDATE_RAW","NUMBER OF TREATMENT","TOTALDOSE","DAYS_NUMBER","PROGRESSIONYN","PROGRESSIONDATE_FORMAT","BESTRESPFU","LASTCONTACTDATE_FORMAT","OFFSTUDYDATE_RAW","OFFSTUDYREASON","OFFTXDATE_RAW","OFFTXREASON","VITALSTATUS","DEATHDATE_FORMAT")]
    
    #rename columns
    mytable = rename(mytable, "Subject"="Subject","Date of Treatment"="INITIALTXDATE_RAW" ,"Number of treatment"="NUMBER OF TREATMENT","Total dose of Neratinib (mg)"="TOTALDOSE","Days from treatment to progression \n or if not progressed to the last contact date"="DAYS_NUMBER",
                                 "Progression?"="PROGRESSIONYN","Progression Date"="PROGRESSIONDATE_FORMAT","Overall Best Response (RECIST 1.1 Criteria)"="BESTRESPFU","Last Contact Date"="LASTCONTACTDATE_FORMAT","Off Study Date"="OFFSTUDYDATE_RAW","Off Study Reason"="OFFSTUDYREASON","Off Treatment Date"="OFFTXDATE_RAW","Off Treatment Reason"="OFFTXREASON","Vital Status"="VITALSTATUS","Date of Death"="DEATHDATE_FORMAT")
    
    
    ############ change the date format from yy/m/d to mm/dd/yy
    #columns with dates
    dates_col = c("Date of Treatment","Off Study Date","Off Treatment Date" )
    
    #convert to date type
    mytable_date = as.data.frame(lapply(mytable[dates_col],convert_date))
    mytable_date = as.data.frame(lapply(mytable_date,as.Date))
    
    #change format
    #change format
    col_format = c("Progression Date","Last Contact Date","Date of Death")
    mytable_date_format = as.data.frame(lapply(mytable_date,format,format="%m/%d/%y"))
    mytable_col_format = as.data.frame(lapply(mytable[col_format],format,format="%m/%d/%y" ))
    
    #replace old table date format(yy-mm-dd) with new date format (mm/dd/yy)
    mytable[,dates_col]<-mytable_date_format
    mytable[,col_format]<-mytable_col_format
    
    rownames(mytable)<-c()
    
    mytable

}