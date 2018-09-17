Report_14108nodb<-function(datasets)
{

    ####################  Data cleanup
    #select the desired columns from each csv files
    offs_date_reason = datasets$offstudy.CSV %>% rename_all(tolower)%>%dplyr::select(subject,offstudydate_raw,offstudyreason)
    offtx_date_reason = datasets$offtx.CSV %>% rename_all(tolower)%>%dplyr::select(subject,offtxdate_raw,offtxreason)
    studydrug_date_first_second = datasets$studydrug.CSV %>% rename_all(tolower)%>%filter(foldername=="Cycle 01 (Surgery/Biopsy)")%>%rename_all(tolower)%>%dplyr::select(subject,d1dosedate_raw,d1totaldose,d2totaldose)
    onstudy_drug = datasets$onstudy.CSV %>% rename_all(tolower)%>% dplyr::select(subject,drugadmindate_raw)
    
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
    
    desired_columns = c("Subject","BESTRESPFU2_STD","PROGRESSIONYN","PROGRESSIONDATE_FORMAT","BESTRESPFU2","DEATHDATE_FORMAT","LASTCONTACTDATE_FORMAT","VITALSTATUS")
    
    #find latest information in fu.CSV
    new_fu = latest(fu_active,desired_columns)
    
    #change all column names to lowercase
    new_fu = new_fu%>%rename_all(tolower)
    
    ####################  merge all tables together
    mytable = Reduce(function(x, y) merge(x, y, all=TRUE), list(offs_date_reason ,onstudy_drug,studydrug_date_first_second ,offtx_date_reason,new_fu))
    
    mytable=mytable[!duplicated(mytable$subject),]
    
    ########################## Check the date format after merging ##########################
    #If true: the date format should be changed; otherwise is ok
    #print results
    print(check_date_format(mytable)[[1]])
    fu_format = check_date_format(mytable)[[2]] 
    
    #convert column names to upper case
    mytable = mytable %>%rename_all(toupper)
    
    ##############  Check each subject's best response from mytable to see if it's the correct best response based on unique best response table (bestresp_unique)
    latest.record = aggregate(fu_active$LASTCONTACTDATE_FORMAT,by=list(Subj=fu_active$Subject),FUN=max,na.rm=TRUE)
    bestres = fu_active[,c("Subject","BESTRESPFU2","BESTRESPFU2_STD","PROGRESSIONYN","PROGRESSIONDATE_FORMAT","LASTCONTACTDATE_FORMAT")]
    
    #change "subject" to "Subject"
    colnames(mytable)[which(names(mytable) == "SUBJECT")] <- "Subject"
    
    #Use check function to check correctness of best response, progression date, progression, and last contact date
    mytable = check_nodb(mytable,bestres,latest.record)
    
    #Calculate # Days Since Neural Stem Cells until progression date if there is progression; otherwise # Days since Neural Stem Cells until last contact date
    if (!("DATE_INFUSION" %in% names(mytable)))
    {
      mytable["DATE_INFUSION"] = as.Date(convert_date(mytable$D1DOSEDATE_RAW))
    }
    mytable = calculate(mytable,mytable$DATE_INFUSION)
    
    ###############Formatting table
    #reorder columns
    mytable = mytable[c("Subject","D1DOSEDATE_RAW","DRUGADMINDATE_RAW","D1TOTALDOSE","D2TOTALDOSE","DAYS_NUMBER", "PROGRESSIONYN","PROGRESSIONDATE_FORMAT","BESTRESPFU2","LASTCONTACTDATE_FORMAT","OFFSTUDYDATE_RAW","OFFSTUDYREASON","OFFTXDATE_RAW","OFFTXREASON","VITALSTATUS","DEATHDATE_FORMAT")]
    
    #rename columns
    mytable = dplyr::rename(mytable, "Date of NSC"="D1DOSEDATE_RAW", "Irinotecan Admin Date"="DRUGADMINDATE_RAW","Actual First Dose of NSC"="D1TOTALDOSE", "Actual Second Dose of NSC"="D2TOTALDOSE","Days from NSCs to progression or if not progressed the last contact date"="DAYS_NUMBER", "Progression?"="PROGRESSIONYN","Progression Date"="PROGRESSIONDATE_FORMAT","Overall Best Response (RANO Criteria)"="BESTRESPFU2",
                                "Last Contact Date"="LASTCONTACTDATE_FORMAT","Off Study Date"="OFFSTUDYDATE_RAW","Off Study Reason"="OFFSTUDYREASON","Off Treatment Date"="OFFTXDATE_RAW","Off Treatment Reason"="OFFTXREASON","Vital Status"="VITALSTATUS","Date of Death"="DEATHDATE_FORMAT")
    
    ###################  change the date format from yy/m/d to mm/dd/yy
    #columns with dates
    dates_col = c("Date of NSC","Irinotecan Admin Date", "Off Study Date","Off Treatment Date" )
    
    #convert to date type
    mytable_date = as.data.frame(lapply(mytable[dates_col],convert_date))
    mytable_date = as.data.frame(lapply(mytable_date,as.Date))
    
    #change format from (yy-mm-dd) to new format (mm/dd/yy)
    col_format = c("Progression Date","Last Contact Date","Date of Death")
    mytable_date_format = as.data.frame(lapply(mytable_date,format,format="%m/%d/%y"))
    mytable_col_format = as.data.frame(lapply(mytable[col_format],format,format="%m/%d/%y" ))
    
    #replace old table date format(yy-mm-dd) with new date format (mm/dd/yy)
    mytable[,dates_col]<-mytable_date_format
    mytable[,col_format]<-mytable_col_format
    
    rownames(mytable)<-c()
    
    colnames(mytable) = str_wrap(colnames(mytable),width = 10)
    
    mytable

}
