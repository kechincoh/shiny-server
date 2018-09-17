Report_13277nodb <- function(datasets)
{
    ####################  Data cleanup
    #select the desired columns from each csv files
    offs_date_reason = datasets$offstudy.CSV %>% rename_all(tolower) %>% dplyr::select(subject,offstudydate_raw,offstudyreason)
    offtx_date_reason = datasets$offtx.CSV %>% rename_all(tolower) %>% dplyr::select(subject,offtxdate_raw,offtxreason)
    tcell_leuk = datasets$tcell.CSV %>% rename_all(tolower) %>% dplyr::select(subject,assigneddose,leukapheresisdate_raw)
    subject = datasets$subj.CSV %>% rename_all(tolower) %>% dplyr::select(subject,upn)
    tcell_infusdate = datasets$transplant.CSV %>% rename_all(tolower) %>% dplyr::select(subject,transplantdate_raw,infusiondate_raw,treatmentarm)
    offprot_date_reason = datasets$offprottherapy.CSV %>% rename_all(tolower) %>% dplyr::select(subject,eventtype,eventdate_raw)
    
    ########################## Check the date format ##########################
    #If true: the date format should be changed; otherwise is ok
    #print results
    fu.CSV = datasets$fu.CSV
    print(check_date_format(fu.CSV)[[1]])
    fu_format = check_date_format(fu.CSV)[[2]] 
    
    fu_active = fu_format
    
    #Convert to date type if it's in the correct date format at the beginning
    if(!any(check_date_format(fu.CSV)[[1]]))
    {
      fu_active["LASTCONTACTDATE_FORMAT"] = as.Date(convert_date(fu_active$LASTCONTACTDATE_RAW))
      fu_active["PROGRESSIONDATE_FORMAT"] = as.Date(convert_date(fu_active$PROGRESSIONDATE_RAW))
      fu_active["DEATHDATE_FORMAT"] = as.Date(convert_date(fu_active$DEATHDATE_RAW))
      fu_active["FURELAPSEDATE_FORMAT"] = as.Date(convert_date(fu_active$FURELAPSEDATE_RAW))
    }
    
    
    desired_columns = c("Subject","BESTRESPFU_STD","PROGRESSIONYN","PROGRESSIONDATE_FORMAT","BESTRESPFU","DEATHDATE_FORMAT","LASTCONTACTDATE_FORMAT","VITALSTATUS")
    
    #find latest information in fu.CSV
    new_fu = latest(fu_active,desired_columns)
    new_fu = new_fu%>%rename_all(tolower)
    
    
    #merge all tables together
    mytable = Reduce(function(x, y) merge(x, y, all=TRUE), list(subject, offs_date_reason ,tcell_leuk,tcell_infusdate,offtx_date_reason,new_fu,offprot_date_reason))
    
    #Find unique subjects in mytable
    mytable =mytable[!duplicated(mytable$subject),]
    
    
    #convert column names to upper case
    mytable = mytable %>%rename_all(toupper)
    
    
    ##############  Check each subject's best response from mytable to see if it's the correct best response based on unique best response table (bestresp_unique)
    latest.record = aggregate(as.Date(fu_active$LASTCONTACTDATE_FORMAT),by=list(Subj=fu_active$Subject),FUN=max,na.rm=TRUE)
    bestres = fu_active[,c("Subject","BESTRESPFU","BESTRESPFU_STD","PROGRESSIONYN","PROGRESSIONDATE_FORMAT","LASTCONTACTDATE_FORMAT")]
    
    #change "subject" to "Subject"
    colnames(mytable)[which(names(mytable) == "SUBJECT")] <- "Subject"
    
    #check correctness of best response, progression date, progression, and last contact date
    mytable = check_nodb(mytable,bestres,latest.record)
    #bestres_unique = check(mytable,bestres)
    
    
    #check for progression column and furelapse column
    mytable = check_prog_relapsed(mytable,fu_active)
    
    #Calculate # Days Since HSCT until progression date if yes, otherwise days since HSCT until present day
    if (!("DATE_HSCT" %in% names(mytable)))
    {
      mytable["DATE_HSCT"] = as.Date(convert_date(mytable$TRANSPLANTDATE_RAW)) 
    }
    #mytable["DATE_HSCT"] = convert_date(mytable$TRANSPLANTDATE_RAW)
    mytable = calculate(mytable,mytable$DATE_HSCT)
    
    ############### Formatting table
    
    #reorder columns
    mytable = mytable[c("Subject","UPN","TREATMENTARM","LEUKAPHERESISDATE_RAW","TRANSPLANTDATE_RAW","INFUSIONDATE_RAW", "ASSIGNEDDOSE","DAYS_NUMBER","PROGRESSIONYN","PROGRESSIONDATE_FORMAT","BESTRESPFU","LASTCONTACTDATE_FORMAT","OFFSTUDYDATE_RAW","OFFSTUDYREASON","OFFTXDATE_RAW","OFFTXREASON","VITALSTATUS","DEATHDATE_FORMAT","EVENTTYPE","EVENTDATE_RAW")]
    
    #rename columns
    mytable = rename(mytable,  "Subject"="Subject","UPN"="UPN","Arm"="TREATMENTARM","Date of Leukapheresis"="LEUKAPHERESISDATE_RAW","Date of HSCT"="TRANSPLANTDATE_RAW","Date of T-cell Infusion"="INFUSIONDATE_RAW","T-Cell Assigned Dose"="ASSIGNEDDOSE","Days from HSCT to progression \n or if not progressed to the last contact date"="DAYS_NUMBER",
                                 "Progression?"="PROGRESSIONYN","Progression Date"="PROGRESSIONDATE_FORMAT","Overall Best Response (Revised IWG Response Criteria)"="BESTRESPFU","Last Contact Date"="LASTCONTACTDATE_FORMAT","Off Study Date"="OFFSTUDYDATE_RAW","Off Study Reason"="OFFSTUDYREASON","Off Treatment Date"="OFFTXDATE_RAW","Off Treatment Reason"="OFFTXREASON","Vital Status"="VITALSTATUS","Date of Death"="DEATHDATE_FORMAT","Off Protocol Therapy Reason"="EVENTTYPE","Off Protocol Therapy Date"="EVENTDATE_RAW")
    
    
    ############ change the date format from yy/m/d to mm/dd/yy
    #columns with dates
    dates_col = c("Date of HSCT","Date of Leukapheresis","Date of T-cell Infusion","Off Study Date","Off Treatment Date","Off Protocol Therapy Date")
    
    #convert to date type
    mytable_date = as.data.frame(lapply(mytable[dates_col],convert_date))
    mytable_date = as.data.frame(lapply(mytable_date,as.Date))
    
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
