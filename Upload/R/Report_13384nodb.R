Report_13384nodb <- function(datasets)
{
    ####################  Data cleanup
    #select the desired columns from each csv files
    offs_date_reason = datasets$offstudy.CSV  %>% rename_all(tolower) %>% dplyr::select(subject,offstudydate_raw,offstudyreason)
    offtx_date_reason =  datasets$offtx.CSV %>% rename_all(tolower) %>% dplyr::select(subject,offtxdate_raw,offtxreason)
    tcell_leuk = datasets$tcell.CSV %>% rename_all(tolower) %>% dplyr::select(subject,assigneddose,leukapheresisdate_raw)
    subject =  datasets$subj.CSV %>% rename_all(tolower) %>% dplyr::select(subject,upn)
    arm= datasets$protocolproced.CSV %>% rename_all(tolower) %>% dplyr::select(subject,protprocedtype)
    doseschedule= datasets$onstudy.CSV %>% rename_all(tolower) %>% dplyr::select(subject,dosescheduleil13)
    offprot_date_reason = datasets$offprottherapy.CSV %>% rename_all(tolower) %>% dplyr::select(subject,eventtype,eventdate_raw)
      
    
    #################### Clean up Infusion data
    #check date format
    tcelladmin.CSV=datasets$tcelladmin.CSV
    print(check_date_format(tcelladmin.CSV)[[1]])
    tcelladmin.CSV = check_date_format(tcelladmin.CSV)[[2]]
    
    #Convert to date type if it's in the correct date format 
    if(!any(check_date_format(tcelladmin.CSV)[[1]]))
    {
      tcelladmin.CSV["INFUSIONDATE_FORMAT"] = as.Date(convert_date(tcelladmin.CSV$INFUSIONDATE_RAW)) 
    
    }
    #extract columns of interenst
    tcell_infusdate = extract_col(tcelladmin.CSV,c("Subject","INFUSIONDATE_FORMAT"))
    
    #count the number of infusion cycle per subject
    infusdate_nona = na.omit(tcell_infusdate)
    number_infusion = aggregate(infusdate_nona$INFUSIONDATE_FORMAT,by=list(Subject=infusdate_nona$Subject),FUN=length)
    names(number_infusion) = c("subject","Number of Infusion")
    
    
    #Pick the first(latest)infusion date, we need to sort in decreasing order and remove "NA"
    #sort decreasing and remove "NA"
    infusdate_sorted= tcell_infusdate[order(tcell_infusdate$INFUSIONDATE_FORMAT,na.last = TRUE),]
    
    #finding unique subjects with latest infusion date
    infusdate_unique = infusdate_sorted[!duplicated(infusdate_sorted$Subject),]%>%rename_all(tolower)
    
    ################### Remove duplicated rows in fu.CSV file by selecting only rows with latest contact
    #check if the date it's in the correct date format
    fu.CSV = datasets$fu.CSV
    print(check_date_format(fu.CSV)[[1]])
    fu_format = check_date_format(fu.CSV)[[2]] 
    
    #filter only active records
    fu_active = fu_format
    
    #Convert to date type if it's in the correct date format from the beginning
    if(!any(check_date_format(fu.CSV)[[1]]))
    {
      fu_active["LASTCONTACTDATE_FORMAT"] = as.Date(convert_date(fu_active$LASTCONTACTDATE_RAW)) 
      fu_active["PROGRESSIONDATE_FORMAT"] = as.Date(convert_date(fu_active$PROGRESSIONDATE_RAW))
      fu_active["DEATHDATE_FORMAT"] = as.Date(convert_date(fu_active$DEATHDATE_RAW))
      fu_active["FURELAPSEDATE_FORMAT"] = as.Date(convert_date(fu_active$FURELAPSEDATE_RAW))
    }
    
    #extract desired columns
    desired_columns = c("Subject","PROGRESSIONYN","PROGRESSIONDATE_FORMAT","BESTRESPFU2RANO","BESTRESPFU2RANO_STD","DEATHDATE_FORMAT","LASTCONTACTDATE_FORMAT","VITALSTATUS")
    
    #find latest information in fu.CSV
    new_fu = latest(fu_active,desired_columns)
    new_fu = new_fu%>%rename_all(tolower)
    
    ################### Remove duplicated rows in fu.CSV file by selecting only rows with latest contact
    
    #merge all tables together
    mytable = Reduce(function(x, y) merge(x, y, all=TRUE), list(subject, number_infusion, offs_date_reason ,tcell_leuk,infusdate_unique,offtx_date_reason,new_fu,arm,doseschedule,offprot_date_reason))
    
    
    #Find unique subjects
    mytable =mytable[!duplicated(mytable$subject),]
    
    ########################## Check the date format ##########################
    #If true: the date format should be changed; otherwise is ok
    #print results
    print(check_date_format(mytable)[[1]])
    mytable = check_date_format(mytable)[[2]]
    
    
    ##############  Adding best response for each subject due to empty entry on best response based on latest contact date and can be found on previous date from fu form
    #Finding unique subjects
    unique_subj = unique(mytable$subject)
    
    # #Subset with best response, progression, progression date
    # bestres = fu_active[,c("Subject","BESTRESPFU2RANO","BESTRESPFU2RANO_STD","PROGRESSIONYN","PROGRESSIONDATE_FORMAT","LASTCONTACTDATE_FORMAT")]
    
    ##############  Check each subject's best response from mytable to see if it's the correct best response based on unique best response table (bestresp_unique)
    #convert column names to upper case
    mytable = mytable %>%rename_all(toupper)
    
    #change "subject" to "Subject"
    colnames(mytable)[which(names(mytable) == "SUBJECT")] <- "Subject"
    
    latest.record = aggregate(fu_active$LASTCONTACTDATE_FORMAT,by=list(Subj=fu_active$Subject),FUN=max,na.rm=TRUE)
    bestres = fu_active[,c("Subject","BESTRESPFU2RANO","BESTRESPFU2RANO_STD","PROGRESSIONYN","PROGRESSIONDATE_FORMAT","LASTCONTACTDATE_FORMAT")]
    mytable = check_nodb(mytable,bestres,latest.record)
    #bestres_unique = check(mytable,bestres)
    
    #check to see if there is a furelapse column
    if("furelapsedate_raw" %in% tolower(names(fu_active)))
    {
      #check for progression column and furelapse column
      mytable = check_prog_relapsed(mytable,fu_active)
    }
    
    #Calculate # Days Since T-Cell infusion until progression date if yes, otherwise days since HSCT until present day
    mytable["DATE_INFUSION"] =  mytable$INFUSIONDATE_FORMAT
    mytable = calculate(mytable,mytable$DATE_INFUSION)
    
    ############### Formatting table
    
    #reorder columns
    mytable = mytable[c("Subject","UPN", "PROTPROCEDTYPE","DOSESCHEDULEIL13","LEUKAPHERESISDATE_RAW","DATE_INFUSION","NUMBER OF INFUSION", "ASSIGNEDDOSE","DAYS_NUMBER","PROGRESSIONYN","PROGRESSIONDATE_FORMAT","BESTRESPFU2RANO","LASTCONTACTDATE_FORMAT","OFFSTUDYDATE_RAW","OFFSTUDYREASON","OFFTXDATE_RAW","OFFTXREASON","VITALSTATUS","DEATHDATE_FORMAT","EVENTTYPE","EVENTDATE_RAW")]
    
    #rename columns
    mytable = rename(mytable,  "Subject"="Subject","UPN"="UPN","ARM"="PROTPROCEDTYPE", "Dose Schedule"="DOSESCHEDULEIL13","Date of Leukapheresis"="LEUKAPHERESISDATE_RAW","Date of T-cell Infusion"="DATE_INFUSION","Number of Infusion"="NUMBER OF INFUSION","T-Cell Assigned Dose"="ASSIGNEDDOSE","Days from T-cell infusion to progression \n or if not progressed the last contact date"="DAYS_NUMBER",
                                 "Progression?"="PROGRESSIONYN","Progression Date"="PROGRESSIONDATE_FORMAT","Overall Best Response (Revised IWG Response Criteria)"="BESTRESPFU2RANO","Last Contact Date"="LASTCONTACTDATE_FORMAT","Off Study Date"="OFFSTUDYDATE_RAW","Off Study Reason"="OFFSTUDYREASON","Off Treatment Date"="OFFTXDATE_RAW","Off Treatment Reason"="OFFTXREASON","Vital Status"="VITALSTATUS","Date of Death"="DEATHDATE_FORMAT","Off Protocol Therapy Reason"="EVENTTYPE","Off Protocol Therapy Date"="EVENTDATE_RAW")
    
    
    ############ change the date format from yy/m/d to mm/dd/yy
    #columns with dates
    dates_col = c("Date of Leukapheresis","Off Study Date","Off Treatment Date","Off Protocol Therapy Date" )
    
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