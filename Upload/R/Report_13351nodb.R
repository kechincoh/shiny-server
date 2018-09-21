Report_13351nodb <- function(datasets)
{

    ####################  Data cleanup
    #select the desired columns 
    offs_date_reason =  datasets$offstudy.CSV  %>% rename_all(tolower) %>% dplyr::select(subject,offstudydate_raw,offstudyreason)
    offtx_date_reason =  datasets$offtx.CSV  %>% rename_all(tolower) %>% dplyr::select(subject,offtxdate_raw,offtxreason)
    tcell_leuk = datasets$tcell.CSV %>% rename_all(tolower) %>% dplyr::select(subject,assigneddose,leukapheresisdate_raw)
    subject = datasets$subj.CSV %>% rename_all(tolower) %>% dplyr::select(subject,upn)
    tcell_infusdate = datasets$tcelladmin.CSV %>% rename_all(tolower) %>% dplyr::select(subject,infusiondate_raw,infusiondose)
    tcell_admin = datasets$tcelladmin.CSV %>% rename_all(tolower) %>% select(subject,treatmentarm)
    offprot_date_reason = datasets$offprottherapy.CSV%>%rename_all(tolower)%>%select(subject,eventtype,eventdate_raw)
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
    
    #desired columns to extract
    desired_columns = c("Subject","PROGRESSIONYN","PROGRESSIONDATE_FORMAT","BESTRESPFU","BESTRESPFU_STD","DEATHDATE_FORMAT","LASTCONTACTDATE_FORMAT","VITALSTATUS")
    
    #find latest information in fu.CSV
    new_fu = latest(fu_active,desired_columns)
    new_fu = new_fu%>%rename_all(tolower)
    
    #merge all tables together
    mytable = Reduce(function(x, y) merge(x, y, all=TRUE), list(subject, tcell_admin,offs_date_reason ,tcell_leuk,tcell_infusdate,offtx_date_reason,new_fu,offprot_date_reason))
    
    #Find unique subjects
    mytable =mytable[!duplicated(mytable$subject),]
    
    ########################## Check the date format after merging tables ##########################
    #If true: the date format should be changed; otherwise is ok
    #print results
    print(check_date_format(mytable)[[1]])
    fu_format = check_date_format(mytable)[[2]] 
    
    #convert column names to upper case
    mytable = mytable %>%rename_all(toupper)
    ##############  Check each best response from mytable is the correct one based on unique best response table (bestresp_unique)
    #Subset with best response, progression, progression date
    latest.record = aggregate(fu_active$LASTCONTACTDATE_FORMAT,by=list(Subj=fu_active$Subject),FUN=max,na.rm=TRUE)
    bestres = fu_active[,c("Subject","BESTRESPFU","BESTRESPFU_STD","PROGRESSIONYN","PROGRESSIONDATE_FORMAT","LASTCONTACTDATE_FORMAT")]
    
    #change "subject" to "Subject"
    colnames(mytable)[which(names(mytable) == "SUBJECT")] <- "Subject"
    
    #check correctness of best response, progression date, progression, and last contact date
    mytable = check_nodb(mytable,bestres,latest.record)
    
    #Calculate # Days Since HSCT until progression date if yes, otherwise days since HSCT until present day
    if (!("DATE_INFUSION" %in% names(mytable)))
    {
      mytable["DATE_INFUSION"] = as.Date(convert_date(mytable$INFUSIONDATE_RAW))
    }
    #mytable["DATE_INFUSION"] = convert_date(mytable$INFUSIONDATE_RAW)
    mytable = calculate(mytable,mytable$DATE_INFUSION)
    
    
    ############### Formatting table
    
    #reorder columns
    mytable = mytable[c("Subject","UPN","TREATMENTARM","LEUKAPHERESISDATE_RAW","INFUSIONDATE_RAW","INFUSIONDOSE","DAYS_NUMBER","PROGRESSIONYN","PROGRESSIONDATE_FORMAT","BESTRESPFU","LASTCONTACTDATE_FORMAT","OFFSTUDYDATE_RAW","OFFSTUDYREASON","OFFTXDATE_RAW","OFFTXREASON","VITALSTATUS","DEATHDATE_FORMAT","EVENTTYPE","EVENTDATE_RAW")]
    
    #rename columns
    mytable = rename(mytable,  "Treatment Arm"="TREATMENTARM","Date of Leukapheresis"="LEUKAPHERESISDATE_RAW","Date of T-cell Infusion"="INFUSIONDATE_RAW","T-Cell Dose x 10^6 cells"="INFUSIONDOSE","Days from T-cell infusion to progression \n or if not progressed the current report date"="DAYS_NUMBER",
                                 "Progression?"="PROGRESSIONYN","Progression Date"="PROGRESSIONDATE_FORMAT","Overall Best Response (IWC and IWCLL Criteria)"="BESTRESPFU",
                                 "Last Contact Date"="LASTCONTACTDATE_FORMAT","Off Study Date"="OFFSTUDYDATE_RAW","Off Study Reason"="OFFSTUDYREASON","Off Treatment Date"="OFFTXDATE_RAW","Off Treatment Reason"="OFFTXREASON","Vital Status"="VITALSTATUS","Date of Death"="DEATHDATE_FORMAT","Off Protocol Therapy Reason"="EVENTTYPE","Off Protocol Therapy Date"="EVENTDATE_RAW")
    
    
    #change the date format from yy/m/d to mm/dd/yy
    #columns with dates
    dates_col = c("Date of Leukapheresis","Date of T-cell Infusion", "Off Study Date","Off Treatment Date","Off Protocol Therapy Date" )
    
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
    
    #print(mytable)
    
    rownames(mytable)<-c()
    
    mytable
    
}