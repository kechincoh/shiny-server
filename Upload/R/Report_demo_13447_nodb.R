Report_demo_13447_nodb<-function(datasets)
{
    #Data cleanup
    #select the desired columns
    demo_select = datasets$demo.CSV %>% rename_all(tolower) %>% dplyr::select(subject,gender,birthdate_raw,consentdate_raw,ethnicity,racecaucasian,raceblack,racepacificislander,
                                                                                                             raceasian,racenativeamerican,raceunknown,racenondisclosed)
    tcell_admin = datasets$tcelladmin.CSV %>% rename_all(tolower) %>% select(subject,treatmentarm)
    
    ######### merge clean files #############################
    merged = Reduce(function(x, y) merge(x, y,all=TRUE), list(tcell_admin,demo_select))
    merged_nodupl = merged[!duplicated(merged),]
    
    #Convert to date type for birthdate and consent date
    merged_nodupl["BIRTHDATE_FORMAT"] = as.Date(convert_date(merged_nodupl$birthdate_raw))
    merged_nodupl["CONSENTDATE_FORMAT"] = as.Date(convert_date(merged_nodupl$consentdate_raw))
    
    #Calculate the age of each subject
    merged_nodupl["Age"]=difftime(merged_nodupl$CONSENTDATE_FORMAT, merged_nodupl$BIRTHDATE_FORMAT, units = "days")
    merged_nodupl['Age_years']=as.numeric(round(merged_nodupl$Age/365.25,digits=0))
    
    #Create Age group
    merged_nodupl$group[merged_nodupl$Age_years<17] <-"Pediatric"
    merged_nodupl$group[merged_nodupl$Age_years>=17 & merged_nodupl$Age_years<65] <- "Adult"
    merged_nodupl$group[merged_nodupl$Age_years>=65] <- "Elderly"
    
    #Create New Ethnicity table with levels as Hispanic,Non-Hispanic,Unknown or Not Reported)
    merged_nodupl = mutate(merged_nodupl, ETHNICITY_RECODE = ifelse( ethnicity %in% "Hispanic or Latino", "Hispanic",ifelse(ethnicity %in% "Non-Hispanic or Latino", "Non-Hispanic", "Unknown or Not Reported")))
    
    
    #Table for Race
    #Combining Pacific islander and asian
    merged_nodupl["RACEAP"]=merged_nodupl[,"racepacificislander"]+merged_nodupl[,"raceasian"]
    
    #combining unknown and nondisclosed as unknown or not reported
    demo_race_unknown_nondisclosed =merged_nodupl[,"raceunknown"]+merged_nodupl[,"racenondisclosed"]
    
    #Extract the race columns
    demo_race_more = merged_nodupl[,c("racecaucasian","raceblack","RACEAP","racenativeamerican")]
    
    #Add "Other" column
    demo_race_more["Other"]=0
    
    #Find the total number of subjects
    num_subjects = dim(demo_race_more)[1]
    
    #check if there is more than one race selected per row, if there is, add one to "Other" and reset to zero for the other races
    for(i in 1:num_subjects)
    {
      row = rowSums(demo_race_more[i,])
      if (row >1)
      {
        #add one to "Other"
        demo_race_more[i,"Other"]=1
        #reset the other races to zero
        demo_race_more[i,1:4]=0
      }
    }
    
    #combine columns
    comb = cbind(demo_race_more,demo_race_unknown_nondisclosed)
    
    #rename columns
    names(comb)<-c("W","B","A/PI","AI","O","UNK")
    
    #reorder column
    comb = comb[c("AI","A/PI","B","W","O","UNK")]
    
    #Adding race combination column to merged_nodupl dataframe
    merged_nodupl= cbind(merged_nodupl,comb)
    
    ################### Split by treatment arm
    split_by_trt = split(merged_nodupl,merged_nodupl$treatmentarm)
    
    ################### Create table for each strata
    ### Table for Age group
    age_table = lapply(split_by_trt,function(x) as.data.frame(table(factor(x$group,levels=c("Pediatric","Adult","Elderly")))))
    
    #get names of columns
    cnames = str_wrap(names(split_by_trt),width = 10)
    
    
    #Convert to data frame
    age_table_df = as.data.frame(age_table)
    
    #substitute only column name with .var1
    names(age_table_df)[grep("var1",names(age_table_df),ignore.case = T)]<-cnames
    
    #### Table for Gender
    gender_table = lapply(split_by_trt,function(x) as.data.frame(table(factor(x$gender,levels=c("Male","Female")))))
    
    #Convert to data frame
    gender_table_df = as.data.frame(gender_table)
    
    #Rename columns's names
    #substitute only column name with .var1
    names(gender_table_df)[grep("var1",names(gender_table_df),ignore.case = T)]<-cnames
    
    #### Table for Ethnicity
    t_ethnicity = lapply(split_by_trt, function(x) as.data.frame(table(factor(x$ETHNICITY_RECODE,levels=c("Hispanic","Non-Hispanic","Unknown or Not Reported")))))
    
    #Convert to data frame
    eth_table_df = as.data.frame(t_ethnicity)
    
    #Rename columns' names
    #substitute only column name with .var1
    names(eth_table_df)[grep("var1",names(eth_table_df),ignore.case = T)]<-cnames
    
    #### Table for Race
    #count number of subjects per race
    extract_race = lapply(split_by_trt, function(x) as.data.frame(x[,c("AI","A/PI","B","W","O","UNK")]))
    race_count = lapply(extract_race,function(x) as.data.frame(t(apply(x,2,sum))))
    t_race = as.data.frame(lapply(race_count,function(x) as.data.frame(cbind(names(x),t(x)))))
    
    #Rename columns' names
    #match t_race column names with other
    names(t_race)<-names(eth_table_df)
    
    
    #combine
    combined = rbind(age_table_df,gender_table_df,t_race,eth_table_df)
    combined_t = t(combined)
    
    label.row = combined_t[1,]
    combined_label = rbind(label.row,combined_t)
    combined_label[1,1:3]=c("","Age Group"," ")
    combined_label[1,4:5]=c("Gender"," ")
    combined_label[1,6:11]=c("Race"," "," "," "," "," ")
    combined_label[1,12:14]=c("Ethnicity"," "," ")
    
    combined_label = as.data.frame(combined_label)
    names(combined_label)<-NULL
    
    colnames(combined_label)<-c(" ","Age Group"," ","Gender"," ","Race"," "," "," "," "," ","Ethnicity"," "," ")
    combined_label = combined_label[-1,]
    
    row.names(combined_label)<-str_wrap(row.names(combined_label),width = 10)
    row.names(combined_label)[grep("[Freq]$",row.names(combined_label),ignore.case = T)]<-c("Total1","Total2")
    
    
    combined_label
}