Report_demo_13277_nodb <- function(datasets)
{

    ##################### Data cleanup
    #select the desired columns 
    demo_select = datasets$onstudy.CSV %>% rename_all(tolower) %>% dplyr::select(subject,gender,birthdate_raw,consentdate_raw,ethnicity,racecaucasian,raceblack,racepacificislander,
                                                                                                             raceasian,racenativeamerican,raceunknown,racenondisclosed)
    trt_arm = datasets$transplant.CSV%>% rename_all(tolower) %>% dplyr::select(subject,treatmentarm)  
    
    #################### merge files
    merged = merge(demo_select,trt_arm)
    merged_nodupl = merged[!duplicated(merged),]      
    
    ################### Create Age Group
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
    
    #finding the different wording for unknown,unknown not reported, not reported
    unkw = grep("unknown|not reported",tolower(merged_nodupl$ethnicity),value=TRUE)
    
    #Create New Ethnicity table with levels as Hispanic,Non-Hispanic,Unknown or Not Reported)
    merged_nodupl = mutate(merged_nodupl, ETHNICITY_RECODE = ifelse(tolower(ethnicity) %in% "hispanic or latino", "Hispanic",ifelse(tolower(ethnicity) %in% "non-hispanic or latino", "Non-Hispanic", ifelse(tolower(ethnicity) %in% unkw,str_wrap("Unknown or Not Reported",width = 12),"NONE"))))
    
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
    
    # if there is more than one race selected per row, if there is, add one to "Other" and reset to zero for the other races
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
    
    #rename columns' names
    names(comb)<-c("W","B","A/PI","AI","O","UNK")
    
    #reorder column
    comb = comb[c("AI","A/PI","B","W","O","UNK")]
    
    #Adding race combination column to merged_nodupl dataframe
    merged_nodupl= cbind(merged_nodupl,comb)
    
    ################### Split by treatment arm
    split_by_treatment = split(merged_nodupl,merged_nodupl$treatmentarm)
    
    ################### Create table for each category
    ### Table for Age group
    age_table = lapply(split_by_treatment,function(x) as.data.frame(table(factor(x$group,levels=c("Pediatric","Adult","Elderly")))))
    
    #Convert to data frame
    age_table_df = as.data.frame(age_table)
    
    #Rename columns's names
    colnames(age_table_df)<-c("Arm 1: AML","Total1","Arm 2: BPDCN","Total2")
    
    #### Table for Gender
    gender_table = lapply(split_by_treatment,function(x) as.data.frame(table(factor(x$gender,levels=c("Male","Female")))))
    
    #Convert to data frame
    gender_table_df = as.data.frame(gender_table)
    
    #Rename columns's names
    colnames(gender_table_df)<-c("Arm 1: AML","Total1","Arm 2: BPDCN","Total2")
    
    
    #### Table for Ethnicity
    t_ethnicity = lapply(split_by_treatment, function(x) as.data.frame(table(factor(x$ETHNICITY_RECODE,levels=str_wrap(c("Hispanic","Non-Hispanic","Unknown or Not Reported"),width = 12)))))
    
    #Convert to data frame
    eth_table_df = as.data.frame(t_ethnicity)
    
    #Rename columns' names
    colnames(eth_table_df)<-c("Arm 1: AML","Total1","Arm 2: BPDCN","Total2")
    
    
    #### Table for Race
    #count number of subjects per race
    extract_race = lapply(split_by_treatment, function(x) as.data.frame(x[,c("AI","A/PI","B","W","O","UNK")]))
    race_count = lapply(extract_race,function(x) as.data.frame(t(apply(x,2,sum))))
    t_race = as.data.frame(lapply(race_count,function(x) as.data.frame(cbind(names(x),t(x)))))
    
    #Rename columns' names
    colnames(t_race)<-c("Arm 1: AML","Total1","Arm 2: BPDCN","Total2")
    
    #combine
    combined = rbind(age_table_df,gender_table_df,t_race,eth_table_df)
    combined_t = t(combined)
    label.row = combined_t[1,]
    combined_label = rbind(label.row,combined_t)
    combined_label[1,1:3]=c("","Age Group"," ")
    combined_label[1,4:5]=c("Gender"," ")
    combined_label[1,6:11]=c("Race"," "," "," "," "," ")
    combined_label[1,12:14]=c("Ethnicity"," "," ")
    combined_label_df = as.data.frame(combined_label)
    names(combined_label_df)<-NULL
    
    colnames(combined_label)<-c(" ","Age Group"," ","Gender"," ","Race"," "," "," "," "," ","Ethnicity"," "," ")
    combined_label = combined_label[-1,]
    
    combined_label
}
