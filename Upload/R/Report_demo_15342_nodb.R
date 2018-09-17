Report_demo_15342_nodb<-function(datasets)
{
    #Data cleanup
    #select the desired columns 
    
    demo_select = datasets$demo.CSV %>% rename_all(. %>% tolower %>% gsub("demo", "", .)) %>% dplyr::select(subject,gender,birthdate_raw,ethnicity,racecaucasian,raceblack,racepacificislander,
                                                                                                             raceasian,racenativeamerican,raceunknown,racenondisclosed)
    onstudy_consent =  datasets$onstudy.CSV %>% rename_all(. %>% tolower %>% gsub("demo", "", .)) %>% dplyr::select(subject,consentdate_raw)
    
    #################### merge files
    merged = merge(demo_select,onstudy_consent)
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
    
    #Table for Age group
    age_table = as.data.frame(table(factor(merged_nodupl$group,levels=c("Pediatric","Adult","Elderly"))))
    
    #Table for Gender
    gender_table = as.data.frame(table(factor(merged_nodupl$gender,levels=c("Male","Female"))))
    
    #Table for Ethnicity
    #check levels of categorical variable for correct output
    check = length(levels(factor(merged_nodupl$ethnicity)))
    {
      if(check<3){
        t_ethnicity = table(factor(merged_nodupl$ethnicity,levels=c("Hispanic or Latino","Non-Hispanic or Latino","Unknown or Not Reported")))
      } 
      else {
        t_ethnicity = table(merged_nodupl$ethnicity)
      }
    }
    #Renaming column names
    names(t_ethnicity)<-str_wrap(c("Hispanic","Non-Hispanic","Unknown or Not Reported"),width = 12)
    eth_table =  as.data.frame(t_ethnicity)
    
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
    
    #count number of subjects per race
    race_count = as.data.frame(t(apply(comb,2,sum)))
    
    #change format of table
    nam = names(race_count)
    race_count_new = rbind(nam,race_count)
    t_race=as.data.frame(t(race_count_new))
    colnames(t_race)=c("Var1","Freq")
    
    
    #combine
    combined = rbind(age_table,gender_table,t_race,eth_table)
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
    
    combined_label

}


