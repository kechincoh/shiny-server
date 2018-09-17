Report_demo_14108_nodb<-function(datasets)
{
    ###############  Data cleanup
    #select the desired columns 
    demo_select = datasets$demo.CSV %>% rename_all(tolower) %>% dplyr::select(subject,gender,birthdate_raw,ethnicity,racecaucasian,raceblack,racepacificislander,
                                                                                                             raceasian,racenativeamerican,raceunknown,racenondisclosed)
    
    onstudy_consent = datasets$onstudy.CSV %>% rename_all(tolower) %>% dplyr::select(subject,consentdate_raw)
    
    #merge files
    merged_demo = merge(demo_select,onstudy_consent,by="subject")
    
    #Convert to date type for birthdate and consent date
    merged_demo["BIRTHDATE_FORMAT"] = convert_date(merged_demo$birthdate_raw)
    merged_demo["CONSENTDATE_FORMAT"] = convert_date(merged_demo$consentdate_raw)
    
    #Calculate the age of each subject
    merged_demo["Age"]=difftime(merged_demo$CONSENTDATE_FORMAT, merged_demo$BIRTHDATE_FORMAT, units = "days")
    merged_demo['Age_years']=as.numeric(round(merged_demo$Age/365.25,digits=0))
    
    #Create Age group
    merged_demo$group[merged_demo$Age_years<17] <-"Pediatric"
    merged_demo$group[merged_demo$Age_years>=17 & merged_demo$Age_years<65] <- "Adult"
    merged_demo$group[merged_demo$Age_years>=65] <- "Elderly"
    
    #Table for Age group
    age_table = as.data.frame(table(factor(merged_demo$group,levels=c("Pediatric","Adult","Elderly"))))
    
    #Table for Gender
    gender_table = as.data.frame(table(merged_demo$gender))
    
    #Table for Ethnicity
    #check levels of categorical variable for correct output
    check = length(levels(factor(demo_select$ethnicity)))
    {
      if(check<3){
        t_ethnicity = table(factor(demo_select$ethnicity,levels=c("Hispanic or Latino","Non-Hispanic or Latino","Unknown or Not Reported")))
      } 
      else {
        t_ethnicity = table(demo_select$ethnicity)
      }
    }
    
    #Renaming column names
    names(t_ethnicity)<-str_wrap(c("Hispanic","Non-Hispanic","Unknown or Not Reported"),width = 12)
    eth_table =  as.data.frame(t_ethnicity)
    
    #Table for Race
    #Combining Pacific islander and asian
    demo_select["RACEAP"]=demo_select[,"racepacificislander"]+demo_select[,"raceasian"]
    
    #combining unknown and nondisclosed as unknown or not reported
    demo_race_unknown_nondisclosed =demo_select[,"raceunknown"]+demo_select[,"racenondisclosed"]
    
    #Extract the race columns
    demo_race_more = demo_select[,c("racecaucasian","raceblack","RACEAP","racenativeamerican")]
    
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
