Report_demo_13401_nodb <- function(datasets)
{

    #Data cleanup
    #select the desired columns 
    demo_select = datasets$demo.CSV %>% rename_all(tolower) %>% select(subject,gender,birthdate_raw,consentdate_raw,ethnicity,racecaucasian,raceblack,racepacificislander,
                                                                                                             raceasian,racenativeamerican,raceunknown,racenondisclosed)
    #Convert to date type for birthdate and consent date
    demo_select["BIRTHDATE_FORMAT"] = as.Date(convert_date(demo_select$birthdate_raw))
    demo_select["CONSENTDATE_FORMAT"] = as.Date(convert_date(demo_select$consentdate_raw))
    
    #Calculate the age of each subject
    demo_select["Age"]=difftime(demo_select$CONSENTDATE_FORMAT, demo_select$BIRTHDATE_FORMAT, units = "days")
    demo_select['Age_years']=as.numeric(round(demo_select$Age/365.25,digits=0))
    
    #Create Age group
    demo_select$group[demo_select$Age_years<17] <-"Pediatric"
    demo_select$group[demo_select$Age_years>=17 & demo_select$Age_years<65] <- "Adult"
    demo_select$group[demo_select$Age_years>=65] <- "Elderly"
    
    #Table for Age group
    age_table = as.data.frame(table(factor(demo_select$group,levels=c("Pediatric","Adult","Elderly"))))
    
    #Table for Gender
    gender_table = as.data.frame(table(demo_select$gender))
    
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
    #combined_label = as.data.frame(combined_label)
    names(combined_label)<-NULL
    
    colnames(combined_label)<-c(" ","Age Group"," ","Gender"," ","Race"," "," "," "," "," ","Ethnicity"," "," ")
    combined_label = combined_label[-1,]
    
    combined_label

}

