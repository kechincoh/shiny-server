# function to check for correct best response, progression, and last contact date. In addition, it checks to replace "Previously Reported" with respective outcome and "relapsed" to "progression"
check_nodb <- function(mtable,bresp,lrecord)
{
  latest.record = lrecord
  #find the bestresp_std column's name(label)
  bestresp_std_col_label = grep("^BEST.*STD$",names(mtable),value=TRUE)
  mtable_bestrespstd_label = grep("^BEST.*STD$",names(mtable),value=TRUE)
  
  #finding the bestresp column's name(label)
  bestresp_label = grep("^BESTRESPFU",names(mtable),value=TRUE)
  bestresp_col_label = bestresp_label[!(bestresp_label %in% bestresp_std_col_label)]
  mtable_col_label = bestresp_col_label
  
  #Check to see if there is a category "relapse" for best response, if there is, change to progressed/relapse and choose earliest date
  Relapse_Number = length(which(tolower(bresp[,bestresp_col_label])=="relapse"))

  if(Relapse_Number>0)
  {
    #recode the best response
    bresp[paste("NEW",bestresp_col_label,sep="_")] = ifelse((bresp[bestresp_col_label]=="Relapse"|bresp[bestresp_col_label]=="Progression"),"Progressed/Relapsed",bresp[,bestresp_col_label])
    
    #recode the best response std
    bresp[paste("NEW",bestresp_std_col_label,sep="_")] = ifelse(bresp[bestresp_col_label]=="Relapse",bresp[which(bresp[bestresp_col_label]=="Progression"),bestresp_std_col_label][1],bresp[,bestresp_std_col_label])
    
    #finding the new bestresp column's name(label)
    bestresp_col_label = paste("NEW",bestresp_col_label,sep="_")
     
    #find the bestresp_std column's name(label)
    bestresp_std_col_label = paste("NEW",bestresp_std_col_label,sep="_")
  }
  #fill empty cell with "NA" in best response std
  bresp[bestresp_std_col_label]=empty_as_na(bresp[,bestresp_std_col_label])
  
  #split dataframe by subject
  split_by_subject = split(bresp,bresp$Subject)

  #sort progression date (increasing order), last contact date (decreasing order)
  ordered_prog_bestresp = lapply(split_by_subject, function(x) x[order(x$PROGRESSIONDATE_FORMAT,-as.numeric(x$LASTCONTACTDATE_FORMAT)),])
  
  #combining previous table with new column (bestrespfu order)
  ordered_bestresp =lapply(ordered_prog_bestresp, function(x) cbind(x,BESTRESP_ORDER=x[order(x[bestresp_std_col_label]),bestresp_col_label]))
  
  #convert to data frame
  bestres_new = data.frame(Reduce(rbind,ordered_bestresp),stringsAsFactors=FALSE)
  
  #change "NA" to Not available
  bestres_new["PROGRESSIONYN"]=ifelse(is.na(bestres_new$PROGRESSIONYN),"Not Available",bestres_new$PROGRESSIONYN)
  print("There are NA changed to Not available")
  
  
  #check "BESTRESP_ORDER" column is present,if not, rename
  if(!("BESTRESP_ORDER" %in% names(bestres_new)))
  {
    names(bestres_new)[which(names(bestres_new)==names(bestres_new[length(bestres_new)]))]<-"BESTRESP_ORDER"
  }
  
  #remove duplicates
  bestres_unique = bestres_new[!duplicated(bestres_new$Subject),]
  
  unique_subj = unique(mtable$Subject)

  for (k in 1:length(unique_subj))
  {
    #Check if there is match of subjects between the two tables
    matched_subjects_bestresp = mtable$Subject[k] %in% bestres_unique$Subject
  
    #check if there is a match in best response between the two tables
    matched_bestresp = mtable[k,mtable_col_label] %in% bestres_unique[which(bestres_unique$Subject==mtable$Subject[k]),"BESTRESP_ORDER"]
    
    #if there is a "NA" in best response from original table or matched subjects, I want to check if it's correct in bestres_unique table
    if((is.na(mtable[k,mtable_col_label]) | matched_subjects_bestresp) )    
    {
       #proceed only if the best response from bestresp_unique table is not "NA", there is a matching subject and no matching best response in both table
      if(!is.na(matched_bestresp) & !matched_bestresp & matched_subjects_bestresp)
      {
        #match Subject from my table and best response table and replace the best response based on response table
        mtable[k,mtable_col_label] = as.character(bestres_unique[which(bestres_unique$Subject==mtable$Subject[k]),"BESTRESP_ORDER"])
      }
    }
    #optional check to fix the progression and progression date if not correct the first time
    if(matched_subjects_bestresp)
    {
      
       mtable[k,"PROGRESSIONYN"] = bestres_unique[which(bestres_unique$Subject==mtable$Subject[k]),"PROGRESSIONYN"]
       mtable[k,"PROGRESSIONDATE_FORMAT"] = bestres_unique[which(bestres_unique$Subject==mtable$Subject[k]),"PROGRESSIONDATE_FORMAT"]
     
      #check to see if progressionyn has an unknown value; if there is, select previous answer
      if(tolower(mtable$PROGRESSIONYN[k])=="unknown" & nrow(bestres_new[which(bestres_new$Subject==mtable$Subject[k]),])>1)
      {
        mtable[k,"PROGRESSIONYN"] = bestres_new[which(bestres_new$Subject==mtable$Subject[k])[1]+1,"PROGRESSIONYN"]
      }
    }
    
    #Optional check to fix the latest contact date if not correct the first time
    if(matched_subjects_bestresp)
    {
      mtable[k,"LASTCONTACTDATE_FORMAT"] = as.character(latest.record[which(latest.record$Subj==mtable$Subject[k]),"x"])
    }
  }
  #check if there is "previously reported",if there is replace with previous answer
  if(any(tolower(bestres_new$PROGRESSIONYN)=="previously reported"))
  {
    #find number of "previously reported" for each subject
    g= lapply(ordered_bestresp, function(x) x$PROGRESSIONYN== "Previously Reported")
    h = lapply(g, function(x) length(which(x=='TRUE')))
    h_df = data.frame(Reduce(rbind,h),stringsAsFactors=FALSE)
    
    num_previously = cbind( unique(bestres_new$Subject),h_df)
    names(num_previously) <- c("Subject","Num_prev")
    
    #combine with dataframe
    bestres_new = merge(bestres_new,num_previously)
    
    #recode to fix error
    bestres_new["num_previously_recode"]= ifelse(tolower(bestres_new$PROGRESSIONYN)=="previously reported"&bestres_new$Num_prev>0,bestres_new$Num_prev,0)
    
    #finding unique subjects
    bestres_new_unique = bestres_new[!duplicated(bestres_new$Subject),]
    
    #How many subjects have more than 1 "previosly recorded"
    n_prev = which(bestres_new_unique$num_previously_recode>=1)
    
    for(d in n_prev)
    {
      #find subject with more than 1 "previously recorded"
      subj = bestres_new_unique[d,"Subject"]
      
      #find the number of repeat
      num_p = bestres_new_unique[d,"num_previously_recode"]
      
      #matching subject in bestres_new to mtable
      mtable_d = which(mtable$Subject==subj)
      
      mtable[mtable_d,"PROGRESSIONYN"] = bestres_new[which(bestres_new$Subject==subj)[1]+num_p,"PROGRESSIONYN"]
    }
  }
  
  #convert last contact date to date type
  mtable$LASTCONTACTDATE_FORMAT=as.Date(mtable$LASTCONTACTDATE_FORMAT)
  mtable$PROGRESSIONYN =ifelse(tolower(mtable$PROGRESSIONYN)=="not available","NA",mtable$PROGRESSIONYN)
  return(mtable)
}