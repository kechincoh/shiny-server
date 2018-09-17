Report_death_13384_nodb <- function(datasets)
{
    #Data cleanup
    #select the desired columns 
    fu_death =  datasets$fu.CSV %>% rename_all(tolower) %>% dplyr::select(subject,deathdate_raw,vitalstatus,deathcause)
    offtx_lastprot =  datasets$offtx.CSV %>% rename_all(tolower) %>% dplyr::select(subject,lastprottxdate_raw)
    
    #merge files
    merged = merge(offtx_lastprot,fu_death)
    merged_nodupl = merged[!duplicated(merged),]
    
    #Finding the dead subjects
    merged_dead =merged_nodupl[which(merged_nodupl$vitalstatus=="Dead"),]
    
    #optional sort to pick the cause of death and remove duplicated subjects
    merged_dead = merged_dead[order(merged_dead$subject,desc(merged_dead$deathcause)),]
    merged_dead = merged_dead[!duplicated(merged_dead$subject),]
    
    #Convert date type
    merged_dead["LASTPROTTXDATE_FORMAT"] = as.Date(convert_date(merged_dead$lastprottxdate_raw))
    merged_dead["DEATHDATE_FORMAT"] = as.Date(convert_date(merged_dead$deathdate_raw))
    
    
    #Calculate days between last treatment date and death date
    merged_dead["diff"] = merged_dead$DEATHDATE_FORMAT - merged_dead$LASTPROTTXDATE_FORMAT
    
    #change date format to m/d/yy
    merged_dead["LASTPROTTXDATE_FORMAT"]=format(merged_dead$LASTPROTTXDATE_FORMAT,"%m/%d/%y")
    merged_dead["DEATHDATE_FORMAT"]=format(merged_dead$DEATHDATE_FORMAT,"%m/%d/%y")
    
    #remove unnecessary columns
    columns = c("lastprottxdate_raw","deathdate_raw","vitalstatus")
    merged_clean2 = merged_dead[,-which(names(merged_dead) %in% columns)]
    
    #reorder columns
    merged_clean2 = merged_clean2[c("subject","DEATHDATE_FORMAT","LASTPROTTXDATE_FORMAT","diff","deathcause")]
    
    #rename columns
    merged_clean2 = rename(merged_clean2, Subject=subject,"Date of Death"=DEATHDATE_FORMAT, "Last protocol Treatment Date"=LASTPROTTXDATE_FORMAT,"Days Between Last Treatment Date and Death Date"=diff,"Cause of Death"=deathcause)
    
    rownames(merged_clean2)<-c()
    
    merged_clean2
}
