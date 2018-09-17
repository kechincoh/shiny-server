#function to combine the progression and relapse column and their dates updated on 071018

check_prog_relapsed <- function(mtable,fu_act)
{
  #########  check if dates column are in date format,if not convert #####################
  dates_column = c("PROGRESSIONDATE_FORMAT","FURELAPSEDATE_FORMAT","LASTCONTACTDATE_FORMAT")
  fu_dates = fu_act[dates_column]
  
  #finding which dates column are not in Date format,is not all column date type? (optional check-if not done first step)
  if(!all(sapply(fu_dates,is.Date)))
  { 
    fu_not_dates = fu_dates[which(sapply(fu_dates,is.Date)=="FALSE")]
    
    #convert to date type
    fu_not_dates_a =  lapply(fu_dates,convert_date)
    fu_not_dates_df = as.data.frame(lapply(fu_not_dates_a,as.Date))
    
    #change label to "FORMAT" ending
    names(fu_not_dates_df) = gsub("RAW","FORMAT",names(fu_not_dates_df))
    
    #combine to fu_act
    fu_prog_relapsed = cbind(fu_act,fu_not_dates_df)
  }
  
  
#select progression and relapse columns and their respective dates
prog_relapse_column = c("Subject","PROGRESSIONYN","PROGRESSIONDATE_FORMAT","FURELAPSEYN","FURELAPSEDATE_FORMAT","LASTCONTACTDATE_FORMAT")
fu_prog_relapsed = fu_act[prog_relapse_column]


#split by subjects
fu_prog_relapsed_split = split(fu_prog_relapsed,fu_prog_relapsed$Subject)

#combine the columns by subject
comb_prog_relapsed = lapply(fu_prog_relapsed_split,comb_columns)

#sort progression date then progressionyn(decreasing order)
comb_ordered = lapply(comb_prog_relapsed, function(x) x[order(x[,"dataset.PROGRESSIONDATE_FORMAT"],x[,"dataset.PROGRESSIONYN"],x[,"dataset.LASTCONTACTDATE_FORMAT"]),])

#convert to dataframe
comb_ordered_df = as.data.frame(do.call(rbind,comb_ordered))

#rename the columns
names(comb_ordered_df) = c("Subject","PROGRESSIONYN","PROGRESSIONDATE_FORMAT","LASTCONTACTDATE_FORMAT")

#remove duplicated subjects
comb_unique = comb_ordered_df[!duplicated(comb_ordered_df$Subject),]

#check to fix/replace progressionyn and progression date with the newly created table(the progression and relapse as one column)
unique_subj = unique(mtable$Subject)

for (k in 1:length(unique_subj))
{
  #Check if there is match of subjects between the two tables
  matched_subjects = mtable$Subject[k] %in% comb_unique$Subject
  
  if(matched_subjects)
  {
    mtable[k,"PROGRESSIONYN"] = as.character(comb_unique[which(comb_unique$Subject==mtable$Subject[k]),"PROGRESSIONYN"])
    mtable[k,"PROGRESSIONDATE_FORMAT"] = comb_unique[which(comb_unique$Subject==mtable$Subject[k]),"PROGRESSIONDATE_FORMAT"]
  }
}
#check to see if progressionyn has "NA", if so, delete
if(any(is.na(tolower(mtable$PROGRESSIONYN))))
{
  mtable_nona = mtable[!is.na(tolower(mtable$PROGRESSIONYN)),]
  
}  
#check if there is "previously reported",if there is replace with previous answer(optional)
if( any(tolower(mtable_nona$PROGRESSIONYN)=="previously reported"))
{
  #find number of "previously reported" for each subject
  g= lapply(comb_ordered, function(x) tolower(x$dataset.PROGRESSIONYN)== "previously reported")
  h = lapply(g, function(x) length(which(x=='TRUE')))
  h_df = data.frame(Reduce(rbind,h),stringsAsFactors=FALSE)
  
  num_previously = cbind( unique(comb_ordered_df$Subject),h_df)
  names(num_previously) <- c("Subject","Num_prev")
  
  #combine with dataframe
  comb_ordered_df = merge(comb_ordered_df,num_previously)
  
  #recode to fix error
  comb_ordered_df["num_previously_recode"]= ifelse(tolower(comb_ordered_df$PROGRESSIONYN)=="previously reported"&comb_ordered_df$Num_prev>0,comb_ordered_df$Num_prev,0)
  
  #finding unique subjects
  comb_ordered_df_unique = comb_ordered_df[!duplicated(comb_ordered_df$Subject),]
  
  #How many subjects have more than 1 "previosly recorded"
  n_prev = which(comb_ordered_df_unique$num_previously_recode>=1)
  
  for(d in n_prev)
  {
    #find subject with more than 1 "previously recorded"
    subj = comb_ordered_df_unique[d,"Subject"]
    
    #find the number of repeat
    num_p = comb_ordered_df_unique[d,"num_previously_recode"]
    
    #matching subject in bestres_new to mtable
    mtable_d = which(mtable$Subject==subj)
    
    mtable[mtable_d,"PROGRESSIONYN"] = as.character.factor(comb_ordered_df[which(comb_ordered_df$Subject==subj)[1]+num_p,"PROGRESSIONYN"])
    
    #if new progressionyn still previously, search until it's not "previously reported" and greater than the index of subject
    i=1
    while(tolower(mtable[mtable_d,"PROGRESSIONYN"])=="previously reported" & which(comb_ordered_df$Subject==subj)[1])
    {
      mtable[mtable_d,"PROGRESSIONYN"] = as.character.factor(comb_ordered_df[which(comb_ordered_df$Subject==subj)[1]+num_p-i,"PROGRESSIONYN"])
      i=i+1
    }
  }
}

mtable
}