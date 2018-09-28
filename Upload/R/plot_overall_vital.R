plot_overall_vital <-function(data)
{
  label_v = grep("^vital",names(data),ignore.case = TRUE,value = TRUE)
  
  vital = data.frame(data$Subject,data[label_v])
  
  # #add "NA" to empty cells
  vital$Vital.Status =  empty_as_na(vital$Vital.Status)
  
  #check if there is any NA in progression column,if there is,convert to character "NA"
  if(any(is.na(vital$Vital.Status)))
  {
    vital["new"]= ifelse(is.na(vital$Vital.Status),"NA",vital$Vital.Status)
  }else
  {
    vital["new"]=vital$Vital.Status
  }
  
  vital_new = aggregate(data.Subject~new,FUN=length,data=vital)
  
  names(vital_new)<-c("Vital Status","Count")
  
  #levels(prog_new$Progression) <- c("NA", "NO","Yes")
  rows = nrow(vital_new)
  
  percentlabels = round(100*vital_new$Count/sum(vital_new$Count),1)
  
  pie(vital_new$Count,labels = paste(percentlabels,"%"),main="Overall Vital Status",col = c("#999999", "#E69F00", "#56B4E9"),clockwise = TRUE) 
  legend("topright", vital_new$`Vital Status`, cex = 0.8, fill = c("#999999", "#E69F00", "#56B4E9"))
}