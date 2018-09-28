plot_overall_prog <- function(data)
{
  
  prog = data.frame(data$Subject,data$`Progression?`)
  
  # #add "NA" to empty cells
   prog$data..Progression.. =  empty_as_na(prog$data..Progression..)
  
  #check if there is any NA in progression column,if there is,convert to character "NA"
  if(any(is.na(prog$data..Progression..)))
  {
    prog["new"]= ifelse(is.na(prog$data..Progression..),"NA",prog$data..Progression..)
  }else
  {
    prog["new"]=prog$data..Progression..
  }
    
  
  prog_new = aggregate(data.Subject~new,FUN=length,data=prog)
  
  names(prog_new)<-c("Progression","Count")
  if(nrow(prog_new)>2)
  {
    levels(prog_new$Progression) <- c("NA", "NO","Yes")
  }else
  {
    levels(prog_new$Progression) <- c("NO","Yes")
  }
  #levels(prog_new$Progression) <- c("NA", "NO","Yes")
  rows = nrow(prog_new)

   percentlabels = round(100*prog_new$Count/sum(prog_new$Count),1)

   pie(prog_new$Count,labels = paste(percentlabels,"%"),main="Overall Progression",col = rainbow(length(prog_new$Count)),clockwise = TRUE) 
   legend("topright",prog_new$Progression, cex = 0.8, fill = rainbow(length(prog_new$Count)))
   
}
