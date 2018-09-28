plot_overall_best<- function(data)
{
  #finding label if different column name
  label = grep("^Overall",names(data),ignore.case=TRUE,value = TRUE)
  
  overall_br = data.frame(data$Subject,data[label])
  names(overall_br)<-c("Subject","Overall")
  
  #add "NA" to empty cells
  overall_br$Overall =  empty_as_na(overall_br$Overall)
 
   #check if there is any NA in progression column,if there is,convert to character "NA"
  if(any(is.na(overall_br$Overall)))
  {
    overall_br$Overall= ifelse(is.na(overall_br$Overall),"NA",overall_br$Overall)
  }
  
  overall_aggr = aggregate(Subject~Overall,FUN=length,data=overall_br,na.action = NULL)
  
  names(overall_aggr)<-c("Overall","Frequency")
  
  #set the width of the overall column
  overall_aggr$Overall <- str_wrap(overall_aggr$Overall, width = 10)
  
  ggplot(overall_aggr,aes(x=Overall,y=Frequency,fill=Overall))+geom_bar(stat="identity")+geom_text(aes(label = overall_aggr$Frequency),  position = position_dodge(width = 1),vjust = -0.5, size = 4,color="black")+
    scale_x_discrete(labels = function(Overall) str_wrap(Overall, width = 10))+ ggtitle("Overall Best Response")+theme(plot.title = element_text(hjust = 0.5))+theme(axis.text.x = element_text(angle = 90, hjust = 1,vjust=0.6))

}