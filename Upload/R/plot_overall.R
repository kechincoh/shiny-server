plot_overall <- function(data)
{
  #finding start of treatment date
  label = names(data)[names(data) %in% c("Date of Treatment","Date of HSCT","Date of T-cell Infusion","Date of NSC","Date of\nNSC")][1]
  
  #print(as.Date(data$`Date of Treatment`,format='%m/%d/%y'))
  if(!is.Date(data[,label]))
  {
    a = data.frame(data$Subject,mdy(data[,label]))
  }
  else
  {
    a = data.frame(data$Subject,data[,label])
  }
  names(a)<-c("Subject","Date")
  new = data.frame(a,factor(year(a$Date)))

  test = aggregate(Subject~factor.year.a.Date..,FUN=length,data=new)
  names(test)<-c("Year","Frequency")

  ggplot(test,aes(x=Year,y=Frequency,fill=Year))+geom_bar(stat="identity")+geom_text(aes(label = test$Frequency),  position = position_dodge(width = 1),vjust = -0.5, size = 4,color="black")+
    ggtitle(paste(gsub("Date of","",label),"\n by year"))+theme(plot.title = element_text(hjust = 0.5))

}