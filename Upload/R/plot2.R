#function to plot if dimensions of row is greater than 3 and less than 6

plot2 <-function(dataset,rows,nx,ny)
{
  eth_trans2 = as.data.frame(t(dataset))
  eth_cols2 = eth_trans2[,1:4]
  colunames = grep("^[^Tot]",names(eth_cols2),ignore.case = T,value = TRUE)
  eth_rows2 = eth_cols2[rows,]
  val2 = melt(eth_rows2,id.vars = colunames)
  tot2 = melt(eth_rows2,id.vars = c("Total1","Total2"))
  tot2_new = tot2[,3:4]
  long_table2 = cbind(tot2_new,val2[,4])
  #print(long_table2)
  names(long_table2)=c("Variable",nx,ny)
 
  long_table2$Freq = as.numeric(levels(long_table2$Freq))[long_table2$Freq]
  # Faceting
  ggplot(long_table2, aes_string(y=ny, x=nx, color=nx, fill=nx)) +
    geom_bar( stat="identity") +geom_text(aes(label = long_table2$Freq,group=nx),  position = position_dodge(width = 1),vjust = -0.5, size = 3,color="black")+
    facet_wrap(~Variable)+scale_y_continuous(breaks=round(seq(0, max(long_table2[,ny]),1),1))+scale_x_discrete(limits=long_table2[,nx])+theme(axis.text.x = element_text(angle = 90, vjust = 0.3,hjust=1))
  #print(long_table2)
}