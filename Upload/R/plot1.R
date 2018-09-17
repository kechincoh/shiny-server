#function to plot if dimension is less than 4 rows

plot1 <-function(dataset,rows,nx,ny)
{
  eth_trans =as.data.frame(t(dataset))
  eth_cols = eth_trans[,1:2]
  eth_rows = eth_cols[rows,]
  #eth_rows[ny] = as.numeric(levels(eth_rows$V3))[eth_rows$V3]
  #eth_rows[,ny] = as.numeric(as.character(eth_rows$V3))
  #print(((str(eth_rows))))
 # eth_rows[name] = eth_rows[,"V2"]
  names(eth_rows)<-c(nx,"Freq")
  eth_rows$Freq = as.numeric(levels(eth_rows$Freq))[eth_rows$Freq]
  
  ggplot(data=eth_rows, aes_string(x=nx, y=ny,fill=nx)) +
    geom_bar(stat="identity")+scale_x_discrete(limits=eth_rows[,nx])+scale_y_continuous(breaks=round(seq(0, max(eth_rows[,ny]),1),1))+geom_text(aes(label = eth_rows$Freq ,group=nx),  position = position_dodge(width = 1),vjust = -0.5, size = 3,color="black") 
  #print(eth_trans)
}