#function to plot if dimension is greater than 10.

plot3 <- function(dataset,rows,nx,ny)
{
  eth_trans3 = as.data.frame(t(dataset))
  eth_cols3 = eth_trans3[,2:7]
  #names(bbb1)
  eth_rows3 = eth_cols3[rows,]
  val3 = melt(eth_rows3,id.vars = c("Unknown","Arm 1: AML","Arm 2: BPDCN"))
  tot3 = melt(eth_rows3,id.vars = c("Total1","Total2","Total3"))
  tot3_new = tot3[,4:5]
  long_table3 = cbind(tot3_new,val3[,5])
  names(long_table3)=c("Variable",nx,ny)
  
  long_table3$Freq = as.numeric(levels(long_table3$Freq))[long_table3$Freq]

  # Faceting
  ggplot(long_table3, aes_string(y=ny, x=nx, color=nx, fill=nx)) + 
    geom_bar( stat="identity") + geom_text(aes(label = long_table3$Freq,group=nx),  position = position_dodge(width = 1),vjust = -0.5, size = 3,color="black")+   
    facet_wrap(~Variable)+scale_y_continuous(breaks=round(seq(0, max(long_table3[,ny]),1),1)) 
}
