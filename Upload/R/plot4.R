#function to plot if dimension is greater than 6.

plot4 <- function(dataset,rows,nx,ny)
{

  eth_trans4 = as.data.frame(t(dataset))
  eth_cols4 = eth_trans4[,1:8]
  eth_rows4 = eth_cols4[rows,]
  val4 = melt(eth_rows4,id.vars = c("Biopsy","Dual","ICV","Resection"))
  tot4 = melt(eth_rows4,id.vars = c("Total1","Total2","Total3","Total4"))
  tot4_new = tot4[,5:6]
  long_table4 = cbind(tot4_new,val4[,6])
  names(long_table4)=c("Strata",nx,ny)
  long_table4$Freq = as.numeric(levels(long_table4$Freq))[long_table4$Freq]
  if(nx=="Age")
  {
    long_table4[nx]<-factor(long_table4[,nx],levels=c("Pediatric", "Adult", "Elderly"))
  }
  # Faceting
  ggplot(long_table4, aes_string(y=ny, x=nx, color=nx, fill=nx)) + 
    geom_bar( stat="identity") + geom_text(aes(label = long_table4$Freq,group=nx),  position = position_dodge(width = 1),vjust = -0.5, size = 2.5,color="black")+   
    facet_wrap(~Strata)+theme(strip.background = element_rect(size=2))+scale_y_continuous(breaks=round(seq(0, max(long_table4[,ny])+5,1),1))+theme(axis.text.x = element_text(angle = 90, vjust = 0.3,hjust=1)) 
}