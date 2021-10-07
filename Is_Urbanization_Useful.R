#Do not edit combined_DF
df<-combined_DF

#Limit to 2019 where we have valid LOCALE data
df.2019<-df[df$year==2019]

#Vector for translating region numbers to names
region.names<-c("New England","Mid East","Great Lakes","Plains","Southeast","Southwest","Rocky Mountains","Far West")
urbanization.values<-c(11,12,13,21,22,23,31,32,33,41,42,43)


#Get stats of each region
region.urb.means<-vector(mode="numeric",length=8)
region.urb.null<-vector(mode="numeric",length=8)
region.counts<-vector(mode="numeric",length=8)

df.test<-df.2019$LOCALE[df.2019$LOCALE=="NULL"]
length(df.test)
#Save regional statistics
for(i in 1:8){
  region.urb.means[i]<-round(mean(as.numeric(df.2019$LOCALE[df.2019$REGION==i]),na.rm=TRUE),2)
  region.counts[i]<-sum(as.numeric(df.2019$REGION[df.2019$REGION==i]))/i
  region.urb.null[i]<-df.2019$LOCALE[df.2019$LOCALE=="NULL"]
}

#Collect into a readable format
region.named_stats<-cbind(region.names,region.counts,region.urb.means)
region.named_stats

#Urbanization Table
urbanization.table<-table(df.2019$LOCALE[df.2019$REGION==1])
for(i in 2:8){
  urbanization.table<-rbind(urbanization.table,table(df.2019$LOCALE[df.2019$REGION==i]))
}
urbanization.table<-cbind(region.names,urbanization.table)
urbanization.table