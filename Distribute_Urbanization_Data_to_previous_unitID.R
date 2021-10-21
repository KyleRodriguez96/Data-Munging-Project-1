write_urbanization_to_all_years<-function(df.all){
#This function writes urbanization data (LOCALE) to all UNITIDs based on 2019 urbanization data.
  #Input
    #df.all should be your working data frame, with all yearly data.
  #Output
    #Returns a version of df.all with urbanization data back-filled.
  
  #WARNING: This is SLOW.  There is a .csv copy of the output on GitHub, I suggest you use that if possible.
  df.2019<-df[df.all$year==2019] 
  #Move data to vectors for loop.  This seems to run more smoothly than using the entire df.
  df.id<-df$UNITID
  df.urb<-rep(NA,length(df.id))
  df2019.id<-df.2019$UNITID
  df2019.urb<-df.2019$LOCALE
  
  #Match data between vectors.  Each UNITID is matched as ablock.
  for(i in 1:length(df2019.id)){
    id.val<-df2019.id[i]
    urb.val<-df2019.urb[i]
    
    df.temp <- df.all[df.all$UNITID==id.val]

    df.temp$LOCALE<-urb.val
    
    if(exists("df.urb")){
      df.urb<-rbind(df.urb,df.temp)
    }else{
      df.urb<-df.temp
    }
  }
  
#Combine 'urbanize-filled' previous year data with other yearly data.
df.dif <-setdiff(df.urb,df)
df.urb<-rbind(df.urb,df.dif)
#Clear df.dif for memory
rm(df.dif)  
return(df.urb)
}


