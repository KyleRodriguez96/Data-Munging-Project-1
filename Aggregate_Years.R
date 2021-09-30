#Group Project 1
  #Function below combines all years into a single dataframe.
  #.csv's to combine should be in the same directory, ideally by themselves.
  #The output has 1 added variable, 'year', which is the *start* year of that period.
    #So, 1996_97 is just 1996.
  

  #Combining 1996-2019 takes ~3-4 minutes on my PC.
  #Dataframe can be saved as a single .csv, it's about 4 gigabytes.




#Parameters needed to run the combine_year_data function.
directory.import<-"C:\\File\\group_project_1\\"
year.start<-1996
year.end<-2019
#Parameters to save the output dataframe
directory.export<-"C:\\File\\group_project_1\\"
file.export_name<-"all_years.csv"

#Function to combine .csv's
combine_year_data<-function(directory.import,year.start,year.end){
  #directory.import is a string of the directory your yearly .csv's are stored in
  #year.start and year.end are integers between 1996 and 2019 inclusive.
  
  #Checks all files in directory.import for matching year.
  #If year matches, reads & adds a 'year' column with 4 digit integer year.
  #Combines all matching files into one dataframe & returns combined dataframe
  print(paste('Start ', Sys.time()))
    if(year.end<year.start){
    print('Invalid year range.')
  }
  files.list <- list.files(path=directory.import)
  if(length(files.list)==0){
    print(paste('No files found in directory: ',directory.import,sep=""))
  }
  year.list<-seq(year.start,year.end,by=1)
  output_DF<-data.frame()
  for(file.name in files.list){
    for(year.int in year.list){
      year.string<-toString(year.int)
      bTest<-grepl(year.string,file.name,fixed=TRUE)
      if(bTest){
        file.full_name<-paste(directory.import,file.name,sep="")
        
        #input_DF<-read.csv(file.full_name) #~4 minutes for all
        input_DF<-fread(file.full_name) #3.5 minutes for all
        
        input_DF$year=year.string
        
        #Combine data
        if(nrow(output_DF)>0){
          output_DF<-rbind(output_DF,input_DF)
        }else{output_DF<-input_DF
        }
        
      }
    }
  }
  print(paste('Finish ', Sys.time()))
  return(output_DF)
}

#Run function & Save output
combined_DF<-combine_year_data(directory.import,year.start,year.end)
fwrite.csv(combined_DF,paste(directory.export,file.export_name,sep=""),row.names=FALSE)


