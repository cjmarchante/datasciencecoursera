pollutantmean<- function(directory, pollutant, id=1:332) {
 

  # find all files in the folder
  file_list <- list.files(directory) 
  data<-c()
  for (monitor in id) {
    #dir + filename
    fname<-paste(directory,"/",file_list[monitor],sep="")
    #read the file  
    filedata<-read.csv(fname)
    #adding pollutant removing NA
    data <-c( data,  filedata[!is.na(filedata[, pollutant]), pollutant]) 

  }
  
  #calculate mean
  sum(data)/length(data)
}
 


complete<- function(directory, id=1:332) {
  
  file_list <- list.files(directory)
  datacompletecases<-c()
for (file_id in id) {
    #dir + filename
    fname<-paste(directory,"/",file_list[file_id],sep="")
    #read the file  
    filedata<-read.csv(fname)
    #adding pollutant removing NA
    datacompletecases <-c(datacompletecases,sum(complete.cases(filedata))) 
  }
 data.frame(id=id,nobs=datacompletecases)
   
}


corr <- function(directory, threshold = 0) {
  # get the complete table
  complete_table <- complete(directory, 1:332)
  nobs <- complete_table$nobs
  # find the valid ids
  ids <- complete_table$id[nobs > threshold]
  # get the length of ids vector
  id_len <- length(ids)
  corr_vector <- rep(0, id_len)
  # find all files in the specdata folder
  all_files <- as.character( list.files(directory) )
  file_paths <- paste(directory,"/", all_files, sep="")
  j <- 1
  for(i in ids) {
    current_file <- read.csv(file_paths[i], header=T, sep=",")
    corr_vector[j] <- cor(current_file$sulfate, current_file$nitrate, use="complete.obs")
    j <- j + 1
  }
  result <- corr_vector
  result   
}


 
  ## mydir "/Users/cjmarchante/Desktop/My Data/Data Science Specialization/2 Rprogramming/week 2/week 2 assigment/specdata"
  