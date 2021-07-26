#rename AUDIO FILES

rm(list=ls())
library(tuneR)

workDir = choose.dir(default = "E:\\RESEARCH\\SanctSound\\AUDIO\\", 
                     caption ="Choose director with Audio Files to Rename")
setwd(workDir)
audioFiles <- list.files(pattern=".flac")


#FOR SBO2_08 and SB02_09
#----------------------------------------------------------------------
#CHECK one file
#----------------------------------------------------------------------
ff = 1
pt1 = sapply( strsplit( gsub("[.]","_",audioFiles[ff]) ,"_"),"[", 1 )
pt2 = sapply( strsplit( gsub("[.]","_",audioFiles[ff]) ,"_"),"[", 2 )
pt3 = sapply( strsplit( gsub("[.]","_",audioFiles[ff]) ,"_"),"[", 3 )
pt4 = sapply( strsplit( gsub("[.]","_",audioFiles[ff]) ,"_"),"[", 4 )
pt5 = sapply( strsplit( gsub("[.]","_",audioFiles[ff]) ,"_"),"[", 6 )
current_timestamp = sapply( strsplit( gsub("[.]","_",audioFiles[ff]) ,"_"),"[", 5 )
new_timestamp     = paste0("20", gsub("_","", gsub("Z","", gsub("T","_", current_timestamp) ) ) )
newName           = paste0(pt1,"_", pt2,"_", pt3,"_", pt4,"_", new_timestamp,".",pt5)

audioFiles[ff]
newName

#Change all files
#----------------------------------------------------------------------
OrigionalNames = NULL
for (ff in 1: length(audioFiles)){
  #extract parts of the file name
  pt1 = sapply( strsplit( gsub("[.]","_",audioFiles[ff]) ,"_"),"[", 1 )
  pt2 = sapply( strsplit( gsub("[.]","_",audioFiles[ff]) ,"_"),"[", 2 )
  pt3 = sapply( strsplit( gsub("[.]","_",audioFiles[ff]) ,"_"),"[", 3 )
  pt4 = sapply( strsplit( gsub("[.]","_",audioFiles[ff]) ,"_"),"[", 4 )
  pt5 = sapply( strsplit( gsub("[.]","_",audioFiles[ff]) ,"_"),"[", 6 )
  
  #rename format time-stamp
  current_timestamp = sapply( strsplit( gsub("[.]","_",audioFiles[ff]) ,"_"),"[", 5 )
  new_timestamp     = paste0("20", gsub("_","", gsub("Z","", gsub("T","_", current_timestamp) ) ) )
  
  #create new file name
  newName = paste0(pt1,"_", pt2,"_", pt3,"_", pt4,"_", new_timestamp,".",pt5)
  
  #save old and new file names
  OrigionalNames = rbind(OrigionalNames, c(ff, audioFiles[ff], newName))
  #rename the file
  file.rename ( audioFiles[ff], newName )
  
  rm(pt1,pt2,pt3,pt4,pt5,current_timestamp,new_timestamp,newName)
}

#save original names!
colnames( OrigionalNames) = c("file","origional","new")
write.csv(OrigionalNames,"RenamedFiles.csv")


#FOR SBO2_02 and SB02_03
#----------------------------------------------------------------------
#CHECK one file
#----------------------------------------------------------------------
ff = 1
pt1 = sapply( strsplit( gsub("[.]","_",audioFiles[ff]) ,"_"),"[", 1 )
pt2 = sapply( strsplit( gsub("[.]","_",audioFiles[ff]) ,"_"),"[", 2 )
pt3 = sapply( strsplit( gsub("[.]","_",audioFiles[ff]) ,"_"),"[", 3 )
pt4 = sapply( strsplit( gsub("[.]","_",audioFiles[ff]) ,"_"),"[", 4 )
pt5 = sapply( strsplit( gsub("[.]","_",audioFiles[ff]) ,"_"),"[", 6 )
current_timestamp = sapply( strsplit( gsub("[.]","_",audioFiles[ff]) ,"_"),"[", 5 )
new_timestamp =  gsub("_","", gsub("Z","", gsub("T","_", current_timestamp) ) )
newName = paste0(pt1,"_", pt2,"_", pt3,"_", pt4,"_", new_timestamp,".",pt5)

audioFiles[ff]
newName

#Change all files
#----------------------------------------------------------------------
OrigionalNames = NULL
for (ff in 1: length(audioFiles)){
  #extract parts of the file name
  pt1 = sapply( strsplit( gsub("[.]","_",audioFiles[ff]) ,"_"),"[", 1 )
  pt2 = sapply( strsplit( gsub("[.]","_",audioFiles[ff]) ,"_"),"[", 2 )
  pt3 = sapply( strsplit( gsub("[.]","_",audioFiles[ff]) ,"_"),"[", 3 )
  pt4 = sapply( strsplit( gsub("[.]","_",audioFiles[ff]) ,"_"),"[", 4 )
  pt5 = sapply( strsplit( gsub("[.]","_",audioFiles[ff]) ,"_"),"[", 6 )
  
  #rename format time-stamp
  current_timestamp  =  sapply( strsplit( gsub("[.]","_",audioFiles[ff]) ,"_"),"[", 5 )
  new_timestamp =  gsub("_","", gsub("Z","", gsub("T","_", current_timestamp) ) )
  
  #create new file name
  newName = paste0(pt1,"_", pt2,"_", pt3,"_", pt4,"_", new_timestamp,".",pt5)
  
  #save old and new file names
  OrigionalNames = rbind(OrigionalNames, c(ff, audioFiles[ff], newName))
  #rename the file
  file.rename ( audioFiles[ff], newName )
  
  rm(pt1,pt2,pt3,pt4,pt5,current_timestamp,new_timestamp,newName)
}

#save original names!
colnames( OrigionalNames) = c("file","origional","new")
write.csv(OrigionalNames,"RenamedFiles.csv")

#----------------------------------------------------------------------
#UNDO Re-NAMING... using "RenamedFiles.csv"
#----------------------------------------------------------------------
