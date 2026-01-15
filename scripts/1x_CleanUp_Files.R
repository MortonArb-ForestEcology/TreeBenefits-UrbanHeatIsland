# Script to clean up file saving issues from Earth Engine being annoyign
path.google <- file.path("~/Google Drive/My Drive")
GoogleFolderSave <- "UHI_Analysis_Output_Final_v5"


foldersSaveDupes <- dir(path.google, GoogleFolderSave)
length(foldersSaveDupes)-1

pb <- txtProgressBar(min=2, max=length(foldersSaveDupes), style=3)
for(i in 2:length(foldersSaveDupes)){
  setTxtProgressBar(pb, i)
  fileMove <- dir(file.path(path.google, foldersSaveDupes[i]))
  file.copy(from=file.path(path.google, foldersSaveDupes[i], fileMove),
            to=file.path(path.google, foldersSaveDupes[1], fileMove))
  
  # # checking to make sure it copied
  # dir(file.path(path.google, foldersSaveDupes[1]),fMove)
  file.remove(file.path(path.google, foldersSaveDupes[i], fileMove), showWarnings=F)
  file.remove(file.path(path.google, foldersSaveDupes[i]), showWarnings=F)
  
}
# length(dir(file.path(path.google, GoogleFolderSave)))
length(dir(file.path(path.google, GoogleFolderSave), "ERA5"))
