library(rgee); library(raster); library(terra)
ee_check() # For some reason, it's important to run this before initializing right now
rgee::ee_Initialize(user = 'crollinson@mortonarb.org', drive=T)
path.google <- "/Volumes/GoogleDrive/My Drive"
GoogleFolderSave <- "UHI_Analysis_Output_v30"
assetHome <- ee_get_assethome()


pathLSTJulAug = "users/crollinson/LST_JulAug_Clean/"

# Delete Image Collections etc.
ee_manage_assetlist(assetHome)


ee_manage_delete("users/crollinson/LST_JulAug_Clean/")
ee_manage_delete("users/crollinson/LST_JanFeb_Clean/")
ee_manage_delete("users/crollinson/ET_JulAug")
ee_manage_delete("users/crollinson/ET_JanFeb")
