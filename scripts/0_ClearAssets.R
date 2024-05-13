library(rgee); library(raster); library(terra)
ee_check() # For some reason, it's important to run this before initializing right now
rgee::ee_Initialize(user = 'crollinson@mortonarb.org', drive=T)
path.google <- "/Volumes/GoogleDrive/My Drive"
GoogleFolderSave <- "UHI_Analysis_Output_v30"
assetHome <- ee_get_assethome()
ee_manage_assetlist(assetHome)


ee_manage_cancel_all_running_task()

# pathLSTJulAug = "users/crollinson/LST_JulAug_Clean/"

# Delete Image Collections etc.
ee_manage_assetlist(assetHome)

ee_manage_delete("users/crollinson/MERIT-DEM-v1_1km_Reproj")
ee_manage_delete("users/crollinson/MOD44b_1km_Reproj_Percent_NonTree_Vegetation")
ee_manage_delete("users/crollinson/MOD44b_1km_Reproj_Percent_NonVegetated")
ee_manage_delete("users/crollinson/MOD44b_1km_Reproj_VegMask")

ee_manage_delete("users/crollinson/MOD44b_1km_Reproj_Percent_Tree_Cover")

ee_manage_delete("users/crollinson/MERIT-DEM-v1_1km_Reproj_NH")
ee_manage_delete("users/crollinson/MERIT-DEM-v1_1km_Reproj_SH")
ee_manage_delete("users/crollinson/MOD44b_1km_Reproj_Percent_Tree_Cover_NH")
ee_manage_delete("users/crollinson/MOD44b_1km_Reproj_Percent_Tree_Cover_SH")
ee_manage_delete("users/crollinson/MOD44b_1km_Reproj_Percent_NonTree_Vegetation_NH")
ee_manage_delete("users/crollinson/MOD44b_1km_Reproj_Percent_NonTree_Vegetation_SH")
ee_manage_delete("users/crollinson/MOD44b_1km_Reproj_Percent_NonVegetated_NH")
ee_manage_delete("users/crollinson/MOD44b_1km_Reproj_Percent_NonVegetated_SH")
ee_manage_delete("users/crollinson/MOD44b_1km_Reproj_VegMask_NH")
ee_manage_delete("users/crollinson/MOD44b_1km_Reproj_VegMask_SH")


ee_manage_delete("users/crollinson/LST_JulAug_Clean/")
ee_manage_delete("users/crollinson/LST_JanFeb_Clean/")
ee_manage_delete("users/crollinson/ET_JulAug")
ee_manage_delete("users/crollinson/ET_JanFeb")


ee_manage_delete("users/crollinson/GLDAS_Annual_JulAug/")
ee_manage_delete("users/crollinson/GLDAS_Annual_JanFeb/")
