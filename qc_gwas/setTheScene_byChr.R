
###################
## set the scene (script adapted from Iulians code)
##################


##################
## load lib
##################
library(opalr)
library(magrittr)
library(stringr)

##############################################
### Arguments for your own settings (username, ps, studyname, pheno, path to results_to_send)
##############################################

## read the arguments (args<-c("sparso","nyCykel17","ADDITION","GLUC","/home/opalroot/project/easyQC/data/ADDITION_14_GLUC_P.csv.gz", "noInteraction")
args <- commandArgs(T)

## add meaningfull names
username <- args[1]
pass <- args[2]
studyName <- args[3]
pheno<-args[4]
local.path <- args[5]
timeInt <- args[6]


## if HBA1C it also pulls out "1". I made a new one
##chr <- gsub("[^0-9]*([0-9]{1,2})[^0-9]*", "\\1", bsName)
bsName <- basename(local.path)
chr <- str_extract(pattern = "\\d{2}", string = bsName)

print("-- Files to be uploaded --")

bsName
chr
timeInt
Sys.sleep(2)

######################
## make the connection with SIB
######################
print("Connect to server")

remote.path <- paste0('/home/', username)
conn <- opal.login(username, pass, url = 'https://rhap-fdb01.vital-it.ch/meta')

###########
## store the opal session (usefull for killing session) [use the close_all_opal.R]
##########
dir.create("conn_list", showWarnings = FALSE)
saveRDS(
  object = conn, 
  file = file.path(
    "conn_list", 
    paste(gsub(".csv.gz$", "", basename(local.path), timeInt, "conn.rds"), sep = "_")
  )
)


######################
## make the infrastructure at SIB
######################
print("-- Make infrastructure at SIB server --")

raw <- paste0("dir.create(\"data/",studyName,"/raw","\", ","recursive=T)" )
ready <- paste0("dir.create(\"data/",studyName,"/readyEasyQC","\", ","recursive=T)" )
out <- paste0("dir.create(\"data/",studyName,"/outEasyQC","\", ","recursive=T)" )

## Create dir in workspace (raw, readyEasyQC, outEasyQC)
opal.execute(conn, raw)
opal.execute(conn, ready)
opal.execute(conn, out)

##define the variables at the SIB server (studyName and pheno)
evalMe1 <- paste0("studyName <- ", "\'",studyName,"\'")
opal.execute(conn, evalMe1)

## get the workspace at the SIB server
workspace <- opal.execute(conn, 'getwd()')
evalMe3 <- paste0("workspace <- ", "\'",workspace,"\'")
opal.execute(conn, evalMe3)

## define the chromsome at theSIB server
evalMe_chr <- paste0("chr <- ", "\'",chr,"\'")
opal.execute(conn, evalMe_chr)

## do we go for interaction or marginal
evalMe_timeInt <- paste0("timeInt <- ", "\'",timeInt,"\'")
opal.execute(conn, evalMe_timeInt)

############################
## cp the helper files (HRC, AlleleFreq, prepareInputEasy, templateRHAPSODY..ecf) to work dir
############################

opal.file_write(conn,
                source = paste0("/projects/meta/helperFiles_easyQC/HRC.r1-1.GRCh37.wgs.mac5.sites.tab.rsid_chr",chr,"_map.gz"),
                destination=paste0("HRC.r1-1.GRCh37.wgs.mac5.sites.tab.rsid_chr",chr,"_map.gz"))

opal.file_write(conn,
                source = "/projects/meta/helperFiles_easyQC/allelefreq.1000G_EUR_p1v3.impute_legends.noDup.noX.gz",
                destination="allelefreq.1000G_EUR_p1v3.impute_legends.noDup.noX.gz")
opal.file_write(conn,
                source = "/projects/meta/helperFiles_easyQC/prepareInputEasyQC.R",
                destination="prepareInputEasyQC.R")
opal.file_write(conn,
                source = "/projects/meta/helperFiles_easyQC/templateRHAPSODYstep7_18.ecf",
                destination="templateRHAPSODYstep7_18.ecf")

######################
## upload files SIB (to raw dir)
######################

print("-- Upload the files to server --")

## Upload your data to your home (at SIB server) and mv (opal.file_write) to workspace (data/ADDITION/raw)
opal.file_upload(conn,
                   source = local.path,
                   destination = paste0("/home/",username))
Sys.sleep(2)

opal.file_write(conn,
                source = paste0("/home/", username, "/", bsName),
                destination=paste0("data/",studyName,"/raw/",bsName))

###########################
## make the input files ready for easyQC and run easyQC
############################

## mk the input files ready (out is saved in readyEasyQC/). The script also modify the ecf file with correct path etc
print("-- Prepare the input files at SIB --")
opal.execute(conn, 'source("prepareInputEasyQC.R")')

print("-- Run EasyQC. This may take a while.... --")

## run the EasyQC (sometimes it RHAPSODYstep7_18_Interaction.ecf)
if(timeInt=="Interaction") {
  opal.execute(conn, 'EasyQC::EasyQC("RHAPSODYstep7_18_Interaction.ecf")', async = T)
} else {
  opal.execute(conn, 'EasyQC::EasyQC("RHAPSODYstep7_18.ecf")', async = T)
}
  

##########################
## tjeck the files in outEasyQC
########################

## make a string for tjekking the outEasyQC data
evalMe4 <- paste0("tmpFiles <- ", "\'","data/",studyName,"/outEasyQC","\'")
opal.execute(conn, evalMe4)
opal.execute(conn, 'list.files(tmpFiles)')


########################
### download to local computer and mv to shared
#################

## the *.rep file and png files[pngs are taken out], remade as ggplot from *.rep). *.CLEANED data is not
## downloaded but moved to shared dir on SIB-server

print("-- Download to local computer and mv results (CLEANED) to shared dir on server --")

## get path to all files in outEasyQC and specifically the rep file
path <- opal.execute(conn, 'list.files(tmpFiles, full.names=T)')

## The .rep file
path_rep <- grep(".rep", path, val=T)

## I need to rename".rep" file. other wise it is overwritten later (add the chromosome number).
newName <- gsub(path_rep, pattern='RHAPSODYstep7_18', replacement = paste0(pheno,"_",studyName,"_chr",chr, "_RHAPSODYstep7_18"))
bsnewName <- basename(newName)

evalMe5 <- paste0("newName <-","\'", newName,"\'" )
evalMe6 <- paste0("oldName <-","\'", path_rep, "\'")
opal.execute(conn, evalMe5)
opal.execute(conn, evalMe6)


## rename. and move .rep file to local computer (ei download the *.rep file)
opal.execute(conn, "file.rename(from=oldName, to=newName)")

opal.file_read(conn, source = newName, destination = paste0("/home/",username))
opal.file_download(conn, source = paste0("./home/", username,"/", bsnewName))



## mv CLEANED data to shared dir (ei (opal) read to home, (opal) mv to shared). IF interaction we need to rename
path_clean <- grep("CLEANED", path, val=T)
bspath_clean <- basename(path_clean)

## if "Interaction" we need to make a new name (add "Interaction") to the name
if(timeInt=="Interaction") {
  newName <- gsub(path_clean, pattern='.csv.gz', replacement = paste0("_Interaction.csv.gz"))
  bsnewName <- basename(newName)
  evalMe7 <- paste0("newName <-","\'", newName,"\'" )
  evalMe8 <- paste0("oldName <-","\'", path_clean, "\'")

  opal.execute(conn, evalMe7)
  opal.execute(conn, evalMe8)

  ## rename. and move to cleanedData
  opal.execute(conn, "file.rename(from=oldName, to=newName)")

  opal.file_read(conn, source = newName, destination = paste0("/home/",username))
  opal.file_mv(conn, source = paste0("./home/", username,"/", bsnewName), destination = "/projects/meta/cleanedData")
} else {
  
  ## mv CLEANED data to shared dir (ei (opal) read to home, (opal) mv to shared)
  opal.file_read(conn, source = path_clean, destination = paste0("/home/",username))
  opal.file_mv(conn, source = paste0("./home/", username,"/", bspath_clean), destination = "/projects/meta/cleanedData")
}




################
## clean up
#################

## Delete all files in my home:
delme <- opal.file_ls(conn, paste0("/home/",username))$path
tmp<-sapply(delme, function(x) { opal.file_rm(conn, x)})

#################
## logout
###################

print("-- logging out! --")
opal.logout(conn)

############
### the end :)
####################


