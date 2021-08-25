## An R script that is placed in the shared directory of SIB server. The scripts prepares the input files
## from Mickeaels pipeline (rm a few col and rm [ACGT] from data). Second step is transfering (modifying) the 
## template ecf file by inserting correct path.


#################
## load the libraries
###############
library(data.table)
library(dplyr)
library(R.utils)


###########################
## define where raw data is (data from Mickaels pipeline). the output dir, and path to files
##########################

## studyName and pheno and chr has been defined earlier through a opal.execute command
pathToRaw <- paste0("data/",studyName, "/raw")
pathToOutput <- paste0("data/", studyName,"/readyEasyQC/")
filesToRead <- list.files(pathToRaw, full.names = T)
HRC_byChr <- paste0("HRC.r1-1.GRCh37.wgs.mac5.sites.tab.rsid_chr",chr,"_map.gz")


## if "Interaction" is specified we go for the beta_int, SE_int etc
if(timeInt=="Interaction") {
  myBeta <- "beta_interaction"
  mySE <- "SE_interaction"
  myPval <- "pval_interaction"
} else{
  myBeta <- "beta"
  mySE <- "SE"
  myPval <- "pval"
}

Sys.sleep(2)


##############################
## loop thrhough the file, modify and write to readyEasyQC/*
##############################
tmp<-lapply(filesToRead, function(x) {
  
  baseName <- gsub(".gz", "",basename(x))
  input <- fread(x)  
  
  mod_input <- input %>% 
    select(SNPID,chr, position,strand_genome, coded_all, noncoded_all, AF_coded_all,!!myBeta,!!mySE,!!myPval,n_total,imputation_quality_INFO) %>% 
    mutate_all(~gsub("[ATGC]:", "", .))   
  

  ## write the file to disk and gzip afterwards
  outputName <- paste0(pathToOutput,baseName)
  write.table(mod_input,file=outputName,sep = "\t",qu=F, col=T, row=F)
  gzip(outputName,destname=paste0(outputName,'.gz'), overwrite=T)
  print(paste0("-- ",baseName," --"))
}
)



##########################################
## make the ecf file (ie modify the template with correct paths). Plus include Interaction term
###########################################
Sys.sleep(2)
print("Make the *.ecf file")

correctFiles <- list.files(paste0("data/",studyName, "/readyEasyQC/"), recursive = T, include.dirs = F, full.names = T)
insertMe <- sapply(correctFiles, function(x) paste0("EASYIN --fileIn ",workspace,"/", x))
insertMe <- paste(insertMe, collapse = "\n")

insertMe2<-paste("DEFINE --pathOut",paste0(workspace,"/data/",studyName,"/outEasyQC"))
pathToHRC_byChr <- paste0(workspace,"/", HRC_byChr)
pathTo1000GAllele <- paste0(workspace,"/", "allelefreq.1000G_EUR_p1v3.impute_legends.noDup.noX.gz")

tmp  <- readLines("templateRHAPSODYstep7_18.ecf")
tmp2  <- gsub(tmp, pattern = "xxxx 1. INSERTME xxxx", replace = insertMe2)
tmp3  <- gsub(tmp2, pattern = "xxxx 2. INSERTME xxxx", replace = insertMe)
tmp4  <- gsub(tmp3, pattern = "xxxx 3. INSERTME xxxx", replace = pathToHRC_byChr)
tmp5  <- gsub(tmp4, pattern = "xxxx 4. INSERTME xxxx", replace = pathTo1000GAllele)

## insert the interaction (if interaction)
if(timeInt=="Interaction") {
  tmp6  <- gsub(tmp5, pattern = "myBeta", replace = "beta_interaction")
  tmp7  <- gsub(tmp6, pattern = "mySE", replace = "SE_interaction")
  tmp8  <- gsub(tmp7, pattern = "myPval", replace = "pval_interaction")
  fileName="RHAPSODYstep7_18_Interaction.ecf"
} else {
  tmp6  <- gsub(tmp5, pattern = "myBeta", replace = "beta")
  tmp7  <- gsub(tmp6, pattern = "mySE", replace = "SE")
  tmp8  <- gsub(tmp7, pattern = "myPval", replace = "pval")
  fileName="RHAPSODYstep7_18.ecf"
}


writeLines(tmp8, fileName)


#################
## make R file to include the EASYQC commands
################
## Not sure exactly how to do this: read the slack
#theLib <- .libPaths()
#writeLines(c('.libPaths(new=theLib)','library(EasyQC)','EasyQC::EasyQC("RHAPSODYstep7_18.ecf")'), "runMe.R")



