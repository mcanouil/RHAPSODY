#!/bin/bash

## input arguments
user=$1
ps=$2
study=$3
pheno=$4
pathToData=$5
timeInt=$6
Rscript="Rscript"

if [ "$#" -eq 7 ]
then
        Rscript=$7
else
        Rscript="Rscript"
fi



## tjeck the interaction arguments (ensure that the user use "Interaction", or else it will be "not_Interaction")
echo ""
if [[ $timeInt == "Interaction" ]]; then
    echo "You have chosen 'Interaction', beta_interaction/se_interaction/etc will be used for QC"
    timeInt="Interaction"
else
    echo "You have NOT chosen Interaction!, beta/se/etc will be used for QC"
    timeInt="not_Interaction"
fi
echo ""
sleep 2


## the output from SIB server needs to be placed in dirs
mkdir -p outEasyQC/


## loop over files from Mickaels pipeline and feed into setTheScene_byChr.R (takes some time)
find  $pathToData | grep $pheno"_"| grep $study | while read myFiles
do 
    $Rscript setTheScene_byChr.R $user $ps $study $pheno $myFiles $timeInt
done


## script that visualize the easyQC
$Rscript plotEasyQC.R $study $pheno $timeInt

## clean up the .rep files (the combined rep, rhaps.RData is saved in outEasyQC)
find ./ -maxdepth 1 -type f -name "${pheno}_${study}_chr**_RHAPSODYstep7_18*" -delete

