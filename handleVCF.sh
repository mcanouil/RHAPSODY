#!/bin/sh

## Description:
## When analysing the SNPs in R (lme4) the scripts crashes due to the amount of SNPs.
## We need to break down the data into smaller pieces.
## In this script we take one vcf file as input (ex chr1).
## Filter out bad SNPs and extact dosage, impute and frequency.
## Author: Thomas Sparsø


## the input arguments are a vcf file (must be gzipped, only one chr)
projectName=$1
vcfout=$2
imputationQualityTag=$3
vcftoolsPath=$4


echo -e "\n"
echo "Name of Project: $projectName"
echo "Name of imputation quality tag (INFO/*): $imputationQualityTag"



# to format only on chromosome 22 vcfs: $vcfin/22*.vcf.gz
# find $vcfin/*.vcf.gz | while read vcf
cat $vcfout/vcffilespath.txt | while read vcf
do

  base=`basename $vcf`
  echo "name of inputFile: $base"

  ## directory for extraction from vcf (dosage, freq, etc). and for the small vcf files
  chrDir=`basename $vcf .vcf.gz`
  eVCF=$vcfout/$chrDir/extractVCF
  sVCF=$vcfout/$chrDir/smallVCF
  mkdir -p $eVCF
  mkdir -p $sVCF


  ## clean up before we start
  rm -f $eVCF/*
  rm -f $sVCF/*


  ############# step 1: Delete the obviuos problematic SNPs, indels, and rare SNPs
  echo -e "\n"
  echo "Reduce bad SNPs in $base ... (takes a while if many SNPs)"
  $vcftoolsPath/vcftools --gzvcf $vcf \
    --get-INFO $imputationQualityTag \
    --out $eVCF/tmp
  awk '{if($5<0.3) print $1"\t"$2}' $eVCF/tmp.INFO > $eVCF/delme
  $vcftoolsPath/vcftools --gzvcf $vcf \
    --exclude-positions $eVCF/delme \
    --maf 0.01 \
    --remove-indels \
    --remove-filtered-all \
    --recode-INFO-all \
    --recode \
    --stdout | gzip -c > $eVCF/filtered.$base


  ############ step 2: Break down the vcf file into smaller files.
  ############ Make a header and attached the header to all Files
  gunzip -c $eVCF/filtered.$base | head -n 200 | awk '/^#/' > $sVCF/tmpHeader

  ## unzip vcf and keep all but header. Split the files into lines of 1000 and call them pre*
  gunzip -c $eVCF/filtered.$base | awk '!/^#/' |  split - -l1000 $sVCF/pre

  ## loop through the pre*, add the header and zip the file. Save as "small_*.vcf.gz"
  find $sVCF/pre* | while read pathToFile;
  do
    file=`basename $pathToFile`
    cat $sVCF/tmpHeader $sVCF/$file | gzip -c > $sVCF/small_${file}.vcf.gz
  done


  ########### step 3: is DS available?
  ###maybe we should check if dosage is available for all cohorts before we make a script here.



  ########### step 4: Extract dosage, impute, freq from each od the small VCFs
  echo -e "\n"
  echo extract dosage, R2, freq...

  find $sVCF/small* | while read pathToFile
  do
    file=`basename $pathToFile .vcf.gz`

    $vcftoolsPath/vcftools --gzvcf $pathToFile \
      --extract-FORMAT-info DS \
      --stdout | gzip -c > $eVCF/$file.DS.gz
      
    $vcftoolsPath/vcftools --gzvcf $pathToFile \
      --get-INFO $imputationQualityTag \
      --out $eVCF/$file
      
    $vcftoolsPath/vcftools --gzvcf $pathToFile \
      --hardy \
      --out $eVCF/$file
      
    # $vcftoolsPath/vcftools --gzvcf $pathToFile \
    #   --missing-site \
    #   --out $eVCF/$file
      
    $vcftoolsPath/vcftools --gzvcf $pathToFile \
      --freq \
      --out $eVCF/$file
    sed -i s/"{ALLELE:FREQ}"/frq1"\t"frq2/ $eVCF/$file.frq
  done


  ########## step 5: clean up
  rm -f $eVCF/filtered.$base
  rm -f $sVCF/tmpHeader
  rm -f $sVCF/pre*
  rm -f $eVCF/delme
  rm -f $eVCF/tmp.INFO

done
