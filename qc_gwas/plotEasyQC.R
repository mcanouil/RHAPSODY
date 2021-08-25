## EasyQC makes a few .png files as well as a .ref file (with all the data). Here I make a few plots based on the .ref file


## input parameters: args <- c("ADDITION", "GLUC", "Interaction"), args <- c("ADDITION", "HBA1C", "Interaction"), 
## args <- c("Botnia", "GLUC", "Interaction")
args <- commandArgs(T)
study <- args[1]
pheno <- args[2]
timeInt <- args[3]

## the phenotype or study is not part of the filename (ex GLUC_ADDITION_chr22_RHAPSODYstep7_18.rep)
allFiles <- grep("rep",grep(pheno, list.files(pattern = "chr[0-9]*_RHAPSODYstep7_18"), val=T),val=T)

## ensure you only get the "interaction" file of the non interaction files 
if(timeInt=="Interaction"){
  allFiles <- grep(timeInt, allFiles, val=T)
} else {
  allFiles <- allFiles
}

## ensure that you have the correct number of inputFiles
Sys.sleep(1)
print(paste0("Number of files: ", length(allFiles)))
Sys.sleep(1)


## load the libraries
library(purrr)
library(data.table)
library(dplyr)
library(ggplot2)
library(magrittr)
library(tidyr)
library(scales)
library(ggrepel)


## NB: I match for "SE" and "median" so I get the correct variable whether it is interaction or not.
rhaps <- allFiles %>% 
  map(fread) %>% 
  reduce(rbind) %>%
  mutate_at(vars(matches("^SE.*median$")), funs(c_trait_transf_SE_median=c_trait_transf /.) ) %>%
  mutate(sqrt_N_max = sqrt(N_max)) %>%
  mutate(keep_frq = numSNPsOut/numSNPsIn)


if(timeInt=="Interaction"){
  fileName <- paste0("outEasyQC/",pheno,"_",study,"_Interaction_rhaps.RData")
} else {
  fileName <- paste0("outEasyQC/",pheno,"_",study,"_rhaps.RData")
}

save(rhaps, file = fileName)

##################
## plotting
#################

## c_trans median ??
p1 <- rhaps %>%
  ggplot(., aes(sqrt_N_max, c_trait_transf_SE_median, label=fileInShortName)) + geom_point() + 
  geom_abline(intercept = 0, slope = 1, lty=2) + 
  xlim(0,1000) + ylim(0,1000) +
  ggtitle(paste0(study, "_",timeInt)) +
  geom_text_repel()+
  theme(plot.title = element_text(hjust = 0.5))

ggsave(p1, filename = paste0("outEasyQC/",pheno,"_",study,"_",timeInt,"_","Nvsc_trait.pdf"))


## Allele frequency correlation per chromomsome
p2 <- rhaps %>%
  ggplot(., aes(fileInShortName, AFCHECK.cor_eaf.ref_EAF)) + geom_point() +
  ylim(0.75,1) + 
  ggtitle(paste0(study, "_",timeInt)) + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
  theme(plot.title = element_text(hjust = 0.5))

ggsave(p2,filename = paste0("outEasyQC/", pheno,"_",study,"_",timeInt,"_","AFCHECK.cor_eaf.ref_EAF_byChr.pdf"))


## Lambda.PVAL.GC per chromomsome
p3 <- rhaps %>%
    ggplot(., aes(fileInShortName, Lambda.PVAL.GC)) + geom_point() +
    ggtitle(paste0(study, "_",timeInt)) + 
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
    theme(plot.title = element_text(hjust = 0.5))

ggsave(p3, filename = paste0("outEasyQC/", pheno,"_",study,"_",timeInt,"_","Lambda.PVAL.GC_byChr.pdf"))


## number of SNPs that are kept after QC per chromosome
p4 <- rhaps %>%
  ggplot(., aes(fileInShortName, keep_frq)) + geom_point() +
  ylim(0.75,1) + 
  ggtitle(paste0(study, "_",timeInt)) + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
  theme(plot.title = element_text(hjust = 0.5))

ggsave(p4, filename = paste0("outEasyQC/", pheno,"_", study,"_",timeInt,"_","keepSNPSpct_byChr.pdf"))

## barplot. Start out by making the wide format into long
rhaps_long <- rhaps %>% 
  select(fileInShortName,numSNPsIn,numSNPsOut, NotInRef,AlleleMismatch, AlleleRefMissing, AFCHECK.numOutlier) %>% 
  gather(., key = "key", value="value",-fileInShortName)

p5 <- ggplot(rhaps_long, aes(fill=key, y=value, x=fileInShortName)) + 
  geom_bar(position="dodge", stat="identity") + 
  ggtitle(paste0(study, "_",timeInt)) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
  theme(plot.title = element_text(hjust = 0.5))

ggsave(p5, filename = paste0("outEasyQC/", pheno,"_", study,"_",timeInt,"_","rmSNPs_byChr.pdf"))


