### Pipeline for running easyQC on results generated from Mickaels pipeline. GWAMA (meta analysis) is next step 

Note: Initially from https://gitlab.com/tspr/rhapsody_qc_gwas

##### easyQC
Use the following command to run the easyQC part:

> ./runMe.sh sparso XXX ADDITION GLUC /home/opalroot/project/easyQC/data/ Interaction \
> ./runMe.sh sparso XXX ADDITION GLUC /home/opalroot/project/easyQC/data/ not_Interaction \

> ./runMe.sh sparso XXX ADDITION GLUC2H /home/opalroot/project/easyQC/data/ Interaction \
> ./runMe.sh sparso XXX ADDITION GLUC2H /home/opalroot/project/easyQC/data/ not_Interaction \

> ./runMe.sh sparso XXX ADDITION HBA1C /home/opalroot/project/easyQC/data/  Interaction\
> ./runMe.sh sparso XXX ADDITION HBA1C /home/opalroot/project/easyQC/data/ not_Interaction


* argument 1 is username (for opal login, ex sparso)
* argument 2 is password (for opal login)
* argument 3 is name of Study (ex ADDITION, Botnia, MDC). Look into your input files to ensure that you write an identical match
* argument 4 is phenotype. (ex GLUC, GLUC2H, HBA1C)
* argument 5 is pathToData. This is the path to where the input data is (ei the data generated from Mickaels pipeline, results_to_send)
* argument 6 is interaction. If you write “Interaction” you will get QC of the time*SNP, if you write anything else you will get the marginal effect.


###### The follwing R packages are needed (R 3.6.0):
opalr, purrr, data.table, dplyr, ggplot2, magrittr, tidyr, scales, ggrepel


###### Output from runMe.sh: 
The output from runMe.sh can be found at the SIB server (CLEANED files) and in local computer (outEasyQC/*):

If you want to check whether you have generated CLEANED data at the SIB server you need to login and do ls:
> conn <- opal.login(username, password, url = 'https://rhap-fdb01.vital-it.ch/meta')
> opal.file_ls(conn, path = "/projects/meta/cleanedData/")

At your local computer there is a dir (outEasyQC) where you will find different QC measurement for each chromosome and phenotype


##### GWAMA - in prep
