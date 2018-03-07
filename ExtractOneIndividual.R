library(opal)
library(tidyverse)

params <- list(
    opal_server = 'http://localhost:8080', 
    opal_login = 'administrator', 
    opal_password = 'password'
)

o <- opal.login(
    username = params$opal_login,
    password = params$opal_password,
    url = params$opal_server
)

availableTable <- opal.datasources(opal = o)[[1]]$table

# Available tables in OPAL (Check RHAPSODY/CDISC documentation for details)
whichTable <- c("DM", "VS", "LB", "MH", "CM")

for (itable in whichTable) {
    opal.assign(opal = o, symbol = itable, value = paste0('rhapsody.', itable))
    assign(x = itable, value = opal.execute(o, itable))
}

opal.logout(o)
rm(list = c("o", "itable"))


FirstIndividual <- DM$SUBJID[1]
data_OneIndividual <- lapply(whichTable, function (iTable) {
    get(iTable) %>% 
        filter(SUBJID==FirstIndividual) %>% 
        mutate(SUBJID = "1")
}) %>% 
    `names<-`(whichTable)
saveRDS(data_OneIndividual, file = "data_OneIndividual.rds")
