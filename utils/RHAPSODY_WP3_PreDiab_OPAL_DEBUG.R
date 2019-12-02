#---------------------------------------------------------------------------------------------------
# Name - RHAPSODY_WP3_PreDiab_OPAL_DEBUG
# Desc - Copy of R code from "RHAPSODY_WP3_PreDiab.Rmd"
# Author - Mickaël Canouil, Ph.D.
# Version - 1.2.22
#---------------------------------------------------------------------------------------------------
options(stringsAsFactors = FALSE)


###############
# Node settings
###############
opal_credentials <- as.data.frame(t(read.table(
  file = "opal_credentials.txt",
  stringsAsFactors = FALSE,
  row.names = c("opal_server", "opal_login", "opal_password")
)), stringsAsFactors = FALSE)


o <- opal::opal.login(
  username = opal_credentials$opal_login,
  password = opal_credentials$opal_password,
  url = opal_credentials$opal_server
)

available_tables <- intersect(opal::opal.datasources(opal = o)[[1]]$table, c("DM", "VS", "LB", "APMH", "CM", "MH"))
available_tables

for (itable in available_tables) {
  opal::opal.assign(opal = o, symbol = itable, value = paste0("rhapsody.", itable))
  assign(x = itable, value = opal::opal.execute(o, itable))
}
opal::opal.logout(o)
rm(list = c("o", "itable", "available_tables"))

ls()
