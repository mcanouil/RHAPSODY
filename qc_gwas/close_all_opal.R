library(opalr)

for (iconn in list.files("conn_list", full.names = TRUE)) {
  opal.logout(readRDS(iconn))
}
