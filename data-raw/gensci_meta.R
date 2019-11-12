library(data.table)
library(xlsx)

gensci_meta <- data.table(
    read.xlsx("C:/Users/Derek/Dropbox/0_TARGET/Topic_Modeling_Master_Sheet_Feb_10_2017.xlsx", 1)
)

write.csv(gensci_meta, file="data-raw/gensci_meta.csv", row.names=FALSE)
devtools::use_data(gensci_meta)