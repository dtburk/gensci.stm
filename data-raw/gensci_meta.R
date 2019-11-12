library(data.table)
library(readxl)

gensci_meta <- data.table(
    read_xlsx(
      "data-raw/Topic_Modeling_Master_Sheet_Feb_10_2017.xlsx",
      .name_repair = "universal"
    )
)

write.csv(gensci_meta, file="data-raw/gensci_meta.csv", row.names=FALSE)
usethis::use_data(gensci_meta, overwrite = TRUE)
