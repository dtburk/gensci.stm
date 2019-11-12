countries <- scan(file="C:/Users/Derek/OneDrive/@Work/Kathrin Zippel/list_of_countries/countries.txt", 
                  what="character", sep="\n")

write.csv(countries, file="data-raw/countries.csv", row.names=FALSE)
devtools::use_data(countries)