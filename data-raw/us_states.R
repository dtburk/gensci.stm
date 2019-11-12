us_states <- scan(file="C:/Users/Derek/OneDrive/@Work/Kathrin Zippel/list_of_states/states.txt", 
                  what="character", sep="\n")

write.csv(us_states, file="data-raw/us_states.csv", row.names=FALSE)
devtools::use_data(us_states)