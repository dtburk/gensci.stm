post_stem_custom_stopwords <- as.character(
    read.csv("C:/Users/Derek/Dropbox/0_TARGET/Derek's Analyses/topic_modeling_results_Oct_2016/post_stem_custom_stopwords.csv", 
             header=FALSE)[,1]
)

post_stem_custom_stopwords <- iconv(post_stem_custom_stopwords, to="ASCII//TRANSLIT")

write.csv(post_stem_custom_stopwords, file="data-raw/post_stem_custom_stopwords.csv", row.names=FALSE)
devtools::use_data(post_stem_custom_stopwords)