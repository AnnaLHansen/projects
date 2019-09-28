library(tm)
textfolder = "./Data/"

(docs <- Corpus(DirSource(textfolder, pattern="textDocs*"), readerControl = list(language="en")))

inspect(docs)

docs_nopunct <- tm_map(docs, removePunctuation)
dtm <- DocumentTermMatrix(docs_nopunct)

inspect(dtm)


