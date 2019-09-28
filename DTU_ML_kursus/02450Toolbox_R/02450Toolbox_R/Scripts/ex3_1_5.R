library(tm)
textfolder = "./Data/"

(docs <- Corpus(DirSource(textfolder, pattern="textDocs*"), readerControl = list(language="en")))

inspect(docs)

docs_nopunct <- tm_map(docs, removePunctuation)
dtm <- DocumentTermMatrix(docs_nopunct)

mystopwords=scan("./Data/stopWords.txt", character(0))

docs_nostopwords <- tm_map(docs_nopunct, removeWords, mystopwords)
dtm_nostopwords <- DocumentTermMatrix(docs_nostopwords, control=list(stopwords=TRUE))
inspect(dtm_nostopwords)

docs_stemmed <- tm_map(docs_nostopwords, stemDocument, language="english")
inspect(docs_stemmed)
dtm_stemmed <- DocumentTermMatrix(docs_stemmed, control=list(stopwords=TRUE))
inspect(dtm_stemmed)


library(sos)
#findFn("cosine", maxPages=2, sortby="MaxScore") # to check whether a certain function exists in some R package, and if so, find out which package.
library(lsa) # the above function shows that a function for calculating the cosine measure exists in the package lsa.

size <- dim(dtm_stemmed)
cosmeas <- c()
q <- c(0,0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 1, 1, 0, 0, 0)
dtmat = matrix(dtm_stemmed,dtm_stemmed$nrow,dtm_stemmed$ncol)

for(irow in 1:size[1])
{
doc <- dtm_stemmed[irow,]
cosmeas[irow] <- cosine(dtmat[irow,],q)
#cosmeas[irow] <- cosine(inspect(dtm_stemmed)[irow,],q)
}
print("Cosine similarity from q to docs: ")
print(cosmeas)
