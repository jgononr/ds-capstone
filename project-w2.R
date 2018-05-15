
# Leer todos los documentos, analizar por cada uno
# Análisis como un todo
library(tm)
library(RWeka)
library(parallel)
library(foreach)
library(doParallel)

set.seed(12345)

basepath <- "final/en_US/"
df <- data.frame(row.names = c("blogs", "news", "twitter"),
                     size = c(0,0,0), nlines = c(0,0,0))
df$content <- I(list(c("1","2")))

for (file in c("blogs", "news", "twitter")) {
    filepath <- paste0(basepath, "en_US.", file, ".txt")
    fd <- file(filepath, "r")
    lines <- readLines(fd, encoding = "UTF-8", skipNul = TRUE)
    l <- length(lines)
    lines <- lines[sample(1:l, round(l * .1), replace = FALSE)]
    df[file, "size"] <- round((file.info(filepath)$size)/(1024^2))
    df[file, "nlines"] <-  l
    df[file, ]$content <- I(list(lines))
    close(fd)
}
rm(l, lines, file, filepath, basepath, fd)


cl <- makeCluster(max(1, (detectCores(logical = TRUE) - 1)), type = "FORK")
registerDoParallel(cl)

#clusterSetRNGStream(12345)

#Sampling de las lineas de cada fichero (10% de las lineas)


corpus <- Corpus(DirSource("final/en_US/", pattern = "twitter.txt$"),
                 readerControl = list(reader = readPlain,
                                      language = "en_US",
                                      load = TRUE))

corpus <- tm_map(tm_map(corpus, removeNumbers),
                 stripWhitespace)

# Derivar del Corpus el TermDocMatrix

tok <- function(x) NGramTokenizer(x, Weka_control(min = 2, max = 10))

tdm.control <- list(tokenize = tok,
                    language = "en_US",
                    removeNumbers = TRUE,
                    removePunctuation = TRUE)
tdm <- TermDocumentMatrix(corpus, control = tdm.control)


stopCluster(cl)
