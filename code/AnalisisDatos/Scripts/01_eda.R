library(readxl)
library(tidyverse)

dataset <- read_excel("Data/dataset-v3.0.xls")
dataset <- tbl_df(dataset)
dataset

dataset %>% 
  group_by(P9) %>% 
  count()

dataset <- subset(dataset, !is.na(P3))
dataset <- subset(dataset, !is.na(P11))
dataset <- subset(dataset, !is.na(P14))

dataset <- subset(dataset, P9 != 'De acuerdo;Indiferente')
dataset <- subset(dataset, P9 != 'Indiferente;En desacuerdo')

dataset %>% 
  group_by(P9) %>% 
  count()

dataset %>% 
  group_by(P3) %>% 
  count()

niveles <- c('Totalmente de acuerdo', 'De acuerdo', 'Indiferente', 'En desacuerdo', 'Totalmente en desacuerdo')

dataset <-  dataset %>% 
  mutate(
    P1 = parse_factor(P1, levels = niveles),
    P2 = parse_factor(P2, levels = niveles),
    P3 = parse_factor(P3, levels = niveles),
    P4 = parse_factor(P4, levels = niveles),
    P5 = parse_factor(P5, levels = niveles),
    P6 = parse_factor(P6, levels = niveles),
    P7 = parse_factor(P7, levels = niveles),
    P8 = parse_factor(P8, levels = niveles),
    P9 = parse_factor(P9, levels = niveles),
    P10 = parse_factor(P10, levels = niveles),
    P11 = parse_factor(P11, levels = niveles),
    P13 = parse_factor(P13, levels = niveles),
    P14 = parse_factor(P14, levels = c('Muy frecuentemente', 'Frecuentemente', 'Ocasionalmente', 'Raramente'),
    P15 = parse_factor(P15, levels = niveles)
  )
)

summary(dataset)

library(likert)

diccionario <- read_excel("Data/diccionario.xlsx")
diccionario <- tbl_df(diccionario)

df_likert <- as.data.frame(dataset)
glimpse(df_likert)

colnames( df_likert ) <- diccionario$descripcion

bloqueVI <- 1:6
bloqueVD <- 7:13
bloqueVD2 <- 15

glimpse(dataset[ , bloqueVI ])
glimpse(dataset[ , bloqueVD ])
glimpse(dataset[ , bloqueVD2 ])

summary(dataset[ , bloqueVI ])
summary(dataset[ , bloqueVD ])
summary(dataset[ , bloqueVD2 ])

itemsVI <- likert( items = df_likert[ , bloqueVI ] )
itemsVD <- likert( items = df_likert[ , bloqueVD ] )

data <- as.data.frame(df_likert[ , bloqueVD2 ])
colnames( data ) <- diccionario$descripcion[[15]]
itemsVD2 <- likert( items = data )

plot(itemsVD2)

plot( itemsVI, centered = TRUE, group.order = colnames( itemsVI$items ),
      legend.position = "right" ) +
  theme( axis.text.x = element_text( size = 10 ),
         axis.text.y = element_text( size = 13, hjust = 0 ),
         legend.text = element_text( size = 10 ),
         legend.title = element_text( size = 10 ) )  

plot( itemsVI, type = "density" )

plot( itemsVD, centered = TRUE, group.order = colnames( itemsVD$items ),
      legend.position = "right" ) +
  theme( axis.text.x = element_text( size = 10 ),
         axis.text.y = element_text( size = 13, hjust = 0 ),
         legend.text = element_text( size = 10 ),
         legend.title = element_text( size = 10 ) )  

plot( itemsVD, type = "heat", group.order = colnames( itemsVD$items ) ) +
  theme( axis.text.x = element_text( size = 8 ),
         axis.text.y = element_text( size = 13, hjust = 0 ),
         legend.text = element_text( size = 10 ),
         legend.title = element_text( size = 10 ) )


plot( itemsVI, type = "heat", group.order = colnames( itemsVI$items ) ) +
  theme( axis.text.x = element_text( size = 8 ),
         axis.text.y = element_text( size = 13, hjust = 0 ),
         legend.text = element_text( size = 10 ),
         legend.title = element_text( size = 10 ) )

library( corrr )
library( corrplot )

glimpse(dataset[ , 1:13 ])

dfCor <- dataset[ , 1:13 ]
dfCor <- lapply(dfCor, as.numeric)
dfCor <- as.data.frame( dfCor )
corr <- cor( dfCor )

library(knitr)

kable( corr,
       caption = "Tabla de correlaciones (solo se muestran las ocho primeras)" )

corr2 <- correlate( dfCor )
kable( fashion( corr2 ),
       caption = "Tabla de correlaciones (solo se muestran las ocho primeras)" )

network_plot( corr2, min_cor = 0.4 )

corrplot.mixed( corr, tl.pos = "lt", diag = 'n', upper = "ellipse",
                number.cex = 0.4, tl.cex = 0.8,
                order = "hclust" )


library(tm)
library(SnowballC)
library(wordcloud)
library(RColorBrewer)
library(RCurl)
library(XML)
library(rsconnect)


palabras <- read.delim("Data/palabras.txt")


# wordcloud function
rquery.wordcloud <- function(x, type=c("text", "url", "file"), 
                             lang="english", excludeWords=NULL, 
                             textStemming=FALSE, colorPalette="Dark2",
                             min.freq=3, max.words=200)
{ 
  library("tm")
  library("SnowballC")
  library("wordcloud")
  library("RColorBrewer") 
  
  if(type[1]=="file") text <- readLines(x)
  else if(type[1]=="url") text <- html_to_text(x)
  else if(type[1]=="text") text <- x
  
  # Load the text as a corpus
  docs <- Corpus(VectorSource(text))
  # Convert the text to lower case
  docs <- tm_map(docs, content_transformer(tolower))
  # Remove numbers
  docs <- tm_map(docs, removeNumbers)
  # Remove stopwords for the language 
  docs <- tm_map(docs, removeWords, stopwords(lang))
  # Remove punctuations
  docs <- tm_map(docs, removePunctuation)
  # Eliminate extra white spaces
  docs <- tm_map(docs, stripWhitespace)
  # Remove your own stopwords
  if(!is.null(excludeWords)) 
    docs <- tm_map(docs, removeWords, excludeWords) 
  # Text stemming
  if(textStemming) docs <- tm_map(docs, stemDocument)
  # Create term-document matrix
  tdm <- TermDocumentMatrix(docs)
  m <- as.matrix(tdm)
  v <- sort(rowSums(m),decreasing=TRUE)
  d <- data.frame(word = names(v),freq=v)
  # check the color palette name 
  if(!colorPalette %in% rownames(brewer.pal.info)) colors = colorPalette
  else colors = brewer.pal(8, colorPalette) 
  # Plot the word cloud
  set.seed(1234)
  wordcloud(d$word,d$freq, min.freq=min.freq, max.words=max.words,
            random.order=FALSE, rot.per=0.35, 
            use.r.layout=FALSE, colors=colors)
  
  invisible(list(tdm=tdm, freqTable = d))
}
# html_to_text function
html_to_text<-function(url){
  library(RCurl)
  library(XML)
  # download html
  html.doc <- getURL(url)  
  #convert to plain text
  doc = htmlParse(html.doc, asText=TRUE)
  # "//text()" returns all text outside of HTML tags.
  # We also don’t want text such as style and script codes
  text <- xpathSApply(doc, "//text()[not(ancestor::script)][not(ancestor::style)][not(ancestor::noscript)][not(ancestor::form)]", xmlValue)
  # Format text vector into one character string
  return(paste(text, collapse = " "))
}

# desde vector de texto
detexto <- c("./Data/palabras.txt")
## ejemplo de "wordcloud" de página de internet
res <- rquery.wordcloud(detexto, type ="file",
                        lang = "spanish",
                        excludeWords = NULL,
                        textStemming = FALSE,
                        colorPalette = "Dark2",
                        min.freq = 3,
                        max.words = 400)
                              