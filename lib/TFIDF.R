library(text2vec)
Create_Coauthor<-function(Filename=AKumar){
  ##Get DTM for Coauthor:
  CoAu <- itoken(Filename$Coauthor,
                 preprocessor = tolower,
                 tokenizer = word_tokenizer,
                 ids = Filename$PaperID,
                 # turn off progressbar because it won't look nice in rmd
                 progressbar = FALSE)
  
  vo_CoAu <- create_vocabulary(CoAu, stopwords = 
                                 c("a", "an", "the", "in", "on", "at", "of", "about", "for"))
  vector_CoAu <- vocab_vectorizer(vo_CoAu)
  dtm_CoAu <- create_dtm(CoAu , vector_CoAu)
  
  ##Get tfidf matrixes 
  tfidf <- TfIdf$new()
  dtm_CoAu_tfidf <- fit_transform(dtm_CoAu, tfidf)
  
  return(as.matrix(dtm_CoAu_tfidf))

}

Create_Title<-function(Filename = AKumar){
  ##Get DTM for Title:
  Pap <- itoken(Filename$Paper,
                preprocessor = tolower,
                tokenizer = word_tokenizer,
                ids = Filename$PaperID,
                # turn off progressbar because it won't look nice in rmd
                progressbar = FALSE)
  
  vo_Pap<- create_vocabulary(Pap, stopwords = 
                               c("a", "an", "the", "in", "on", "at", "of", "about", "for"))
  
  vector_Pap <- vocab_vectorizer(vo_Pap)
  dtm_Pap <- create_dtm(Pap,vector_Pap)
  
  ##Get tfidf matrixes 
  tfidf <- TfIdf$new()
  dtm_Pap_tfidf<-fit_transform(dtm_Pap, tfidf)
  
  return(as.matrix(dtm_Pap_tfidf))
}

Create_Journal<-function(Filename = AKumar){
  ##Get DTM for Journal:
  Jour <- itoken(as.character(Filename$Journal),
                 preprocessor = tolower,
                 tokenizer = word_tokenizer,
                 ids = Filename$PaperID,
                 # turn off progressbar because it won't look nice in rmd
                 progressbar = FALSE)
  vo_Jour<- create_vocabulary(Jour, stopwords = 
                                c("a", "an", "the", "in", "on", "at", "of", "about", "for"))
  
  vector_Jour <- vocab_vectorizer(vo_Jour)
  dtm_Jour <- create_dtm(Jour,vector_Jour)
  
  ##Get tfidf matrixes 
  tfidf <- TfIdf$new()
  dtm_Jour_tfidf<-fit_transform(dtm_Jour, tfidf)
  
  return(as.matrix(dtm_Jour_tfidf))
}




