
loadVocab <- function(worksheet_name=NA, TermsToAdd=NA) {
  ### Load data from Google Sheets
  # require(googlesheets)
  # df.vocab <- gs_title("Fall 2015 Notes")%>% gs_read_csv(ws = worksheet_name)
  df <- read.csv("~/Github/utexas/Classes/Gov_ex2_vocab.csv")
  
  vocab <- as.character(df$vocab_word)
  vocab <- gsub("[^A-z]", " ", vocab)
  vocab <- tolower(vocab)
  vocab <- gsub("  ", " ", vocab)
  
  df$vocab_full <- vocab
  
  vocab <- gsub(" v ", " ", vocab)
  vocab <- gsub(" of ", " ", vocab)
  vocab <- gsub(" the ", " ", vocab)
  vocab <- gsub(" in ", " ", vocab)
  
  df$vocab_search <- vocab
  return(df)
}

findMatchingTerms <- function(vocab_list, notes_corpus, TermsToAdd=NA) {
  require(stringi)
  require(tm)
  VOCAB  <- vocab_list
  CORPUS <- notes_corpus
  
    
  vocab_words <- stri_split(VOCAB, regex=" ") %>% unlist() %>% unique()
  
  m0 <- DocumentTermMatrix(CORPUS, control = list(
    dictionary = vocab_words)) %>% as.matrix()
    
  not_found <- colSums(m0)[colSums(m0)<1]
  message("Couldn't find:")
  message(paste(names(not_found), " "))
  
  found <<- colSums(m0)[colSums(m0)>0] # global save
  
  words_to_match <- c(names(found),    # These are vocab words
                      TermsToAdd)      # optional vector of words to append to list
  
  # DTM of words we want to look up
  found.dtm <- DocumentTermMatrix(CORPUS, control = list(
    dictionary = words_to_match))   
  
  
  return(found.dtm)
}

# as.data.frame(list(term = df.vocab$vocab_full,
#      colsplit(vocab, " ", c("w1", "w2", "w3", "w4", "w5")))) %>%
#   melt(id.vars = "term") %>% select(term, search_term=value) %>%
#   filter(search_term!="") %>% unique()
