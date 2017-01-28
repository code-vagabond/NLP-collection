textimager <- function(doclines, parameters, json_pretty, session_object)
{
  library(jsonlite);

  # Zusammenfassung und Stylo braucht keine Filter
  if (parameters$method == "summary")
  {
    return(ti_summary(doclines, parameters, json_pretty));
  }  
  if (parameters$method == "stylo")
  {
    return(ti_stylo(doclines, parameters, json_pretty));
  }
  # STM hat eigene Filter Funktion, sonst mit TM
  if (parameters$method == "stm")
  {
    return(ti_stm(doclines, parameters, json_pretty, session_object));
  }


  if (parameters$method == "igraph")
  {
    return(ti_igraph(doclines, parameters, json_pretty));
  }
  
  #use tm for stemming, then use lda package
  library(tm);
  library(lda);

  # Falls kein Session Objekt übergeben wird Daten vorbereiten
  docs_data <- FALSE;
  if (length(session_object) == 0)
  {
    docs_data <- ti_preapare_docs(doclines, parameters);
  }
  
  # Passendes Verfahren wählen
  if (parameters$method == "lda")
  {
    return(ti_lda(docs_data, parameters, json_pretty, session_object));
  }
  else if (parameters$method == "slda")
  {
    if (parameters$slda$method == "train")
    {
      return(ti_slda_train(docs_data, parameters, json_pretty));
    }
    else if (parameters$slda$method == "predict")
    {
      return(ti_slda_predict(docs_data, parameters, json_pretty));
    }
  }
  
  # Als Standard eine Fehlermeldung senden
  return(toJSON(list(status="error", message="Kein/Unbekanntes Verfahren gewählt")));
}

# Filtert Dokumente usw.
ti_preapare_docs <- function(doclines, parameters)
{
  docs_data <- Corpus(VectorSource(doclines));

  if (parameters$lower)
    docs_data <- tm_map(docs_data, content_transformer(tolower));

  if (parameters$remove_stops)
    {
      my_stopwords <- stopwords(parameters$lang);
      
      if (!parameters$lower)
      {
        # Wenn nicht Kleinbuchstaben Stopwordsliste anpassen
        stopws_upper <- toupper(my_stopwords);
        stopws_flup <- sapply(my_stopwords, function(s) paste0(toupper(substring(s, 1, 1)), substring(s, 2)), USE.NAMES=FALSE);
      
        my_stopwords <- c(my_stopwords, stopws_upper, stopws_flup);
      }

      docs_data <- tm_map(docs_data, removeWords, my_stopwords);
  }

  if (parameters$remove_numbers)
    docs_data <- tm_map(docs_data, removeNumbers);

  if (parameters$remove_punctuation)
    docs_data <- tm_map(docs_data, removePunctuation);

  if (parameters$min_word_length > 0)
    docs_data <- tm_map(docs_data, content_transformer(ti_filter_wordlength_fun), parameters$min_word_length);
  
  docs_data <- tm_map(docs_data, stripWhitespace);
  docs_data <- tm_map(docs_data, stripWhitespace);

  if (parameters$stem)
    docs_data <- tm_map(docs_data, stemDocument, language=parameters$lang);

  docs_data <- unlist(sapply(docs_data, `[`, "content"));

  docs_data <- lexicalize(docs_data, sep=parameters$sep, lower=parameters$lower);

  return(docs_data);
}

# Wortlänge filtern
ti_filter_wordlength_fun <- function(s, word_length)
{
  ss <- unlist(strsplit(s, split=" "));
  r <- paste(ss[nchar(ss)>=word_length], collapse=" ");
  return(r);
}
