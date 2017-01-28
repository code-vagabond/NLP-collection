ti_stm <- function(doclines, parameters, json_pretty, session_object)
{
  if (parameters$method != "stm")
    return(toJSON(list(status="error", message="Falsches Verfahren gewählt, dieser Fehler sollte nicht auftreten.")));
  
  library(stm);

  # Prüfen ob bereits ein Session Objekt vorhanden ist
  if (length(session_object) == 0)
  {
    # Nein, also Model berechnen
    docs_data <- textProcessor(doclines, lowercase=parameters$lower, removestopwords=parameters$remove_stops, removenumbers=parameters$remove_numbers, removepunctuation=parameters$remove_punctuation, stem=parameters$stem, wordLengths=c(parameters$min_word_length, Inf), language=parameters$lang, verbose=FALSE, sparselevel=parameters$stm$sparselevel);

    # Parameter sind Strings damit Inf und NULL angegeben werden kann, deshalb hier erst umwandeln
    if (parameters$stm$subsample == "NULL")
    {
      parameters$stm$subsample <- NULL;
    }
    else
    {
      parameters$stm$subsample = as.numeric(parameters$stm$subsample);
    }
    
    parameters$stm$lower_thresh = as.numeric(parameters$stm$lower_thresh);
    parameters$stm$upper_thresh = as.numeric(parameters$stm$upper_thresh);

    prep <- prepDocuments(docs_data$documents, docs_data$vocab, docs_data$meta, subsample=parameters$stm$subsample, lower.thresh=parameters$stm$lower_thresh, upper.thresh=parameters$stm$upper_thresh);
    
    control <- list(alpha=parameters$alpha, eta=parameters$eta);

    model <- stm(prep$documents, prep$vocab, parameters$k, data=prep$meta, control=control, init.type=parameters$stm$init_type, max.em.its=parameters$stm$max_em_its, emtol=as.numeric(parameters$stm$emtol), LDAbeta=parameters$stm$ldabeta, interactions=parameters$stm$interactions, ngroups=parameters$stm$ngroups);

    # Nur Model zurückgeben, geplottet wird im zweiten Aufruf
    return(model);
  }
  else
  {      
    # Ja, Model muss nicht nochmal berechnet werden
    model <- session_object;

    if (parameters$stm$plot == "stm_summary")
    {
      return(plot(model, type="summary", labeltype=parameters$stm$labeltype));
    }
    else if (parameters$stm$plot == "stm_labels")
    {
      return(plot(model, type="labels", labeltype=parameters$stm$labeltype));
    }
    else if (parameters$stm$plot == "stm_perspectives")
    {
      return(plot(model, type="perspectives", topics=as.numeric(parameters$stm$perspective_topics), labeltype=parameters$stm$labeltype));
    }
    else if (parameters$stm$plot == "stm_hist")
    {
      return(plot(model, type="hist", labeltype=parameters$stm$labeltype));
    }
    else if(parameters$stm$plot == "topic_corr_simple")
    {
      return(plot(topicCorr(model, method="simple")));
    }
    else if(parameters$stm$plot == "topic_corr_huge")
    {
      return(plot(topicCorr(model, method="huge")));
    }
      
    # Sonst Standard Plot ausgeben
    return(plot(model));
  }
}
