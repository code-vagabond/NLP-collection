ti_lda <- function(docs_data, parameters, json_pretty, session_object)
{
	if (parameters$method != "lda")
	return(toJSON(list(status="error", message="Falsches Verfahren gewählt, dieser Fehler sollte nicht auftreten.")));

	if (length(session_object) == 0)
	{
  	# Keine alte Session, Daten generieren
  	result <- lda.collapsed.gibbs.sampler(docs_data$documents, parameters$k, docs_data$vocab, parameters$lda$num_iterations, parameters$alpha, parameters$eta, burnin=parameters$lda$burnin);
  	return(result); 	
  }
  else
  {
  	# Session Daten verwenden
  	result <- session_object;

  	# Als Matrix verwenden, damit auch Words/Docs Anzahl mit 1 funktioniert
    # TODO top.topic.words hat Probleme bei Topics==1 ?
  	top_topic_words <- matrix(top.topic.words(result$topics, parameters$lda$top_words_num, parameters$lda$top_by_score), nrow=parameters$lda$top_words_num);
  	top_topic_docs <- matrix(top.topic.documents(result$document_sums, parameters$lda$top_documents_num), nrow=parameters$lda$top_documents_num);

    # Top Words Werte mit ausgeben, für Berechnung siehe LDA top.topics.words
    topics_to_use <- result$topics;
    if (parameters$lda$top_by_score)
    {
    	normalized_topics <- result$topics/(rowSums(result$topics) + 1e-05);
    	scores <- apply(normalized_topics, 2, function(x) x * (log(x + 1e-05) - sum(log(x + 1e-05))/length(x)));

    	topics_to_use <- scores;
    }
    
    top_topic_words_numbers <- apply(topics_to_use, 1, function(x) x);
    # TODO Fehler bei PoS
    top_topic_words_numbers <- top_topic_words_numbers[top_topic_words, , drop=FALSE];  # Unbenutzte Einträge entfernen, drop=FALSE macht keinen Vektor draus.
    top_topic_words_scores <- matrix(mapply(function(i, y, x) top_topic_words_numbers[i, x], top_topic_words, row(top_topic_words), col(top_topic_words)), nrow=nrow(top_topic_words));
    
    # Auch Top Documents Scores ausgeben, Berechnung siehe LDA top.topic.documents
    alpha <- 0.1;
    top_topic_docs_numbers <- t(result$document_sums + alpha)/colSums(result$document_sums + alpha);
    top_topic_docs_scores <- matrix(mapply(function(i, y, x) top_topic_docs_numbers[i, x], top_topic_docs, row(top_topic_docs), col(top_topic_docs)), nrow=nrow(top_topic_docs));
    
    ret <- list(top_topic_words=top_topic_words, top_topic_docs=top_topic_docs, top_topic_words_scores=top_topic_words_scores, top_topic_docs_scores=top_topic_docs_scores);
    return(toJSON(ret, pretty=json_pretty));
  }
}
