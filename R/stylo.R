ti_stylo <- function(doclines, parameters, json_pretty)
{
  if (parameters$method != "stylo")
    return(toJSON(list(status="error", message="Falsches Verfahren gewählt, dieser Fehler sollte nicht auftreten.")));
  
  library(stylo);
  
  if (parameters$stylo$method == "unsupervised")
  {
    parsed_corpus <- strsplit(doclines, " ");
    names(parsed_corpus) <- parameters$doclines_meta$names;
    
    # Sicherstellen, dass alte Version funktioniert ohne Parameter
    pcaflavour <- parameters$stylo$pca_flavour;
    if (is.null(pcaflavour))
      pcaflavour <- "technical";
    
    result <- stylo(linkage=parameters$stylo$linkage, pca.visual.flavour=pcaflavour, text.id.on.graphs="both", gui=FALSE, parsed.corpus=parsed_corpus, corpus.lang=parameters$stylo$corpus_lang, analysis.type=parameters$stylo$analysis_type, display.on.screen=TRUE, write.pdf.file=FALSE, write.jpg.file=FALSE, write.svg.file=FALSE, write.png.file=FALSE, analyzed.features=parameters$stylo$features$analyzed_features, ngram.size=parameters$stylo$features$ngram_size, preserve.case=parameters$stylo$features$preserve_case, mfw.min=parameters$stylo$mfw$min, mfw.max=parameters$stylo$mfw$max, mfw.incr=parameters$stylo$mfw$incr, start.at=parameters$stylo$mfw$start_at, culling.min=parameters$stylo$culling$min, culling.max=parameters$stylo$culling$max, culling.incr=parameters$stylo$culling$incr, mfw.list.cutoff=parameters$stylo$culling$mfw_list_cutoff, delete.pronouns=parameters$stylo$culling$delete_pronouns, distance.measure=parameters$stylo$distances$measure, sampling=parameters$stylo$sampling$sampling, sample.size=parameters$stylo$sampling$size);
  }
  else if (parameters$stylo$method == "classify")
  {
    # Wenn kein Dokument gewählt, erstes wählen
    # TODO besser in JS machen?
    if (parameters$doclines_meta$selected_document == -1)
      parameters$doclines_meta$selected_document <- 1;
    
    # R zählt ab 1!
    parameters$doclines_meta$selected_document <- parameters$doclines_meta$selected_document + 1;
    
    training_corpus <- strsplit(doclines, " ");
    names(training_corpus) <- parameters$doclines_meta$names;
    # Selektiertes Dokument aus Trainigs Corpus entfernen
    training_corpus <- training_corpus[-parameters$doclines_meta$selected_document];
    
    # TODO Test-Corpus braucht mehr als 1 Element, sonst Fehler?
    test_corpus <- training_corpus;
    #test_corpus <- strsplit(doclines[parameters$doclines_meta$selected_document], " ");
    #names(test_corpus) <- parameters$doclines_meta$names[parameters$doclines_meta$selected_document];
    
    result <- classify(gui=FALSE, training.corpus=training_corpus, test.corpus=test_corpus, corpus.lang=parameters$stylo$corpus_lang, classification.method=parameters$stylo$classification_method, display.on.screen=TRUE, write.pdf.file=FALSE, write.jpg.file=FALSE, write.svg.file=FALSE, write.png.file=FALSE);
    
    # TODO wie kommen wir an den Output?
    
    ret <- list(result=result);  
    return(toJSON(ret, pretty=json_pretty));
  }
  else if (parameters$stylo$method == "oppose")
  {
    # R zählt ab 1!
    parameters$stylo$oppose_corpus_primary <- as.numeric(parameters$stylo$oppose_corpus_primary) + 1;
    parameters$stylo$oppose_corpus_secondary <- as.numeric(parameters$stylo$oppose_corpus_secondary) + 1;
    parameters$stylo$oppose_corpus_test <- as.numeric(parameters$stylo$oppose_corpus_test) + 1;
    
    # Eigentlich mindestens 2 Element nötig!
    # Sonst verdoppeln wir einfach das Dokument in der Liste...
    if (length(parameters$stylo$oppose_corpus_primary) < 2)
    {
      parameters$stylo$oppose_corpus_primary <- c(parameters$stylo$oppose_corpus_primary, parameters$stylo$oppose_corpus_primary);
    }
    if (length(parameters$stylo$oppose_corpus_secondary) < 2)
    {
      parameters$stylo$oppose_corpus_secondary <- c(parameters$stylo$oppose_corpus_secondary, parameters$stylo$oppose_corpus_secondary);
    }
        
    corpus_primary <- strsplit(doclines[parameters$stylo$oppose_corpus_primary], " ");
    names(corpus_primary) <- parameters$doclines_meta$names[parameters$stylo$oppose_corpus_primary];
    
    corpus_secondary <- strsplit(doclines[parameters$stylo$oppose_corpus_secondary], " ");
    names(corpus_secondary) <- parameters$doclines_meta$names[parameters$stylo$oppose_corpus_secondary];
    
    # Test Corpus nur anlegen falls angegeben, Oppose funktioniert auch ohne!
    if (!all(is.na(parameters$stylo$oppose_corpus_test)))
    {
      corpus_test <- strsplit(doclines[parameters$stylo$oppose_corpus_test], " ");
      names(corpus_test) <- parameters$doclines_meta$names[parameters$stylo$oppose_corpus_test]; 

      # Test Corpus scheint nicht über Code nutzbar zu sein, also Texte erst als Datei rausschreiben...
      dir.create("test_corpus");
      lapply(names(corpus_test), function(x) writeLines(unlist(corpus_test[x]), file(paste("test_corpus/", x, ".txt"))));
    }

    result <- oppose(gui=FALSE, use.color.graphs=TRUE, titles.on.graph=TRUE, classification=TRUE, visualization=parameters$stylo$oppose_visualization, test.corpus.dir="test_corpus", primary.corpus=corpus_primary, secondary.corpus=corpus_secondary, oppose.method=parameters$stylo$oppose_method, text.slice.length=parameters$stylo$oppose_text_slice_length, text.slice.overlap=parameters$stylo$oppose_text_slice_overlap, rare.occurrences.threshold=parameters$stylo$oppose_rare_occurrences_threshold, zeta.filter.threshold=parameters$stylo$oppose_zeta_filter_threshold, display.on.screen=TRUE, write.pdf.file=FALSE, write.jpg.file=FALSE, write.svg.file=FALSE, write.png.file=FALSE);
    
    # TODO Hier den Test-set Ordner wieder löschen?

    return(result);
  }
  else
  {    
    return(toJSON(list(status="error", message="Unbekannte Methode für Stylo gewählt.")));
  }
}
