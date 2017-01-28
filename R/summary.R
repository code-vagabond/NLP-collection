ti_summary <- function(doclines, parameters, json_pretty)
{
  library(LSAfun);
  
  if (parameters$method != "summary")
    return(toJSON(list(status="error", message="Falsches Verfahren gewählt, dieser Fehler sollte nicht auftreten.")));
  
  my_k <- parameters$summary$k;
  last_k <- 0;
  cancel_val <- 10;
  tries_needed <- 0;
  
  # Solange, bis genügens Sätze vorhanden sind...
  while (last_k != parameters$summary$k)
  {
    result <- genericSummary(doclines, my_k, parameters$summary$split, parameters$summary$min);
  
    # Doppelte herausfiltern
    result <- unique(result);
    
    # Wieviele Sätze haben wir?
    last_k = length(result);
    
    tries_needed <- tries_needed+1;
    
    # Bei zuvielen Versuchen abbrechen...
    if (cancel_val <= 0)
      break;
    cancel_val <- cancel_val-1;
    
    # Neuer Versuch mit einem Satz mehr
    my_k <- my_k+1;
  }
  
  ret <- list(result=result, tries_needed=tries_needed, cancel_val=cancel_val);
  
  return(toJSON(ret, pretty=json_pretty));
}

ti_summar_alt <- function(doclines, parameters, json_pretty)
{
  library(LSAfun);
  
  if (parameters$method != "summary")
    return(toJSON(list(status="error", message="Falsches Verfahren gewählt, dieser Fehler sollte nicht auftreten.")));
  
  result <- genericSummary(doclines, parameters$summary$k, parameters$summary$split, parameters$summary$min);
  
  ret <- list(result=result);
  
  return(toJSON(ret, pretty=json_pretty));
}
