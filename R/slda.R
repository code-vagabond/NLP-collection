ti_slda_train <- function(docs_data, parameters, json_pretty)
{
  if (parameters$method != "slda")
    return(toJSON(list(status="error", message="Falsches Verfahren gewählt, dieser Fehler sollte nicht auftreten.")));
  
  result <- slda.em(docs_data$documents, parameters$k, docs_data$vocab, parameters$slda$num_e_iterations, parameters$slda$num_m_iterations, parameters$alpha, parameters$eta, parameters$slda$annotations, parameters$slda$params, parameters$slda$variance, lambda=parameters$slda$lambda, method="sLDA", logistic=parameters$slda$logistic, MaxNWts=parameters$slda$maxnwts);
  
  trained_model_serialized <- rawToChar(serialize(result, NULL, TRUE));
  
  return(toJSON(trained_model_serialized, pretty=json_pretty));
}

ti_slda_predict <- function(docs_data, parameters, json_pretty)
{
  library(ggplot2);
  
  if (parameters$method != "slda")
    return(toJSON(list(status="error", message="Falsches Verfahren gewählt, dieser Fehler sollte nicht auftreten.")));
  
  trained_model <- unserialize(charToRaw(fromJSON(parameters$slda$trained_model)));
  
  result <- slda.predict(docs_data$documents, trained_model$topics, trained_model$model, parameters$alpha, parameters$eta, num.iterations=parameters$slda$num_iterations, average.iterations=parameters$slda$average_iterations);
  
  ret <- list(result=result);
  
  return(toJSON(ret, pretty=json_pretty));
}
