plottest <- function(data)
{
  barplot(table(data)/length(data)*100, ylab="Häufigkeit in %")
}

# Testfunktionen für Doku
doku_test_plot <- function(parameters, session_object)
{
	# Anzahl Worte in den Dokumenten berechnen
	words <- sapply(strsplit(parameters$doclines, " "), length);

	# Namen für die X-Achse hinzufügen
	names(words) <- parameters$docnames;

	# und plotten
	return(barplot(words, main="Wörter Plot", xlab="Dokument", ylab="Wörter"));
}
doku_test_berechnung <- function(parameters, session_object)
{
	library(jsonlite);

	# Wenn kein Dokument ausgewählt ist, Fehlermeldung ausgeben
	if (parameters$selected == -1)
	{
		return(toJSON(list(status="error")));
	}
	else
	{
		# Achtung: R beginnt bei 1 zu zählen!
		parameters$selected <- parameters$selected+1;
	}

	# Sonst alle Wörter die im selektierten Dokument vorkommen auflisten
	words <- strsplit(parameters$doclines[parameters$selected], " ");

	# Alle doppelten entfernen und Liste zurückgeben
	words <- unique(words[[1]]);

	# und zurückgeben
	return(toJSON(list(status="ok", result=words)));
}
doku_test_plot2_prepare <- function(parameters, session_object)
{
	# Berechnung aus dem ersten Beispiel durchführen
	words <- sapply(strsplit(parameters$doclines, " "), length);
	names(words) <- parameters$docnames;

	# Die berechneten Daten "merken"
	return(words);
}
test_plot2_plot <- function(parameters, session_object)
{
	if (length(session_object) == 0)
	{
		# Es ist kein Session Objekt vorhanden, Fehlermeldung ausgeben
		plot.new();
		return(text(0.5, y=0.5, labels=c("Error: Session Object is empty!")));
	}
	
	# Sonst können wir einfach die zuvor berechneten Daten auslesen
	words <- session_object;

	# und wieder plotten
	return(barplot(words, main=parameters$title, xlab="Dokument", ylab="Wörter"));
}