dendrogram_original <- function(scores, rows, method, json_pretty)
{
  library(jsonlite);
  
  # Feed the dissimilarity string into a single vector
  A = lapply(strsplit(c(scores), ", "), as.numeric)[[1]];
  
  # create a 0s matrix with the number of sentences as nunber of rows
  M = matrix(0, rows, rows);
  
  # mark the upper left triangle of matrix including diagnole line with 1s
  # but leave out the last rows and columns
  M [row(M)+col(M)<=rows+1 & row(M) != rows & col(M) != rows] <- 1 ;
  
  # invert matrix
  M.v <- apply(M, 2, rev);
  
  # remove the 1s marks from the diagnol line of the inverted matrix
  diag(M.v) = 0;
  
  # replace the 1s in the matrix with values from the similarity vector
  M.v[M.v==1]= A;
  
  
  B = dist(M.v);
  C = hclust(B, method=method);
  
  # return cluster matrix as tree and cophonetic matrix
  ret <- list(cluster_matrix= cutree(C, k=1:rows), coph = as.matrix(cophenetic(C)));
  
  # Rueckgabe direkt in JSON
  return(toJSON(ret, pretty=json_pretty));
  
  return (cluster_matrix);
  
}

dendrogram_alt <- function(scores, method, json_pretty)
{
	library(jsonlite);

	# Eingabe ist untere Dreiecksmatrix als JSON String, jeweils ein Array für eine Zeile der Matrix
	# Matrix bauen, macht toJSON automatisch
	A <- fromJSON(scores);
	B <- as.dist(A);

	# Anzahl Rows(==Colums==Sätze)
	rows <- nrow(A);

	C <- hclust(B, method=method);
	cluster_matrix <- cutree(C, k=1:rows);
	coph <- cophenetic(C);

	# coph eigentlich untere Dreiecksmatrix,
	# hier als Matrix ausgeben -> wird nach oben gespiegelt
	# damit Umwandlung mit JSON funktioniert
	ret <- list(cluster_matrix=cluster_matrix, coph=as.matrix(coph));

	# Rückgabe direkt in JSON
	return(toJSON(ret, pretty=json_pretty));
}

dendrogram_test_plot <- function(scores, method, hang)
{
	library(jsonlite);

	A <- fromJSON(scores);
	B <- as.dist(A);
	C <- hclust(B, method=method);
	if(hang) plot(C, hang=-1) else plot(C);
}

# Fehler versuchen zu lösen

dendrogram <- function(scores, rows, method, json_pretty)
{
	library(jsonlite);

	# Eingabe ist untere Dreiecksmatrix als JSON String, jeweils ein Array für eine Zeile der Matrix
	# Matrix bauen, macht toJSON automatisch
	A <- t(fromJSON(scores));

	# Inf, NA und NaN herausfiltern
	A[!is.finite(A)] <- 1;

	B <- as.dist(A);

	# Anzahl Rows(==Colums==Sätze)
	#rows <- nrow(A);

	C <- hclust(B, method=method);
	cluster_matrix <- cutree(C, k=1:rows);
	coph <- cophenetic(C);

	# coph eigentlich untere Dreiecksmatrix,
	# hier als Matrix ausgeben -> wird nach oben gespiegelt
	# damit Umwandlung mit JSON funktioniert
	ret <- list(cluster_matrix=cluster_matrix, coph=as.matrix(coph));

	# Rückgabe direkt in JSON
	return(toJSON(ret, pretty=json_pretty));
}
