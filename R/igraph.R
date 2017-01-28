ti_igraph <- function(docs_data, parameters, json_pretty)
{
  library(igraph);

  # Beispielsgraph erstellen
  g <- NULL;
  if (parameters$igraph$beispiel == "small_static" || parameters$igraph$beispiel == "small_vsize_static")
  {
    # Seed setzen um immer den gleichen Graph zu erhalten
    set.seed(456123);
    g <- barabasi.game(10, power=1);    
  }
  else if (parameters$igraph$beispiel == "big_static")
  {
    set.seed(456123);
    g <- barabasi.game(1000, power=1);    
  }
  else if (parameters$igraph$beispiel == "big")
  {
    g <- barabasi.game(1000, power=1);
    
  }
  else # == "small" oder "small_vsize"
  {
    g <- barabasi.game(10, power=1);
    layout <- layout_with_fr(g);
  }
  
  # Layout festlegen, besser erkennbar
  layout <- layout_with_fr(g);

  # Berechnung durchführen
  vec <- NULL;
  if (parameters$igraph$centrality == "pr")
  {
    # Pagerank
    vec <- page_rank(g)$vector;
  }
  else if (parameters$igraph$centrality == "eigen")
  { 
    # Eigenvector
    vec <- eigen_centrality(g)$vector;
  } 
  else if (parameters$igraph$centrality == "betweenness")
  { 
    # Betweenness
    vec <- betweenness(g);
  } 
  else if (parameters$igraph$centrality == "closeness")
  { 
    # Closeness
    vec <- closeness(g);      # TODO zu groß??
  } 
  else if (parameters$igraph$centrality == "alpha")
  { 
    # Alpha
    vec <- alpha_centrality(g);
  } 
  else if (parameters$igraph$centrality == "authority")
  { 
    # Authority
    vec <- authority_score(g)$vector;
  } 
  else if (parameters$igraph$centrality == "hub")
  { 
    # Hub
    vec <- hub_score(g)$vector;
  } 
  else if (parameters$igraph$centrality == "power")
  { 
    # Power
    vec <- power_centrality(g);
  } 
  else if (parameters$igraph$centrality == "subgraph")
  { 
    # subgraph
    vec <- subgraph_centrality(g);
  } 
  else if (parameters$igraph$centrality == "degree")
  { 
    # Degree
    vec <- degree(g);
  }
  else
  {
    # Sonst einfach nur plotten...
    return(plot(g, layout=layout));
  }

  # Plotten, bei kleinem Graphen mit Werten, bei großem Vertex-Größe anpassen
  if (parameters$igraph$beispiel == "big" || parameters$igraph$beispiel == "big_static")
  {
    return(plot(g, layout=layout, vertex.size=map(vec, c(1,5)), vertex.label=NA, edge.arrow.size=.3));
  }
  else if (parameters$igraph$beispiel == "small_vsize" || parameters$igraph$beispiel == "small_vsize_static")
  {    
    return(plot(g, layout=layout, vertex.size=map(vec, c(1,20)), vertex.label=NA));
  }
  else
  {    
    return(plot(g, layout=layout, vertex.label=paste(sep="\n", V(g), vec)));
  }

}

# Mapping Helper Funktion

#######################################################################
# arulesViz - Visualizing Association Rules and Frequent Itemsets
# Copyrigth (C) 2011 Michael Hahsler and Sudheer Chelluboina
#
# This program is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 2 of the License, or
# any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License along
# with this program; if not, write to the Free Software Foundation, Inc.,
# 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.

## mapping helper

map <- function(x, range = c(0,1), from.range=NA) {
    if(any(is.na(from.range))) from.range <- range(x, na.rm=TRUE)
    
    ## check if all values are the same
    if(!diff(from.range)) return(
	    matrix(mean(range), ncol=ncol(x), nrow=nrow(x), 
		    dimnames = dimnames(x)))
    
    ## map to [0,1]
    x <- (x-from.range[1])
    x <- x/diff(from.range)
    ## handle single values
    if(diff(from.range) == 0) x <- 0 
    
    ## map from [0,1] to [range]
    if (range[1]>range[2]) x <- 1-x
    x <- x*(abs(diff(range))) + min(range)
    
    x[x<min(range) | x>max(range)] <- NA
    
    x
}
