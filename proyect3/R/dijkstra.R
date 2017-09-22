#'Dijkstra's algorithm
#'
#'shortest path algorithm in a graph
#'
#'@param graph a data frame
#'@param init_node a numeric scalar
#'@return a numeric vector, with the shortest distance to each node
#'@description find the shortest path algorithm in a graph
#'@references \url{https:\\en.wikipedia.org/wiki/Dijkstra%27s_algorithm.}

dijkstra1<-function(graph,init_node){
  if(!is.data.frame(graph)){stop("FAIL: graph is not a data frame")}
  if(!is.numeric(init_node)){stop("FAIL: init_node is not a number")}
  if(length(graph)!=3){stop("FAIL: the structure of data frame is wrong")}
  if(!all(names(graph)==c("v1","v2","w"))){stop("FAIL: the structure of data frame is wrong")}
  if(init_node>max(graph$v1)){stop("FAIL: there is not a node that big")}
  
  q <- vector()
  dist <- vector()

  for(i in 1:max(graph$v1)){
    dist[i] <- Inf
    q[i] <- i
  }
  
  q <- q[!q%in%init_node]
  dist[init_node] <- 0
  u <- init_node
  dist[graph$v2[which(graph$v1 == init_node)]] <- graph$w[which(graph$v1 == init_node)]
  
  while(length(q)>1){
    u1 <- which(graph$w == min(graph$w[which(graph$v1 == u & graph$v2 %in% q) ]))
    u <- graph$v2[u1[which(graph$v1[u1] == u )]]
    aux <- graph$v2[which(graph$v1 == u)]
    a <- aux[aux %in% q]
    
    for(j in a){
      if(is.infinite(dist[u])){dist[u]<- 0}
      aux2 <- dist[u] + graph$w[which(graph$v1 == u & graph$v2 == j)]
      if(aux2 < dist[j]){
        dist[j] <- aux2
      }
    }
    q <- q[!q %in% u]
  }
  return(dist)
}
  
  