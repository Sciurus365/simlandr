get_neighbor_idx <- function(x, y, nr, nc) {
  res <- list()
  for (i in c(-1, 0, 1)) {
    for (j in c(-1, 0, 1)) {
      if (i == 0 & j == 0) next
      if (x + i <= nr & x + i >= 1 & y + j <= nr & y + j >= 1) res <- append(res, list(c(x + i, y + j)))
    }
  }
  return(res)
}


dijkstra <- function(d, s, e) {
  nr <- nrow(d)
  nc <- ncol(d)
  D <- matrix(Inf, nr, nc)
  P <- array(NA, dim = c(nr, nc, 2))
  Q <- matrix(Inf, nr, nc)
  Q[s[1], s[2]] <- 0
  Q_size <- 1
  while (Q_size > 0) {
    v <- arrayInd(which.min(Q), dim(Q))
    v_value <- Q[v[1], v[2]]
    Q[v[1], v[2]] <- Inf
    Q_size <- Q_size - 1
    D[v[1], v[2]] <- v_value

    neighs <- get_neighbor_idx(v[1], v[2], nr, nc)
    for (w in neighs) {
      vwLength <- D[v[1], v[2]] + abs(d[v[1], v[2]] - d[w[1], w[2]])
      if (D[w[1], w[2]] != Inf) {
        if (vwLength < D[v[1], v[2]]) stop("ValueError: vwLength < D[v[1], v[2]]")
      }
      else if (vwLength < Q[w[1], w[2]]) {
        if (Q[w[1], w[2]] == Inf) Q_size <- Q_size + 1
        Q[w[1], w[2]] <- vwLength
        P[w[1], w[2], ] <- c(v[1], v[2])
      }
    }
  }

  path <- list()
  while (1) {
    path <- append(path, list(c(e[1], e[2])))
    if (e[1] == s[1] & e[2] == s[2]) break
    e <- P[e[1], e[2], ]
  }

  path <- rev(path)
  return(path)
}
