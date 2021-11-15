# This implementation is adapted from https://math.stackexchange.com/questions/3088292/finding-lowest-elevation-path-between-two-points
# @MISC {3092082,
#     TITLE = {Finding Lowest Elevation Path Between Two Points},
#     AUTHOR = {BBSysDyn (https://math.stackexchange.com/users/6786/bbsysdyn)},
#     HOWPUBLISHED = {Mathematics Stack Exchange},
#     NOTE = {URL:https://math.stackexchange.com/q/3092082 (version: 2019-01-31)},
#     EPRINT = {https://math.stackexchange.com/q/3092082},
#     URL = {https://math.stackexchange.com/q/3092082}
# }
# According to the terms of stack exchange, the original content is distributed under the terms of CC BY-SA 4.0.
# https://creativecommons.org/licenses/by-sa/4.0/

get_neighbor_idx <- function(x, y, nr, nc) {
  res <- list()

  if (x == 1) {
    ilist <- c(0, 1)
  } else if (x == nr) {
    ilist <- c(-1, 0)
  } else {
    ilist <- c(-1, 0, 1)
  }

  if (y == 1) {
    jlist <- c(0, 1)
  } else if (y == nc) {
    jlist <- c(-1, 0)
  } else {
    jlist <- c(-1, 0, 1)
  }


  for (i in ilist) {
    for (j in jlist) {
      if (i == 0 & j == 0) next
      res <- append(res, list(c(x + i, y + j)))
    }
  }
  return(res)
}

get_gradient_magnitude <- function(x, y, nr, nc, d) {
  if (x == 1) {
    grad_x <- d[x + 1, y] - d[x, y]
  } else if (x == nr) {
    grad_x <- d[x, y] - d[x - 1, y]
  } else {
    grad_x <- (d[x + 1, y] - d[x - 1, y]) / 2
  }

  if (y == 1) {
    grad_y <- d[x, y + 1] - d[x, y]
  } else if (y == nc) {
    grad_y <- d[x, y] - d[x, y - 1]
  } else {
    grad_y <- (d[x, y + 1] - d[x, y - 1]) / 2
  }

  if (is.nan(grad_x)) grad_x <- 0
  if (is.nan(grad_y)) grad_y <- 0

  return(sqrt(grad_x^2 + grad_y^2))
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
    grad_m <- get_gradient_magnitude(v[1], v[2], nr, nc, d)
    for (w in neighs) {
      vwLength <- D[v[1], v[2]] + sqrt((v[1] - w[1])^2 + (v[2] - w[2])^2) * grad_m
      # if(is.na(vwLength)) return(list(v[1], v[2], d[v[1],v[2]], w[1], w[2], d[w[1],w[2]]))
      if (D[w[1], w[2]] != Inf) {
        if (vwLength < D[v[1], v[2]]) stop("ValueError: vwLength < D[v[1], v[2]]")
      } else if (vwLength < Q[w[1], w[2]]) {
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
