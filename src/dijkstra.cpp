#include <Rcpp.h>
#include <queue>
#include <vector>
#include <cmath>

using namespace Rcpp;

struct Node {
  int index;
  double dist;

  bool operator>(const Node& other) const {
    return dist > other.dist;
  }
};

// [[Rcpp::export]]
List dijkstra(NumericMatrix d, IntegerVector s, IntegerVector e) {
  int nr = d.nrow();
  int nc = d.ncol();
  int n = nr * nc;

  int start_r = s[0] - 1;
  int start_c = s[1] - 1;
  int end_r = e[0] - 1;
  int end_c = e[1] - 1;
  int start_idx = start_c * nr + start_r;
  int end_idx = end_c * nr + end_r;

  std::vector<double> dists(n, std::numeric_limits<double>::infinity());
  std::vector<int> parent(n, -1);
  std::priority_queue<Node, std::vector<Node>, std::greater<Node>> pq;

  dists[start_idx] = 0;
  pq.push({start_idx, 0.0});

  int dr[] = {-1, 1, 0, 0, -1, -1, 1, 1};
  int dc[] = {0, 0, -1, 1, -1, 1, -1, 1};
  double step_dist[] = {1.0, 1.0, 1.0, 1.0, M_SQRT2, M_SQRT2, M_SQRT2, M_SQRT2};

  while (!pq.empty()) {
    Node current = pq.top();
    pq.pop();

    int u = current.index;
    double d_u = current.dist;

    if (d_u > dists[u]) {
      continue;
    }
    if (u == end_idx) {
      break;
    }

    int r = u % nr;
    int c = u / nr;

    double dx;
    double dy;
    if (r == 0) {
      dx = d(r + 1, c) - d(r, c);
    } else if (r == nr - 1) {
      dx = d(r, c) - d(r - 1, c);
    } else {
      dx = (d(r + 1, c) - d(r - 1, c)) / 2.0;
    }

    if (c == 0) {
      dy = d(r, c + 1) - d(r, c);
    } else if (c == nc - 1) {
      dy = d(r, c) - d(r, c - 1);
    } else {
      dy = (d(r, c + 1) - d(r, c - 1)) / 2.0;
    }

    double grad_m = std::sqrt(dx * dx + dy * dy);

    for (int i = 0; i < 8; ++i) {
      int nr_idx = r + dr[i];
      int nc_idx = c + dc[i];

      if (nr_idx >= 0 && nr_idx < nr && nc_idx >= 0 && nc_idx < nc) {
        int v = nc_idx * nr + nr_idx;
        double weight = step_dist[i] * grad_m;

        if (dists[u] + weight < dists[v]) {
          dists[v] = dists[u] + weight;
          parent[v] = u;
          pq.push({v, dists[v]});
        }
      }
    }
  }

  List path;
  int curr = end_idx;
  if (parent[curr] == -1 && curr != start_idx) {
    return path;
  }

  while (curr != -1) {
    path.push_back(IntegerVector::create((curr % nr) + 1, (curr / nr) + 1));
    curr = parent[curr];
  }

  int path_size = path.size();
  List rev_path(path_size);
  for (int i = 0; i < path_size; ++i) {
    rev_path[i] = path[path_size - 1 - i];
  }

  return rev_path;
}
