# This implementation is from https://math.stackexchange.com/questions/3088292/finding-lowest-elevation-path-between-two-points
# @MISC {3092082,
#     TITLE = {Finding Lowest Elevation Path Between Two Points},
#     AUTHOR = {BBSysDyn (https://math.stackexchange.com/users/6786/bbsysdyn)},
#     HOWPUBLISHED = {Mathematics Stack Exchange},
#     NOTE = {URL:https://math.stackexchange.com/q/3092082 (version: 2019-01-31)},
#     EPRINT = {https://math.stackexchange.com/q/3092082},
#     URL = {https://math.stackexchange.com/q/3092082}
# }
# According to the terms of stack exchange, this content is distributed under the terms of CC BY-SA 4.0.
# https://creativecommons.org/licenses/by-sa/4.0/

from pqdict import pqdict
import numpy as np

def get_neighbor_idx(x,y,dims):
    res = []
    for i in ([0,-1,1]):
        for j in ([0,-1,1]):
            if i==0 and j==0: continue
            if x+i<(dims[0]) and x+i>-1 and y+j<(dims[1]) and y+j>-1:
                res.append((x+i,y+j))
    return res

def dijkstra(C,s,e):    
    D = {}
    P = {}
    Q = pqdict() 
    Q[s] = 0

    while len(Q)>0:
        (v,vv) = Q.popitem()
        D[v] = vv
        neighs = get_neighbor_idx(v[0],v[1],C.shape)
        for w in neighs:
            vwLength = D[v] + np.abs(C[v[0],v[1]] - C[w[0],w[1]])
            if w in D:
                if vwLength < D[v]:
                    raise ValueError
            elif w not in Q or vwLength < Q[w]:
                Q[w] = vwLength
                P[w] = v

    path = []
    while 1:
       path.append(e)
       if e == s: break
       e = P[e]
    path.reverse()
    return path 
