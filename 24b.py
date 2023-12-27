import re
from z3 import IntVector, Solver


with open("24t.in") as f:
    ls = f.read().strip().split("\n")

ns = [list(map(int, re.findall("-?\d+", x))) for x in ls]
ps = ns[:3]

x, y, z, dx, dy, dz = IntVector("solutions", 6)
ts = IntVector("times", len(ps))
s = Solver()

for t, (xi, yi, zi, dxi, dyi, dzi) in zip(ts, ps):
    s.add(x + t*dx == xi + t*dxi)
    s.add(y + t*dy == yi + t*dyi)
    s.add(z + t*dz == zi + t*dzi)

s.check()
m = s.model()
result = m[x].as_long() + m[y].as_long() + m[z].as_long()

print(result)

