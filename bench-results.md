| Command | Mean [ms] | Min [ms] | Max [ms] | Relative |
|:---|---:|---:|---:|---:|
| `./zig-out/bin/pipe samples/fibonacci.pipe` | 78.8 ± 6.8 | 74.8 | 90.9 | 1.00 |
| `./zig-out/bin/pipe --interp samples/fibonacci.pipe` | 2105.1 ± 13.8 | 2089.6 | 2126.3 | 26.71 ± 2.31 |
| `python3 samples/fibonacci.py` | 80.1 ± 1.1 | 78.8 | 81.2 | 1.02 ± 0.09 |
