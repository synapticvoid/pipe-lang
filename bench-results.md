| Command | Mean [ms] | Min [ms] | Max [ms] | Relative |
|:---|---:|---:|---:|---:|
| `./zig-out/bin/pipe samples/fibonacci.pipe` | 77.3 ± 0.5 | 76.9 | 78.1 | 1.04 ± 0.02 |
| `./zig-out/bin/pipe --interp samples/fibonacci.pipe` | 2087.1 ± 28.3 | 2045.0 | 2123.2 | 28.22 ± 0.57 |
| `python3 samples/fibonacci.py` | 74.0 ± 1.1 | 73.2 | 75.8 | 1.00 |
