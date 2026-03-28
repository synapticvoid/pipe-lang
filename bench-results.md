| Command | Mean [s] | Min [s] | Max [s] | Relative |
|:---|---:|---:|---:|---:|
| `./zig-out/bin/pipe samples/fibonacci.pipe` | 4.492 ± 0.037 | 4.448 | 4.545 | 56.23 ± 2.10 |
| `./zig-out/bin/pipe --interp samples/fibonacci.pipe` | 12.302 ± 0.123 | 12.171 | 12.449 | 154.01 ± 5.81 |
| `python3 samples/fibonacci.py` | 0.080 ± 0.003 | 0.077 | 0.085 | 1.00 |
