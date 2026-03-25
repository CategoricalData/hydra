# Hydra Benchmark Analysis

This directory contains Jupyter notebooks for analyzing Common Test Suite benchmark results across Hydra implementations.

## Directory Structure

```
analysis/
  python/           # Python implementation analysis
  haskell/          # Haskell implementation analysis (future)
  java/             # Java implementation analysis (future)
  cross-impl/       # Cross-implementation comparison (future)
```

## Generating Benchmark Data

### Python

```bash
cd hydra-python
./bin/benchmark.sh --all -o test_timings.csv
```

See [wiki/Benchmarking.md](../wiki/Benchmarking.md) for details.

## Running the Notebooks

```bash
cd analysis/python
jupyter notebook benchmark_analysis.ipynb
```

Or with JupyterLab:
```bash
jupyter lab benchmark_analysis.ipynb
```

## Analysis Sections

The Python benchmark notebook includes:

1. **Slowest Individual Tests** - Top 30 tests by duration
2. **Slowest Test Groups** - Aggregated by hierarchy level
3. **Kernel vs Generation Comparison** - Runtime vs generated code performance
4. **Duration Distribution** - Percentiles and time buckets
5. **Failed/Skipped Analysis** - Test failure patterns
6. **Performance Patterns** - Keyword analysis of slow tests
7. **Export Summary** - CSV files for cross-implementation comparison
