# Changelog for `lfudacaching`

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to the
[Haskell Package Versioning Policy](https://pvp.haskell.org/).

## 0.1.0.0 - 2026-03-06

Pure Haskell implementation of cache data structures with three eviction policies:
LFUDA (Least Frequently Used with Dynamic Aging), GDSF (Greedy Dual-Size Frequency),
and LFU (Least Frequently Used).

- Immutable API: `insert`, `insertView`, `lookup`, `peek`, `contains`, `remove`, `purge`
- `Functor`, `Foldable`, `Traversable` instances for `LfudaCache`
- Test suite (29 tests), benchmarks, and demo program
