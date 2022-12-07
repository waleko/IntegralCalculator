<a href="https://github.com/waleko/IntegralCalculator">
  <img align="left" height="80px" src="assets/logo.svg">
</a>
<h1 style="display: inline;">
  Calcigral
</h1>

<p>Simple integral calculator in Haskell</p>

[![GitHub Workflow Status](https://img.shields.io/github/workflow/status/waleko/IntegralCalculator/IntegralCalculator%20Haskell%20App?style=flat-square)](https://github.com/waleko/IntegralCalculator/actions)
[![GitHub top language](https://img.shields.io/github/languages/top/waleko/IntegralCalculator?logo=github&style=flat-square&color=%235e5086)](https://github.com/waleko/wiki-race)
[![license](https://img.shields.io/github/license/waleko/IntegralCalculator?style=flat-square)](./LICENSE)

<br />
<br />

<p align="center">
  <a href="https://asciinema.org/a/ljwLUvlCR95hhZROGqQVvzph9" target="_blank"><img src="https://asciinema.org/a/ljwLUvlCR95hhZROGqQVvzph9.svg" /></a>
</p>

<br />

## What is Calcigral
Calcigral is a simple command-line utility for approximating the value of definite integrals with one variable over a real line.

## Features
* Three approximation strategies: `Rectangle`, `Trapezoid`, `Paraboloid`
* Can approximate up to any absolute error
* Supports $-\infty$, $+\infty$ approximation bounds
* Function input: any haskell function using `Prelude`
* Command-line interface

## Examples
### Simple
Evaluate $I = \int \limits_0^1 \sin(x) dx = 1 - \cos 1 \approx 0.45969...$

```console
$ calcigral 0 1 sin
Strategy: Rectangle
IntegralResult {value = 0.459997112932708, steps = 15}
===
Strategy: Trapezoid
IntegralResult {value = 0.45954804321221476, steps = 62}
===
Strategy: Paraboloid
IntegralResult {value = 0.45970774492731087, steps = 18}
===
```
> Note: Default max absolute error is 1e-3

### Infinite bounds
Evaluate $I = \int \limits_{-\infty}^{\infty} e^{-x^2} dx = \sqrt \pi \approx 1.77245385...$

```console
$ calcigral MinusInfinity PlusInfinity '\x -> exp (-x * x)' 0.001 1000 -s Paraboloid
Strategy: Paraboloid
IntegralResult {value = 1.7724624243349956, steps = 276}
===
```

### Diverging

Evaluate $I = \int \limits_{0}^{1} \frac{1}{x} dx = \ln 1 - \ln 0 \rightarrow \infty$

```console
$ calcigral 0 1 '\x -> 1 / x' -s Rectangle
Strategy: Rectangle
Diverging
===
```

## Goals of this project
This utility is a educational project for the JetBrains Functional Programming course (Fall 2022) at Constructor University</a>. Hence, the goals of this project are educational.

## License
This project is [BSD-3-Clause](./LICENSE) licensed

---
> [@waleko](https://github.com/waleko)
