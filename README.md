<p align="center">
  <a href="" rel="noopener">
 <img width=200px height=200px src="assets/logo.png" alt="Project logo"></a>
</p>

<h3 align="center">Quark lang</h3>

<div align="center">

[![Status](https://img.shields.io/badge/status-active-success.svg)]()
[![GitHub Issues](https://img.shields.io/github/issues/quark-lang/quark.svg)](https://github.com/quark-lang/quark/issues)
[![GitHub Pull Requests](https://img.shields.io/github/issues-pr/quark-lang/quark.svg)](https://github.com/quark-lang/quark/pulls)
[![License](https://img.shields.io/badge/license-Creative%20commons-blue.svg)](/LICENSE)
 
</div>

---

<p align="center"> 
    Quark is a compiled lisp-like functional language influenced by and written in Haskell.
    <br> 
</p>

## 📝 Table of Contents

-   [About](#about)
-   [Getting Started](#getting_started)
-   [Manual installation](#manual)
-   [Build](#build)
-   [Getting started](./GUIDE.md)
-   [Usage](#usage)
-   [TODO](./TODO.md)
-   [Contributing](./CONTRIBUTING.md)
-   [Authors](#authors)
-   [Thanks](#thanks)

## 🧐 About <a name = "about"></a>

The main goal of Quark is to offer a language that combines simplicity and productivity in addition to its cool and easy syntax as it provides a easy and powerful functional approach.

## 🏁 Getting Started <a name = "getting_started"></a>

These instructions will get you a copy of the project up and running on your
local machine for development and testing purposes. See
[Manual installation](#manual) for notes on how to install the project on a live
system.

### Prerequisites

To install Quark, you will need:

```
GHC >= 8.10.x
Cabal >= 3.4.0
```

### Installing

---

## 🔧 Running the tests <a name = "tests"></a>

To run the tests:

```bash
 $ cabal test
 # OR in order to get test details
 $ cabal test --test-show-details=direct
```

<!-- ### And coding style tests

The linter is present in order to allow anyone to be able to contribute while
being in the main coherence of the code.

```
 deno lint
``` -->

## 🎈 Usage <a name="usage"></a>

No usage informations for the moment.

## 🚀 Manual installation <a name = "manual"></a>

To deploy Quark lang, do:

```bash
 $ git clone git@github.com:quark-lang/quark.git

 # OR

 $ git init
 $ git remote add origin git@github.com:quark-lang/quark.git
 $ git pull

```

## 🚀 Build <a name = "build"></a>

To build the project, do:

```bash
 $ cabal update
 $ cabal build
 $ ./quark

 # OR

 $ docker build -t quark-lang .
 $ docker run -it --rm quark-lang
```
## ✍️ Authors <a name = "authors"></a>

-   [@thomasvergne](https://github.com/thomasvergne) - Idea & Initial work

<a href="https://github.com/quark-lang/quark/graphs/contributors">
  <img src="https://contributors-img.web.app/image?repo=quark-lang/quark" />
</a>

## ❤️ Special thanks to️ <a name = "thanks"></a>
- [@Wafelack](https://github.com/Wafelack) - Contribution to STD
- [@SuperFola](https://github.com/SuperFola) - Helping about ASTs and compilers...
- [@Mesabloo](https://github.com/Mesabloo) - Helping about variable scoping, module importing...
- [@Uriopass](https://github.com/Uriopass) - Helping about function and variable scoping...
- [@felko](https://github.com/felko) - Helping about module importing...

See also the list of
[contributors](https://github.com/quark-lang/quark/contributors) who
participated in this project.
