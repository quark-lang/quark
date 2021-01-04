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
    Quark is an interpreted programming language written in Typescript.
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

## 🧐 About <a name = "about"></a>

The main goal of Quark is to offer a language that combines simplicity and
productivity in addition to its cool and easy syntax.

## 🏁 Getting Started <a name = "getting_started"></a>

These instructions will get you a copy of the project up and running on your
local machine for development and testing purposes. See
[Manual installation](#manual) for notes on how to install the project on a live
system.

### Prerequisites

To install Quark, you will need:

```
Deno >= 1.6.0
```

### Installing

---

## 🔧 Running the tests <a name = "tests"></a>

To run the tests:

```
 deno test
```

### Break down into end to end tests

No tests for the moment.

### And coding style tests

The linter is present in order to allow anyone to be able to contribute while
being in the main coherence of the code.

```
 deno lint
```

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
 $ mkdir build
 $ deno compile --unstable src/main.ts -o build/quark-lang
 $ ./build/quark-lang

 # OR

 $ docker build -t quark-lang .
 $ docker run -it --rm quark-lang
```
## ✍️ Authors <a name = "authors"></a>

-   [@thomasvergne](https://github.com/thomasvergne) - Idea & Initial work

<a href="https://github.com/quark-lang/quark/graphs/contributors">
  <img src="https://contributors-img.web.app/image?repo=quark-lang/quark" />
</a>

See also the list of
[contributors](https://github.com/quark-lang/quark/contributors) who
participated in this project.
