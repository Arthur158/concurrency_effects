# Concurrency Effects and Handlers

This project explores the implementation of a model for concurrency using algebraic effects and handlers in Haskell. It includes various modules and laws to ensure that concurrency operations are well-defined and adhere to established principles.

## Table of Contents

- [Introduction](#introduction)
- [Installation](#installation)
- [Usage](#usage)
- [Project Structure](#project-structure)
- [Modules](#modules)
- [Contributing](#contributing)
- [License](#license)

## Introduction

The project provides an in-depth look at implementing concurrency using algebraic effects and handlers. The main goal is to create a framework that respects concurrency laws from the book "Modelling and Analysis of Communicating Systems", and for which we can create handlers that will run the programs.

## Installation

To get started with this project, you need to have Haskell and Stack installed on your machine. Follow the steps below to set up the project:

1. Clone the repository:
   ```sh
   git clone https://github.com/yourusername/concurrency_effects_handlers.git
   cd concurrency_effects_handlers

2. Install dependencies and build the project:
```sh
   stack setup
   stack build
```
3. To run the project, use the following command:
```sh
   stack run
```
## Project Structure

The project is organized as follows:
```sh
.
├── app
│   └── Main.hs              # Entry point for the application, with some examples of programs
├── laws
│   ├── abp2.hs
│   ├── associativity.txt
│   ├── commutativity.txt
│   ├── first_laws.txt
│   ├── lemmas.txt
│   ├── newcommu.txt
│   └── newassoc
├── src
│   ├── ABPModel.hs
│   ├── Choose.hs
│   ├── Conc.hs
│   ├── End.hs
│   ├── Err.hs
│   ├── HigherOrder.hs
│   ├── HigherOrder2.hs
│   ├── Lib.hs
│   ├── LockConc.hs
│   ├── Programs.hs
│   ├── State.hs
│   └── Util.hs
├── .gitignore
├── CHANGELOG.md
├── LICENSE
├── projecttest.cabal
├── README.md
├── Setup.hs
├── stack.yaml
└── stack.yaml.lock
```
Modules

    app/Main.hs: Entry point for the program, contains some example programs.

application.

    laws/: Contains various proofs and laws related to concurrency.
        lemmas.txt: contains different lemmas other proofs use.
        firstlaws.txt: contains the shorter proofs of some of the laws.
    src/: Source files for different modules.
        ABPModel.hs: Implementation of the ABP model.
        Choose.hs: Module for the choose effect.
        Conc.hs: Concurrency module.
        End.hs: End module from Poulsen's blog post http://casperbp.net/posts/2023-07-algebraic-effects/index.html
        Err.hs: Error Effect module from Poulsen's blog post http://casperbp.net/posts/2023-07-algebraic-effects/index.html
        HigherOrder.hs: Higher-order functions and effects from Poulsen's second blog post http://casperbp.net/posts/2023-08-encoding-higher-order-effects/
        Lib.hs: Code from Poulsen's blog post http://casperbp.net/posts/2023-07-algebraic-effects/index.html
        LockConc.hs: Lock-based concurrency.
        Programs.hs: Example programs using the concurrency framework and the Choose effect
        State.hs: State management module from Poulsen's blog post http://casperbp.net/posts/2023-07-algebraic-effects/index.html
        Util.hs: Utility functions.
