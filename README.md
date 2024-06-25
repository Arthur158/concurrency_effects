# Research Project CSE3000

This repository links to the work of Arthur Jacques for the Research project course of the CSE bachelor at TU Delft.

Please see their projects [here](https://cse3000-research-project.github.io/).

# Concurrency Effects and Handlers

This project explores the implementation of a model for concurrency using algebraic effects and handlers in Haskell. It includes various modules and laws to ensure that concurrency operations are well-defined and adhere to established principles.

## Table of Contents

- [Introduction](#introduction)
- [Installation](#installation)
- [Usage](#usage)
- [Project Structure](#project-structure)
- [Modules](#modules)

## Introduction

The project provides an in-depth look at implementing concurrency using algebraic effects and handlers. The main goal is to create a framework that respects concurrency laws from the book "Modelling and Analysis of Communicating Systems", and for which we can create handlers that will run the programs.

## Installation

To get started with this project, you need to have Haskell and Stack installed on your machine. Follow the steps below to set up the project:

1. Clone the repository:
   ```sh
   git clone https://github.com/Arthur158/concurrency_effects_handlers.git
   cd concurrency_effects_handlers

2. Install dependencies and build the project:
```sh
   cabal build
```
3. To run main, which contains examples of programs that use the implementation, use the following command:
```sh
   cabal run
```
## Project Structure

The project is organized as follows:
```sh
.
├── app
│   └── Main.hs
├── laws
│   ├── abpmodel.txt
│   ├── assoc.txt
│   ├── commu.txt
│   ├── lemmas.txt
│   ├── lemmasplus.txt
│   ├── LM.txt
│   └── TC.txt
├── src
│   ├── ABPModel.hs
│   ├── Choose.hs
│   ├── Conc.hs
│   ├── End.hs
│   ├── Err.hs
│   ├── HigherOrder.hs
│   ├── Lib.hs
│   ├── LockConc.hs
│   ├── NewConc.hs
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

    laws/: Contains various proofs and laws related to concurrency.
        lemmas.txt: contains different lemmas other proofs use, as weell as some explanations concerning the laws.
        lemmasplus.txt: contains proofs concerning the plus operator.
        LM.txt: contains the proofs for the laws starting with LM.
        TC.txt: contains the proofs for the laws starting with TC.
        commu.txt: contains the proof of the commutativity of ||.
        assoc.txt: contains the proof of the associativity of ||.
        abpmodel.txt: contains the proof of the integrity of our implementation of the ABP model.

    src/: Source files for different modules.
        Lib.hs: Code from Poulsen's blog post http://casperbp.net/posts/2023-07-algebraic-effects/index.html
        HigherOrder.hs: Higher-order functions and effects from Poulsen's second blog post http://casperbp.net/posts/2023-08-encoding-higher-order-effects/
        State.hs: State management module from Poulsen's blog post http://casperbp.net/posts/2023-07-algebraic-effects/index.html
        Err.hs: Error Effect module from Poulsen's blog post http://casperbp.net/posts/2023-07-algebraic-effects/index.html
        End.hs: End module from Poulsen's blog post http://casperbp.net/posts/2023-07-algebraic-effects/index.html
        Choose.hs: Module for the Choose effect, the plus operator, and different handlers for Choose.
        Conc.hs: Implementation of the par function for concurrency.
        NewConc.hs: New implementation of the par function that respects LM4.
        LockConc.hs: Lock-based concurrency.
        ABPModel.hs: Implementation of the ABP model.
        Programs.hs: Example programs using the concurrency framework and the Choose effect
        Util.hs: Utility functions.

