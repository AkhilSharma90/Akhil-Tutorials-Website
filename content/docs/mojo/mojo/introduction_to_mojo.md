---
title: "A Getting Started Guide to Mojo"
description: "Mojo Lang description"
icon: "code"
draft: false
---

## Background

- **Common Perception:** Python is often labeled as a slow and merely a scripting language.
- **Frustration with Misconception:** The belief that Python's performance issues stem from the language itself, rather than how the code is written.

## Choosing Mojo: The Main Reason

- **Modular | Mojo as a Bridge:** Mojo combines Python's ease of use with advanced programming capabilities. It's tailored for both research and production environments.
- **Systems Programming Language:** Designed for heterogeneous computing, Mojo excels in handling different processors like CPUs, GPUs, FPGAs, and NPUs.
- **Versatility and Control:** As an AI Application Research Engineer, the author values complete control over the entire process - from research to deployment, without needing extensive hardware knowledge or multiple tools.

## Key Takeaway

- Mojo is not just about enhancing speed; it's about offering a comprehensive solution that balances ease of use with sophisticated programming capabilities for diverse computing environments.

# Getting Started

## Initial Setup

- **Download Mojo SDK:** Essential for running Mojo code.
  - Includes the "Mojo CLI" (Command Line Interface), a versatile tool for executing Mojo code and other tasks.
  - Provides a REPL (Read-Eval-Print Loop) environment for interactive coding.

## Learning and Development Environment

- **Using REPL:** Ideal for beginners, similar to a basic calculator, great for practicing small code snippets.
- **Advancing with Mojo:**
  - Organize code into Mojo files, modules, and packages.
  - Use Visual Studio Code (VSCode) for a more structured coding environment.
  - Leverage a Mojo extension for VSCode offering features like auto-completion and quick fixes.

## Installation on Different Operating Systems

- **Compatibility:** Currently, Mojo SDK is only compatible with Ubuntu & MacOS. Windows users can utilize WSL (Windows Subsystem for Linux) container.
- **Installation Steps:**
  - For Linux or MacOS:
    ```bash
    curl https://get.modular.com | MODULAR_AUTH=mut_e982ece66e6949d593f64xxxx sh -
    modular install mojo
    ```
  - **Setting Environment Variables:** Add MODULAR_HOME and PATH to .bashrc or .zsh files as suggested during installation.

## Updating Mojo SDK

- Run the following commands:
  ```bash
  sudo apt-get update
  sudo apt-get install modular
  modular clean
  modular install mojo
  ```

## Visual Studio Code Extension

- Download the VSCode extension for an enhanced Mojo coding experience.

## Mojo Playground

- Access the Mojo environment in a browser via the Mojo playground link.

# Running Mojo Code

## Using REPL

- Start REPL by typing `mojo`.
- Example:
  ```
  mojo
  # Type your code here
  ```

## Mojo Source File

- Create a file (e.g., hello.mojo) and write your Mojo code.
- Example:
  ```mojo
  fn main():
      print("Hello, world!")
  ```

## Executing Code

- Run a Mojo file: `mojo hello.mojo`
- Build an executable: `mojo build hello.mojo`
- Run the executable: `./hello`

## Mojo CLI Usage

- **Basic Commands:**
  - Check version: `mojo --version` or `mojo -v`
  - Get help: `mojo --help` or `mojo -h`
  - Run a file: `mojo run ./hello.mojo`
  - Build a file: `mojo build ./hello.mojo`
  - Launch REPL: `mojo repl`
  - Other commands include debug, package, format, doc, and demangle.
