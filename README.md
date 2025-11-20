# Opium

*the project is in an early stage and does not provide an adequate interface yet*

Ultimate (or at least pretty versatile) type system engine.

If it is theoretically possible to determine types in a type-annotation-free
piece of code, Opium can do it, provided you ask it correctly.

I have a dream of creating some perfect kind of programming language, that is as
all-rounded as C++, as powerful as Scheme, and as simple as Python. This project
is my yet-another-iteration towards this impossible goal.

In the folder ./osl you can find the language built on top of the Opium-engine:
**O**pium **S**cripting **L**anguage (OSL). Syntax highlighting for VS-Code is
available in some of my other repos on GitHub. OSL "compiles" into scheme
scripts meant to be run/compiled by [Chicken scheme](https://call-cc.org/).
Apparently, scheme is a decent assembly*esque* target. I will not be surprised
when I see a [6GL](https://en.wikipedia.org/wiki/Programming_language_generations)
running on some dialect of Lisp.


## Implementation

Opium type resolver uses a tweaked variant of Prolog. Types and overloads are
resolved by translating the entire code into a Prolog query to determine all
suitable type annotations.

Type-annotation free recursive functions and function overloads are supported.
Both hold not only for the "regular" top-level-functions but also for closures
capturing surrounding values.

Scheme-like S-expressions are chosen for the intermediate representations, which
makes it trivial to machine-generate inputs and machine-process the outputs.
What might be not so trivial is to write the actual logics of the algorithms.


## Building the Project

```bash
# Make sure the requirements are there
git submodule update --init

# Create a build directory
mkdir build

# Configure the project
cmake -B build -S . -DCMAKE_INSTALL_PREFIX=./install

# Build the project
make -C build -j            # ;]
```

