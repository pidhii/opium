
*the work is in a rather early stage, and probably not ready for production*

# Prelude

I have a dream of creating some perfect kind of programming language, that is as
all-rounded as C++, as powerful as Scheme, and as simple as Python. This project
is my yet-another-iteration towards this impossible goal.

In the folder ./osl you can find the language built on top of the Opium-engine:
**O**pium **S**cripting **L**anguage (OSL). Syntax highlighting for VS-Code is
available in some of my other repos on GitHub. OSL "compiles" into scheme
scripts meant to be run or compiled by [Chicken scheme](https://call-cc.org/).
Apparently, scheme is a decent assembly*esque* target. I will not be surprised
when I see a [6GL](https://en.wikipedia.org/wiki/Programming_language_generations)
running on some dialect of Lisp.

This repository consists of two projects:

- [Opium](#opium) - type-system engine;
- [OSL](#OSL) - staticaly typed programing language that explores possibilities and limitations of Opium.


## Building the Projects
<u>**NOTE:** At the moment only the 64 bit x86_64 system is supported.</u>
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


# Opium

The subject of this project is developing an ultimate type system engine.
Where *ultimate* stands for ability to solve a problem of assigning types for
code units (expressions) of arbitrary code, infered from arbitrary programing
language with whichever programing idioms it had chosen.

The inherent issue of solving the problem of type assignments is the raise of
ambiguities. This is especially prevelent when the language exposes generic
functions or alows for function name overloading.


## Implementation

The typecheck is implemented over a symbolical computation system ([see below](#prolog-engine)),
alowing to attack the problem in a strict logical/algebraic formalism.

### Prolog engine

Opium implements a tweaked variant of the Prolog system. The *tweaks* are addressing
issues such as solving recursive/self-referencing problems that normally result in
unbound computations, and preemptive elimination of solution-branches that would lead
to equivalent results. These are implemented by automatic
[*green cuts*](https://en.wikipedia.org/wiki/Cut_(logic_programming)) in the
choice-points of the Prolog interpreter.

# OSL



