
*the work is in a rather early stage, and probably not ready for production*

# Prelude

I have a dream of creating some perfect kind of programming language that is as
all-rounded as C++, as powerful as Scheme, and as simple as Python. This project
is my yet-another-iteration towards this impossible goal.

In the folder ./osl, you can find the language built on top of the Opium-engine:
**O**pium **S**cripting **L**anguage (OSL). Syntax highlighting for VS-Code is
available in some of my other repos on GitHub. OSL "compiles" into scheme
scripts meant to be run or compiled by [Chicken scheme](https://call-cc.org/).
Apparently, scheme is a decent assembly*esque* target. I will not be surprised
when I see a [6GL](https://en.wikipedia.org/wiki/Programming_language_generations)
running on some dialect of Lisp.

This repository consists of two projects:

- [Opium](#opium) - type-system engine;
- [OSL](#OSL) - statically typed programming language that explores possibilities and limitations of Opium.


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
Where *ultimate* stands for the ability to solve a problem of assigning types for
code units (expressions) of arbitrary code, inferred from an arbitrary programming
language with whichever programming idioms it had chosen.

The inherent issue of solving the problem of type assignments is the rise of
ambiguities. This is especially prevalent when the language exposes generic
functions or allows for function name overloading.

In C++, with the introduction of concepts, it became possible to restrict template
parameters to types that definitively satisfy some particular set of expressions.
This allows disambiguation of templated overloads not only by the apparent signatures,
but also by the code units (expressions) themself. In Opium, functions are, by
construction, concepts of themselves. Rust language features some degree of automatic
backwards-type-deduction; however, it also completely prohibits function overloading.
Haskell features one of the most robust type systems to date. However, even Haskell
has some weird limitations (besides the lack of name overloading).

I don't like the presence of compromises in all the languages (all I'm familiar with)
unless there are real reasons behind them. I respect the "for historical reasons"
kind of arguments, but history goes on, and only C++ seems to be bold enough
to be actually pushing the boundaries towards the absolute point of "do everything
that is possible, because why not". But this point hasn't been reached just yet.
And instead of waiting, I hope Opium can shed some light on the
"...*that is possible*..."-part.

What do I look at:
- What are the permissible implicit operations on types that don't lead to ambiguities. 
- The more job is put on the typecheck, the more computations it will be forced to do, the
  longer compilation time it requires, and up to a point when it becomes impractical.
  In this way, computation time happens to impose limits on a practical type system. <br/>
  &Rightarrow; The type system has to be such that compilation times scale at most linearly
  with the size of the program.
- $$\color{red}{TODO}$$


## Implementation

The typecheck is implemented over a symbolical computation system ([see below](#prolog-engine)),
allowing to attack the problem in a strict logical/algebraic formalism.

### Prolog engine

Opium implements a tweaked variant of the Prolog system. The *tweaks* are addressing
issues such as solving recursive/self-referencing problems that normally result in
unbound computations, and preemptive elimination of solution-branches that would lead
to equivalent results. These are implemented by automatic
[*green cuts*](https://en.wikipedia.org/wiki/Cut_(logic_programming)) in the
choice-points of the Prolog interpreter.

# OSL

Some completely arbitrary code in OSL:
![random OSL snippet](random-osl-code.png)


