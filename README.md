# Opium

*the project is in an eraly stage and does not provide an adequate interface yet*

Ultimate static type system.

If it is theoretically possible to determine types in a type-annotation-free
piece of code, Opium will do it.  
**Note**: dynamic dispatch not supported yet


## Implementation

Opium type resolver uses a tweaked variant of Prolog. Types and overloads are
resolved by translating the entire code into a Prolog query to determine all
sutiable type annoations.

Type-annotation free recursive fucntions and function overloads are supported.
Both hold not only for the "regular" functions but also for closures (functions
that capture variables from the surrounding context).

Scheme-like S-expressions are chosen for the intermediate representation (IR) of
the code.

Maybe take a look into the opium/test.scm can give a better idea on what is this
project about. The code there defines few functions and evaluates them few times.
Based on type requirements of only the primitive functions Opium determines types
for everything else in that code.

Run the command below to see the annotated version of the code.
```bash
opium -l test.sup.scm test.scm
```
In the output you should also find a working Scheme code generated from this
input that could be run by any scheme interpreter supporting srfi-11 (you may
need to import the srfi-11 extention explicitly if you want to verify it runs)


## Building the Project

```bash
# Create a build directory
mkdir build
cd build

# Configure the project
cmake ..

# Build the project
make
```


## Run the Example


## Running Tests

```bash
# In the build directory
make test
```

## Generating Documentation

The project uses Doxygen for API documentation. To generate the documentation:

### Prerequisites

Make sure you have Doxygen installed:

```bash
# On Debian/Ubuntu
sudo apt-get install doxygen

# On Fedora/RHEL
sudo dnf install doxygen

# On macOS with Homebrew
brew install doxygen
```

### Building Documentation

```bash
# In the build directory
make docs
```

The documentation will be generated in the `docs/html` directory.
Open `docs/html/index.html` in your web browser to view it.