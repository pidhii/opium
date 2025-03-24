# Opium

A C++ library for logic programming.

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

The documentation will be generated in the `docs/html` directory. Open `docs/html/index.html` in your web browser to view it.