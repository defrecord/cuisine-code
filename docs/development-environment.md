# Development Environment Setup

This document details how to set up the development environment for the Cuisine Code project.

## Prerequisites

### FreeBSD

Cuisine Code is developed and optimized for FreeBSD. For the best development experience, use FreeBSD 14.0 or later.

### Required Software

- **Guile Scheme 3.x**: The core programming language
- **Emscripten**: For WebAssembly compilation
- **Node.js**: For web development
- **Git**: For version control
- **Make**: For build automation

## Installation

### 1. Install Dependencies

```bash
# Update package repository
pkg update

# Install core dependencies
pkg install -y guile3 emscripten node npm gmake git

# Install documentation tools
pkg install -y mermaid-cli openapi-generator

# Install testing tools
pkg install -y guile-test
```

### 2. Clone the Repository

```bash
git clone https://github.com/defrecord/cuisine-code.git
cd cuisine-code
```

### 3. Set Up the Project

```bash
# Create project directories
make setup

# Install project dependencies
make deps
```

## Development Workflow

### Building the Project

```bash
# Build everything
make all

# Build specific components
make compile-scheme
make compile-c
make compile-wasm
```

### Running the Development Server

```bash
# Start the development server
make serve
```

This will:
- Start the API server on port 3001
- Start the web server on port 3000
- Enable livereload for automatic browser refresh

### Running Tests

```bash
# Run all tests
make test

# Run specific test suites
make test-core
make test-game
make test-ui
```

## Project Structure

```
cuisine-code/
├── scheme/               # Scheme source code
│   ├── src/              # Main source code
│   │   ├── core/         # Core implementation
│   │   ├── game/         # Game mechanics
│   │   ├── ui/           # User interface
│   │   └── server/       # Server implementation
│   └── tests/            # Test suites
├── web/                  # Web interface
│   ├── src/              # Web source files
│   └── wasm/             # WebAssembly output
├── c-output/             # C compilation output
├── tools/                # Development tools
├── docs/                 # Documentation
├── scripts/              # Utility scripts
└── config/               # Configuration files
```

## Code Style Guidelines

### Scheme Style

- Use 2-space indentation
- Use kebab-case for function and variable names
- Include docstrings for all public functions
- Group related functions together

### JavaScript Style

- Use 2-space indentation
- Follow ES6+ conventions
- Use camelCase for variables and functions
- Use PascalCase for classes and components

## Version Control Workflow

We follow a feature branch workflow:

1. Create a feature branch:
   ```bash
   git checkout -b feature/your-feature-name
   ```

2. Make your changes and commit:
   ```bash
   git add .
   git commit -m "feat: Add your feature description"
   ```

3. Push your branch:
   ```bash
   git push origin feature/your-feature-name
   ```

4. Create a pull request to the main branch

## Troubleshooting

### Common Issues

#### 1. Compilation Errors

If you encounter compilation errors:

```bash
# Clean build artifacts
make clean

# Rebuild from scratch
make all
```

#### 2. Development Server Issues

If the development server fails to start:

```bash
# Check for port conflicts
ss -tulpn | grep -E '3000|3001'

# Restart the server
make serve
```

#### 3. WebAssembly Compilation Issues

If WebAssembly compilation fails:

```bash
# Update Emscripten
emcc --clear-cache
emcc --check

# Try compilation again
make compile-wasm
```

## Additional Tools

### Recommended Editor: Emacs

For the best development experience with Scheme, we recommend using Emacs with:

- Geiser: For Scheme interaction
- Paredit: For structural editing
- Org-mode: For literate programming

### Alternative Editors

- VSCode with "Magic Racket" extension
- Vim with "vim-sexp" and "vim-scheme" plugins
- IntelliJ IDEA with "Cursive" plugin

## Getting Help

If you need help with the development environment:

- Check the [GitHub Issues](https://github.com/defrecord/cuisine-code/issues)
- Join our [Discord Server](https://discord.gg/cuisinecode)
- Email the development team at dev@cuisinecode.com
