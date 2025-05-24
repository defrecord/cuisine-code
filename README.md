# Cuisine Code

Learn stack-based computing through French culinary metaphors.

![Cuisine Code Logo](docs/logo.png)

## Overview

Cuisine Code is an educational game that teaches programming concepts through the familiar and engaging world of French cuisine. By representing computational operations as cooking techniques, the game makes abstract concepts tangible and intuitive.

### Key Features

- **Stack-Based Operations**: Learn fundamental computing concepts through cooking operations
- **French Culinary Focus**: Master authentic techniques from mirepoix to mother sauces
- **Visual Feedback**: See your stack operations through beautiful culinary visualizations
- **Progressive Learning**: Advance from simple preparations to complex recipes
- **Social Cooking**: Share recipes and cook collaboratively with friends (v4.0)
- **Cross-Platform**: Play in your terminal or browser

## Getting Started

### Installation

```bash
# Clone the repository
git clone https://github.com/defrecord/cuisine-code.git
cd cuisine-code

# Install dependencies
make deps

# Build the project
make all

# Start the development server
make serve
```

Visit `http://localhost:3000` in your browser to start cooking!

### Basic Usage

```scheme
;; Create a new kitchen
(define kitchen (make-kitchen))

;; Push ingredients onto the stack
(kitchen 'push "butter")
(kitchen 'push "herbs")

;; Apply transformations
(kitchen 'transform 'chop '((style . 'fine)))

;; See what's on the stack
(kitchen 'stack)  ;; => ("fine-chopped-herbs" "butter")

;; Apply more transformations
(kitchen 'transform 'combine)
;; => ("combined-butter-and-fine-chopped-herbs")
```

## Documentation

- [Installation Guide](docs/installation.md)
- [User Guide](docs/user-guide.md)
- [API Reference](docs/api-reference.md)
- [Development Guide](docs/development-guide.md)
- [Contributing Guidelines](CONTRIBUTING.md)

## Architecture

Cuisine Code is built with a focus on functional programming principles using Scheme as its core language. The project structure follows a modular design:

```
cuisine-code/
├── scheme/         # Scheme source code
├── web/            # Web interface
├── c-output/       # C compilation output
├── tools/          # Development tools
├── docs/           # Documentation
├── scripts/        # Utility scripts
└── config/         # Configuration files
```

## Built With

- [Guile Scheme](https://www.gnu.org/software/guile/) - Core programming language
- [Emscripten](https://emscripten.org/) - WebAssembly compilation
- [Node.js](https://nodejs.org/) - Web development
- [PostgreSQL](https://www.postgresql.org/) - Database

## Contributing

We welcome contributions! Please see [CONTRIBUTING.md](CONTRIBUTING.md) for guidelines.

## License

This project is licensed under the MIT License - see the [LICENSE](LICENSE) file for details.

## Acknowledgments

- The French culinary tradition for its rich techniques and vocabulary
- Forth and other stack-based languages for inspiration
- The FreeBSD community for support and infrastructure
- All contributors who have helped shape the project
