#!/bin/bash
# Install dependencies for Cuisine Code development

# Check for FreeBSD
if [ "$(uname)" == "FreeBSD" ]; then
    echo "Installing FreeBSD dependencies..."
    
    # Core dependencies
    pkg install -y guile3
    pkg install -y emscripten
    pkg install -y node npm
    pkg install -y git
    pkg install -y gmake
    pkg install -y clang
    
    # Documentation tools
    pkg install -y mermaid-cli
    pkg install -y openapi-generator
    
    # Testing tools
    pkg install -y guile-test
    
    echo "FreeBSD dependencies installed successfully!"
else
    echo "This script is designed for FreeBSD. Please adapt for your OS."
    exit 1
fi

# Node.js dependencies
echo "Installing Node.js dependencies..."
npm install --prefix ./web

echo "All dependencies installed successfully!"
