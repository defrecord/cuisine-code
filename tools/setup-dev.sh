#!/bin/bash
# Setup development environment

# Create Git hooks
mkdir -p .git/hooks

cat > .git/hooks/pre-commit << 'EOF'
#!/bin/bash
# Pre-commit hook for Cuisine Code

# Run tests
echo "Running tests..."
make test

# Check for syntax errors in Scheme files
echo "Checking Scheme syntax..."
find scheme -name "*.scm" -exec guile -c "(load \"{}\")" \; 2>/dev/null

# If any command failed, prevent the commit
if [ $? -ne 0 ]; then
  echo "Tests or syntax checks failed. Commit aborted."
  exit 1
fi

echo "Pre-commit checks passed."
EOF

chmod +x .git/hooks/pre-commit

echo "Development tools setup completed!"
