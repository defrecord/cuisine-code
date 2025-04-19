#!/bin/bash
# Create the base project structure for Cuisine Code

# Root level directories
mkdir -p scheme/src/{core,game,ui,social,server}
mkdir -p scheme/tests/{core,game,ui,social,server}
mkdir -p web/{src/{js,css,assets},wasm}
mkdir -p c-output/{src,build}
mkdir -p tools
mkdir -p docs/{api,diagrams}
mkdir -p scripts
mkdir -p config

# Core subdirectories
mkdir -p scheme/src/core/{kitchen,ingredients,transformations,recipes}
mkdir -p scheme/src/game/{levels,challenges,scoring,tutorial}
mkdir -p scheme/src/ui/{terminal,browser,ascii-art,messages}
mkdir -p scheme/src/social/{auth,profiles,sharing,collaboration}
mkdir -p scheme/src/server/{api,db,oauth}

# Web structure
mkdir -p web/src/js/{kitchen,recipes,ui,auth}
mkdir -p web/src/css/{base,components,themes}
mkdir -p web/src/assets/{icons,fonts,images}

# Documentation structure
mkdir -p docs/api/{kitchen,recipe,social,integration}
mkdir -p docs/diagrams/{architecture,flows,components}

echo "Directory structure created successfully!"
