#!/bin/bash
# Database migration script for Cuisine Code
# Copyright (c) 2025 Aidan Pace

set -e

# Configuration
DB_HOST=${DB_HOST:-localhost}
DB_PORT=${DB_PORT:-5432}
DB_USER=${DB_USER:-cuisine}
DB_PASSWORD=${DB_PASSWORD:-cuisine_password}
DB_NAME=${DB_NAME:-cuisine_code}
MIGRATIONS_DIR=${MIGRATIONS_DIR:-db/migrations}
SCHEMA_FILE=${SCHEMA_FILE:-db/schema.sql}

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[0;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

# Check if psql is available
if ! command -v psql &> /dev/null; then
    echo -e "${RED}Error: PostgreSQL client (psql) is not installed.${NC}"
    exit 1
fi

# Function to display usage information
function show_usage {
    echo -e "${BLUE}Cuisine Code Database Migration Tool${NC}"
    echo "Usage: $0 [options]"
    echo
    echo "Options:"
    echo "  -h, --help              Show this help message"
    echo "  -e, --env ENV           Environment (development, staging, production)"
    echo "  -i, --init              Initialize the database schema"
    echo "  -c, --create NAME       Create a new migration file"
    echo "  -s, --status            Show migration status"
    echo "  -u, --up [STEPS]        Apply migrations (default: all pending)"
    echo "  -d, --down [STEPS]      Rollback migrations (default: 1)"
    echo "  -v, --version VERSION   Migrate to specific version"
}

# Function to initialize the database schema
function init_schema {
    echo -e "${BLUE}Initializing database schema...${NC}"
    
    # Create migrations table if it doesn't exist
    psql -h $DB_HOST -p $DB_PORT -U $DB_USER -d $DB_NAME -c "
        CREATE TABLE IF NOT EXISTS migrations (
            id SERIAL PRIMARY KEY,
            version VARCHAR(20) NOT NULL,
            name VARCHAR(255) NOT NULL,
            applied_at TIMESTAMP WITH TIME ZONE DEFAULT NOW()
        );
    "
    
    # Apply schema
    if [ -f "$SCHEMA_FILE" ]; then
        echo -e "${BLUE}Applying schema from $SCHEMA_FILE...${NC}"
        psql -h $DB_HOST -p $DB_PORT -U $DB_USER -d $DB_NAME -f "$SCHEMA_FILE"
        echo -e "${GREEN}Schema initialized successfully.${NC}"
    else
        echo -e "${RED}Error: Schema file not found: $SCHEMA_FILE${NC}"
        exit 1
    fi
}

# Function to create a new migration file
function create_migration {
    local name=$1
    
    if [ -z "$name" ]; then
        echo -e "${RED}Error: Migration name is required.${NC}"
        exit 1
    fi
    
    # Format the timestamp
    local timestamp=$(date +"%Y%m%d%H%M%S")
    local filename="${MIGRATIONS_DIR}/${timestamp}_${name}.sql"
    
    # Create migrations directory if it doesn't exist
    mkdir -p $MIGRATIONS_DIR
    
    # Create migration file with template
    cat > "$filename" << EOF
-- Migration: $name
-- Created at: $(date -u +"%Y-%m-%d %H:%M:%S UTC")

-- Up Migration
-- =============================================================================
-- Write your UP migration SQL here

-- Down Migration
-- =============================================================================
-- Write your DOWN migration SQL here

EOF
    
    echo -e "${GREEN}Created migration file: $filename${NC}"
}

# Function to show migration status
function show_status {
    echo -e "${BLUE}Migration Status:${NC}"
    
    # Get applied migrations
    applied=$(psql -h $DB_HOST -p $DB_PORT -U $DB_USER -d $DB_NAME -t -c "
        SELECT version FROM migrations ORDER BY version;
    " | tr -d ' ')
    
    # Get all migrations
    all_migrations=$(find $MIGRATIONS_DIR -name "*.sql" | sort)
    
    if [ -z "$all_migrations" ]; then
        echo -e "${YELLOW}No migration files found in $MIGRATIONS_DIR${NC}"
        return
    fi
    
    echo -e "Status | Version        | Name"
    echo -e "-------|----------------|------------------"
    
    for migration in $all_migrations; do
        filename=$(basename $migration)
        version=${filename%%_*}
        name=${filename#*_}
        name=${name%.sql}
        
        if echo "$applied" | grep -q "$version"; then
            status="${GREEN}Applied${NC}"
        else
            status="${YELLOW}Pending${NC}"
        fi
        
        echo -e "$status | $version | $name"
    done
}

# Function to apply migrations
function apply_migrations {
    local steps=$1
    
    # Get applied migrations
    applied=$(psql -h $DB_HOST -p $DB_PORT -U $DB_USER -d $DB_NAME -t -c "
        SELECT version FROM migrations ORDER BY version;
    " | tr -d ' ')
    
    # Get all migrations
    all_migrations=$(find $MIGRATIONS_DIR -name "*.sql" | sort)
    
    if [ -z "$all_migrations" ]; then
        echo -e "${YELLOW}No migration files found in $MIGRATIONS_DIR${NC}"
        return
    fi
    
    # Find pending migrations
    pending_migrations=()
    for migration in $all_migrations; do
        filename=$(basename $migration)
        version=${filename%%_*}
        
        if ! echo "$applied" | grep -q "$version"; then
            pending_migrations+=($migration)
        fi
    done
    
    if [ ${#pending_migrations[@]} -eq 0 ]; then
        echo -e "${GREEN}No pending migrations.${NC}"
        return
    fi
    
    # Apply only the specified number of steps if provided
    if [ -n "$steps" ] && [ $steps -gt 0 ]; then
        if [ $steps -lt ${#pending_migrations[@]} ]; then
            pending_migrations=("${pending_migrations[@]:0:$steps}")
        fi
    fi
    
    echo -e "${BLUE}Applying ${#pending_migrations[@]} migration(s)...${NC}"
    
    for migration in "${pending_migrations[@]}"; do
        filename=$(basename $migration)
        version=${filename%%_*}
        name=${filename#*_}
        name=${name%.sql}
        
        echo -e "${BLUE}Applying migration: $version $name${NC}"
        
        # Extract and apply the UP migration
        sed -n '/^-- Up Migration/,/^-- Down Migration/p' "$migration" | 
            grep -v "^-- Up Migration" | 
            grep -v "^-- Down Migration" | 
            grep -v "^--" | 
            psql -h $DB_HOST -p $DB_PORT -U $DB_USER -d $DB_NAME -f -
        
        # Record the migration
        psql -h $DB_HOST -p $DB_PORT -U $DB_USER -d $DB_NAME -c "
            INSERT INTO migrations (version, name) VALUES ('$version', '$name');
        "
        
        echo -e "${GREEN}Applied migration: $version $name${NC}"
    done
    
    echo -e "${GREEN}Applied ${#pending_migrations[@]} migration(s) successfully.${NC}"
}

# Function to rollback migrations
function rollback_migrations {
    local steps=${1:-1}
    
    # Get applied migrations
    applied=$(psql -h $DB_HOST -p $DB_PORT -U $DB_USER -d $DB_NAME -t -c "
        SELECT version FROM migrations ORDER BY version DESC LIMIT $steps;
    " | tr -d ' ')
    
    if [ -z "$applied" ]; then
        echo -e "${YELLOW}No migrations to rollback.${NC}"
        return
    fi
    
    echo -e "${BLUE}Rolling back $steps migration(s)...${NC}"
    
    for version in $applied; do
        # Find the migration file
        migration=$(find $MIGRATIONS_DIR -name "${version}_*.sql")
        
        if [ -z "$migration" ]; then
            echo -e "${RED}Error: Migration file for version $version not found.${NC}"
            continue
        fi
        
        filename=$(basename $migration)
        name=${filename#*_}
        name=${name%.sql}
        
        echo -e "${BLUE}Rolling back migration: $version $name${NC}"
        
        # Extract and apply the DOWN migration
        sed -n '/^-- Down Migration/,$ p' "$migration" | 
            grep -v "^-- Down Migration" | 
            grep -v "^--" | 
            psql -h $DB_HOST -p $DB_PORT -U $DB_USER -d $DB_NAME -f -
        
        # Remove the migration record
        psql -h $DB_HOST -p $DB_PORT -U $DB_USER -d $DB_NAME -c "
            DELETE FROM migrations WHERE version = '$version';
        "
        
        echo -e "${GREEN}Rolled back migration: $version $name${NC}"
    done
    
    echo -e "${GREEN}Rolled back $steps migration(s) successfully.${NC}"
}

# Function to migrate to a specific version
function migrate_to_version {
    local target_version=$1
    
    if [ -z "$target_version" ]; then
        echo -e "${RED}Error: Target version is required.${NC}"
        exit 1
    fi
    
    # Get applied migrations
    applied=$(psql -h $DB_HOST -p $DB_PORT -U $DB_USER -d $DB_NAME -t -c "
        SELECT version FROM migrations ORDER BY version;
    " | tr -d ' ')
    
    # Get all migrations
    all_migrations=$(find $MIGRATIONS_DIR -name "*.sql" | sort)
    
    if [ -z "$all_migrations" ]; then
        echo -e "${YELLOW}No migration files found in $MIGRATIONS_DIR${NC}"
        return
    fi
    
    # Check if target version exists
    target_file=$(find $MIGRATIONS_DIR -name "${target_version}_*.sql")
    
    if [ -z "$target_file" ]; then
        echo -e "${RED}Error: Migration file for version $target_version not found.${NC}"
        exit 1
    fi
    
    # Determine the latest applied version
    latest_applied=$(echo "$applied" | tail -n 1)
    
    if [ -z "$latest_applied" ]; then
        # No migrations applied, we need to go forward
        echo -e "${BLUE}Migrating forward to version $target_version...${NC}"
        
        # Collect migrations to apply
        migrations_to_apply=()
        for migration in $all_migrations; do
            filename=$(basename $migration)
            version=${filename%%_*}
            
            if [ "$version" -le "$target_version" ]; then
                migrations_to_apply+=($migration)
            fi
        done
        
        # Apply migrations
        for migration in "${migrations_to_apply[@]}"; do
            filename=$(basename $migration)
            version=${filename%%_*}
            name=${filename#*_}
            name=${name%.sql}
            
            echo -e "${BLUE}Applying migration: $version $name${NC}"
            
            # Extract and apply the UP migration
            sed -n '/^-- Up Migration/,/^-- Down Migration/p' "$migration" | 
                grep -v "^-- Up Migration" | 
                grep -v "^-- Down Migration" | 
                grep -v "^--" | 
                psql -h $DB_HOST -p $DB_PORT -U $DB_USER -d $DB_NAME -f -
            
            # Record the migration
            psql -h $DB_HOST -p $DB_PORT -U $DB_USER -d $DB_NAME -c "
                INSERT INTO migrations (version, name) VALUES ('$version', '$name');
            "
            
            echo -e "${GREEN}Applied migration: $version $name${NC}"
        done
    elif [ "$latest_applied" -lt "$target_version" ]; then
        # We need to go forward
        echo -e "${BLUE}Migrating forward to version $target_version...${NC}"
        
        # Collect migrations to apply
        migrations_to_apply=()
        for migration in $all_migrations; do
            filename=$(basename $migration)
            version=${filename%%_*}
            
            if [ "$version" -gt "$latest_applied" ] && [ "$version" -le "$target_version" ]; then
                migrations_to_apply+=($migration)
            fi
        done
        
        # Apply migrations
        for migration in "${migrations_to_apply[@]}"; do
            filename=$(basename $migration)
            version=${filename%%_*}
            name=${filename#*_}
            name=${name%.sql}
            
            echo -e "${BLUE}Applying migration: $version $name${NC}"
            
            # Extract and apply the UP migration
            sed -n '/^-- Up Migration/,/^-- Down Migration/p' "$migration" | 
                grep -v "^-- Up Migration" | 
                grep -v "^-- Down Migration" | 
                grep -v "^--" | 
                psql -h $DB_HOST -p $DB_PORT -U $DB_USER -d $DB_NAME -f -
            
            # Record the migration
            psql -h $DB_HOST -p $DB_PORT -U $DB_USER -d $DB_NAME -c "
                INSERT INTO migrations (version, name) VALUES ('$version', '$name');
            "
            
            echo -e "${GREEN}Applied migration: $version $name${NC}"
        done
    elif [ "$latest_applied" -gt "$target_version" ]; then
        # We need to go backward
        echo -e "${BLUE}Migrating backward to version $target_version...${NC}"
        
        # Get migrations to roll back
        rollback_versions=$(psql -h $DB_HOST -p $DB_PORT -U $DB_USER -d $DB_NAME -t -c "
            SELECT version FROM migrations WHERE version > '$target_version' ORDER BY version DESC;
        " | tr -d ' ')
        
        for version in $rollback_versions; do
            # Find the migration file
            migration=$(find $MIGRATIONS_DIR -name "${version}_*.sql")
            
            if [ -z "$migration" ]; then
                echo -e "${RED}Error: Migration file for version $version not found.${NC}"
                continue
            fi
            
            filename=$(basename $migration)
            name=${filename#*_}
            name=${name%.sql}
            
            echo -e "${BLUE}Rolling back migration: $version $name${NC}"
            
            # Extract and apply the DOWN migration
            sed -n '/^-- Down Migration/,$ p' "$migration" | 
                grep -v "^-- Down Migration" | 
                grep -v "^--" | 
                psql -h $DB_HOST -p $DB_PORT -U $DB_USER -d $DB_NAME -f -
            
            # Remove the migration record
            psql -h $DB_HOST -p $DB_PORT -U $DB_USER -d $DB_NAME -c "
                DELETE FROM migrations WHERE version = '$version';
            "
            
            echo -e "${GREEN}Rolled back migration: $version $name${NC}"
        done
    else
        echo -e "${GREEN}Already at version $target_version.${NC}"
    fi
    
    echo -e "${GREEN}Migration to version $target_version completed.${NC}"
}

# Main script logic
if [ $# -eq 0 ]; then
    show_usage
    exit 0
fi

while [[ $# -gt 0 ]]; do
    case $1 in
        -h|--help)
            show_usage
            exit 0
            ;;
        -e|--env)
            ENV=$2
            shift 2
            ;;
        -i|--init)
            init_schema
            shift
            ;;
        -c|--create)
            create_migration "$2"
            shift 2
            ;;
        -s|--status)
            show_status
            shift
            ;;
        -u|--up)
            if [[ $2 =~ ^[0-9]+$ ]]; then
                apply_migrations $2
                shift 2
            else
                apply_migrations
                shift
            fi
            ;;
        -d|--down)
            if [[ $2 =~ ^[0-9]+$ ]]; then
                rollback_migrations $2
                shift 2
            else
                rollback_migrations
                shift
            fi
            ;;
        -v|--version)
            migrate_to_version "$2"
            shift 2
            ;;
        *)
            echo -e "${RED}Error: Unknown option: $1${NC}"
            show_usage
            exit 1
            ;;
    esac
done
