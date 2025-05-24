# Cuisine Code Database Management Runbook

## Overview

This runbook covers database operations, maintenance, and troubleshooting for the Cuisine Code PostgreSQL database.

## Database Architecture

- **Primary Database**: Production database cluster
- **Read Replicas**: For reporting and high-read operations
- **Backup Storage**: Off-site encrypted backups
- **Database Version**: PostgreSQL 14.x

## Regular Maintenance Tasks

### Daily Maintenance

- **Automated Backups**:
  - Full backups run daily at 01:00 UTC
  - Transaction logs backed up every 15 minutes
  - Retention: 7 daily, 4 weekly, 12 monthly

- **Monitoring Checks**:
  - Connection pool usage
  - Transaction volume
  - Query performance
  - Storage utilization

### Weekly Maintenance

- **Index Maintenance**:
  - Run index analysis: `make db-analyze env=production`
  - Review index usage statistics
  - Implement recommended indexes

- **Query Performance Review**:
  - Identify slow queries: `make db-slow-queries env=production`
  - Tune problematic queries
  - Update query patterns if needed

### Monthly Maintenance

- **Storage Management**:
  - Review growth patterns
  - Plan capacity increases if needed
  - Archive old data if applicable

- **Security Audit**:
  - Review user permissions
  - Audit connection patterns
  - Check for unusual activity

## Backup and Recovery

### Creating Manual Backups

```bash
# Create a full backup
make db-backup env=production type=full

# Create a schema-only backup
make db-backup env=production type=schema

# Create a data-only backup
make db-backup env=production type=data

# Create a backup of specific tables
make db-backup env=production type=tables tables=users,recipes
```

### Restoring from Backups

```bash
# Full restore (requires downtime)
make db-maintenance-mode enable
make db-restore env=production backup={BACKUP_NAME}
make db-maintenance-mode disable

# Restore specific tables
make db-restore-tables env=production backup={BACKUP_NAME} tables=users,recipes

# Restore to development environment
make db-restore env=development backup={BACKUP_NAME}
```

### Verifying Backups

```bash
# Verify backup integrity
make db-verify-backup backup={BACKUP_NAME}

# Test restore to staging
make db-test-restore env=staging backup={BACKUP_NAME}
```

## Database Migrations

### Creating Migrations

1. Create a new migration file:
   ```bash
   make db-create-migration name=add_user_preferences
   ```

2. Edit the migration file in `db/migrations/YYYYMMDDHHMMSS_add_user_preferences.sql`

3. Test the migration locally:
   ```bash
   make db-migrate env=development
   ```

### Deploying Migrations

1. Run migrations in staging:
   ```bash
   make db-migrate env=staging
   ```

2. Verify application functionality with new schema

3. Run migrations in production (during deployment):
   ```bash
   make db-migrate env=production
   ```

### Rolling Back Migrations

```bash
# Roll back the last migration
make db-rollback env=production steps=1

# Roll back to a specific version
make db-rollback env=production version=20250315120000
```

## Performance Tuning

### Configuration Management

Update database parameters:

```bash
# View current configuration
make db-show-config env=production

# Update configuration
make db-update-config env=production param=max_connections value=200
```

Common optimization parameters:

- `shared_buffers`: 25% of total RAM
- `effective_cache_size`: 75% of total RAM
- `work_mem`: 32-64MB per connection
- `maintenance_work_mem`: 256MB
- `random_page_cost`: 1.1 for SSD, 4.0 for HDD
- `effective_io_concurrency`: 200 for SSD, 2 for HDD

### Index Management

```bash
# Analyze table and recommend indexes
make db-analyze-table env=production table=recipes

# Create index
make db-create-index env=production table=recipes columns=user_id,created_at

# Remove unused indexes
make db-remove-unused-indexes env=production
```

### Query Optimization

1. Identify slow queries:
   ```bash
   make db-slow-queries env=production
   ```

2. Explain a specific query:
   ```bash
   make db-explain query="SELECT * FROM recipes WHERE user_id = 123"
   ```

3. Update application code to optimize query patterns

## Troubleshooting

### High Connection Usage

If database connections are near limit:

1. Check for connection leaks in application
2. Adjust connection pool settings
3. Consider increasing `max_connections` (requires restart)
4. Use connection pooling middleware (PgBouncer)

### Slow Queries

For consistently slow queries:

1. Run EXPLAIN ANALYZE on the query
2. Check for missing indexes
3. Verify statistics are up to date: `VACUUM ANALYZE table_name`
4. Consider query rewriting or application-level caching

### Database Locks

For lock contention issues:

1. Identify blocking queries:
   ```bash
   make db-show-locks env=production
   ```

2. Terminate blocking queries if necessary:
   ```bash
   make db-terminate-query env=production pid=12345
   ```

3. Review application transaction patterns

### Disk Space Issues

If running low on disk space:

1. Check database size:
   ```bash
   make db-size env=production
   ```

2. Identify large tables and indexes:
   ```bash
   make db-table-sizes env=production
   ```

3. Consider emergency actions:
   - Remove unneeded indexes
   - Archive old data
   - Increase storage allocation

## Security Management

### User Management

```bash
# Create a new database user
make db-create-user env=production username=reports password=secure_password

# Update user password
make db-update-password env=production username=reports

# Grant permissions
make db-grant-permission env=production username=reports permission=SELECT tables=ALL
```

### Audit and Compliance

```bash
# Enable audit logging
make db-enable-audit env=production

# Generate audit report
make db-audit-report env=production start=2025-03-01 end=2025-03-31

# Check for security best practices
make db-security-check env=production
```

## Disaster Recovery

### Complete Database Failure

1. Initiate incident response:
   ```bash
   make incident-create severity=1 description="Database failure"
   ```

2. Enable maintenance mode:
   ```bash
   make maintenance-mode enable reason="Database restoration in progress"
   ```

3. Restore from latest backup:
   ```bash
   make db-restore env=production backup=latest
   ```

4. Apply transaction logs:
   ```bash
   make db-apply-logs env=production
   ```

5. Verify data integrity:
   ```bash
   make db-verify-integrity env=production
   ```

6. Resume service:
   ```bash
   make maintenance-mode disable
   ```

### Corruption Recovery

1. Identify corrupted tables:
   ```bash
   make db-check-corruption env=production
   ```

2. Restore affected tables:
   ```bash
   make db-restore-tables env=production backup=latest tables=affected_table1,affected_table2
   ```

3. Verify recovery:
   ```bash
   make db-verify-tables env=production tables=affected_table1,affected_table2
   ```

## Database Schema Reference

Key tables and relationships:

- `users`: User accounts and profiles
- `ingredients`: Ingredient definitions
- `transformations`: Cooking transformation definitions
- `recipes`: Recipe definitions
- `recipe_steps`: Individual steps in recipes
- `kitchens`: User kitchen configurations
- `executions`: Recipe execution history

Full schema documentation available in [Database Schema](../database-schema.md)

## Related Documentation

- [Database Schema](../database-schema.md)
- [Backup Policy](../backup-policy.md)
- [Production Database Setup](../production-db-setup.md)
- [Database Scaling Guide](../database-scaling.md)
