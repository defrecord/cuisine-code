# Cuisine Code Incident Response Runbook

## Overview

This runbook outlines the steps to follow during a production incident with the Cuisine Code application.

## Incident Severity Levels

- **SEV1**: Complete system outage or data loss
- **SEV2**: Partial system outage or degraded performance affecting multiple users
- **SEV3**: Minor issues affecting a small number of users
- **SEV4**: Cosmetic issues with minimal impact

## Incident Response Team

- **Primary On-Call Engineer**: First responder
- **Secondary On-Call Engineer**: Backup support
- **Incident Commander**: Coordinates response for SEV1/SEV2
- **Communications Lead**: Handles external communications
- **Subject Matter Experts**: Called in as needed

## Response Workflow

### 1. Detection and Triage

- **Alert Received**: Automated alert or user report
- **Acknowledge Alert**: Acknowledge in PagerDuty/OpsGenie
- **Initial Assessment**:
  - Verify the issue exists
  - Determine severity level
  - Check monitoring dashboards
  - Review recent changes

### 2. Escalation (for SEV1/SEV2)

- **Engage Incident Commander**
- **Create Incident Channel**: #incident-{date}-{description}
- **Notify Stakeholders**:
  - Internal teams: Engineering, Product, Support
  - External: Status page update

### 3. Investigation

- **Check System Status**:
  - Database health: `make db-status`
  - API health: `make api-status`
  - Web server health: `make web-status`
- **Review Logs**:
  - Application logs: `make logs service=api`
  - Database logs: `make logs service=db`
  - Web server logs: `make logs service=web`
- **Check Metrics**:
  - CPU/Memory usage
  - Request latency
  - Error rates
  - Database connections

### 4. Mitigation

- **Implement Quick Fixes**:
  - Restart service: `make restart service=<service-name>`
  - Scale up resources: `make scale service=<service-name> count=<number>`
  - Enable maintenance mode: `make maintenance-mode enable`
- **Roll Back Changes** (if applicable):
  - Revert deployment: `make rollback version=<previous-version>`
- **Apply Temporary Workarounds**:
  - Feature flags: `make toggle feature=<feature-name> state=off`

### 5. Resolution

- **Verify Fix**:
  - Run test suite: `make test`
  - Manual verification
  - Monitor metrics for stability
- **Update Status**:
  - Update incident status
  - Notify stakeholders
- **Document Actions Taken**

### 6. Post-Incident

- **Schedule Post-Mortem Meeting**
- **Document Root Cause**
- **Create Action Items**:
  - Preventative measures
  - Process improvements
  - Monitoring enhancements

## Common Issues and Resolutions

### Database Connection Issues

**Symptoms**:
- `ConnectionError` in API logs
- High latency in database-related operations

**Investigation**:
1. Check database status: `make db-status`
2. Check connection pool metrics in Grafana
3. Review recent database changes

**Resolution**:
1. Restart database if unresponsive: `make restart service=db`
2. Check connection string configuration
3. Verify network connectivity between API and database

### High API Latency

**Symptoms**:
- Slow response times reported by users
- Increase in response time metrics

**Investigation**:
1. Check API resource usage
2. Review recent traffic patterns
3. Check for slow database queries

**Resolution**:
1. Scale up API instances: `make scale service=api count=3`
2. Enable caching if appropriate
3. Optimize slow queries

### Website Unavailable

**Symptoms**:
- 502/503 errors when accessing website
- NGINX error logs showing issues

**Investigation**:
1. Check web server status: `make web-status`
2. Verify API is functioning correctly
3. Check load balancer health

**Resolution**:
1. Restart web service: `make restart service=web`
2. Check NGINX configuration
3. Verify SSL certificates are valid

## Contact Information

- **Primary On-Call**: +1-555-0123 (24/7)
- **Secondary On-Call**: +1-555-0124 (24/7)
- **Engineering Manager**: +1-555-0125 (Business hours)

## Useful Commands

```bash
# View logs for a service
make logs service=<api|web|db>

# Restart a service
make restart service=<api|web|db>

# Scale a service
make scale service=<api|web> count=<number>

# Check service status
make status service=<api|web|db>

# Enable/disable maintenance mode
make maintenance-mode <enable|disable>

# Rollback to previous version
make rollback version=<version>
```

## Related Documentation

- [System Architecture](../architecture.md)
- [Database Schema](../database.md)
- [Monitoring Setup](../monitoring.md)
- [Deployment Process](../deployment.md)
