# Cuisine Code Monitoring Runbook

## Overview

This runbook details the monitoring setup, alert management, and observability practices for the Cuisine Code application.

## Monitoring Architecture

- **Metrics Collection**: Prometheus
- **Visualization**: Grafana
- **Log Management**: ELK Stack (Elasticsearch, Logstash, Kibana)
- **Alert Management**: Alertmanager + PagerDuty
- **Distributed Tracing**: Jaeger

## Key Metrics

### Application Metrics

- **Request Rate**: Requests per second
- **Error Rate**: Percentage of 4xx/5xx responses
- **Latency**: P50, P95, P99 response times
- **Active Users**: Currently active kitchen sessions
- **Recipe Executions**: Rate of recipe executions

### System Metrics

- **CPU Usage**: Per service and host
- **Memory Usage**: Per service and host
- **Disk Usage**: Storage utilization
- **Network Traffic**: Bytes in/out
- **Connection Pool**: Database connection utilization

### Database Metrics

- **Query Latency**: P50, P95, P99 query times
- **Connection Count**: Active and idle connections
- **Transaction Rate**: Transactions per second
- **Index Usage**: Hit rate for indexes
- **Cache Efficiency**: Buffer cache hit ratio

## Dashboards

### Main Dashboards

1. **Service Overview**: High-level health of all services
   - URL: https://grafana.cuisinecode.com/d/overview
   - Refresh: 1m

2. **API Performance**: Detailed API metrics
   - URL: https://grafana.cuisinecode.com/d/api
   - Refresh: 30s

3. **Database Performance**: Database metrics and query stats
   - URL: https://grafana.cuisinecode.com/d/database
   - Refresh: 1m

4. **Infrastructure**: Host-level metrics
   - URL: https://grafana.cuisinecode.com/d/infrastructure
   - Refresh: 5m

5. **User Experience**: User-facing performance metrics
   - URL: https://grafana.cuisinecode.com/d/users
   - Refresh: 1m

### Creating Custom Dashboards

1. Use the Dashboard Template:
   ```bash
   make dashboard-create name="Custom Dashboard" template=service
   ```

2. Access the new dashboard in Grafana

3. Save dashboard JSON to repository:
   ```bash
   make dashboard-export dashboard_id=custom-dashboard
   ```

## Alerts

### Alert Severity Levels

- **Critical**: Immediate action required, service outage
- **Warning**: Potential issue requiring investigation
- **Info**: Noteworthy event, no immediate action needed

### Common Alerts

#### API Alerts

- **APIHighErrorRate**: Error rate exceeds 5% for 5 minutes
- **APIInstanceDown**: API service instance is down
- **APIHighLatency**: P95 latency exceeds 500ms for 5 minutes

#### Database Alerts

- **DBHighConnections**: Connection pool above 80% for 5 minutes
- **DBSlowQueries**: Increase in slow queries
- **DBHighReplicationLag**: Replication lag exceeds 1 minute

#### Infrastructure Alerts

- **HighCPUUsage**: CPU usage above 80% for 5 minutes
- **HighMemoryUsage**: Memory usage above 80% for 5 minutes
- **DiskSpaceLow**: Disk usage above 85%

### Managing Alerts

#### Silencing Alerts

```bash
# Silence an alert for 2 hours
make alert-silence alertname=APIHighErrorRate duration=2h reason="Known issue, being investigated"

# View current silences
make alert-silences

# Remove a silence
make alert-unsilence silence_id=abc123
```

#### Adjusting Alert Thresholds

```bash
# Update alert threshold
make alert-update alertname=APIHighErrorRate threshold=0.1 duration=10m

# Disable an alert
make alert-disable alertname=APIHighLatency

# Enable an alert
make alert-enable alertname=APIHighLatency
```

## Log Management

### Accessing Logs

```bash
# View real-time logs for a service
make logs service=api env=production

# Search logs for specific terms
make logs-search term="error" service=api env=production timeframe=1h

# Export logs to file
make logs-export service=api env=production timeframe=6h output=api-logs.json
```

### Log Retention Policies

- **Production Logs**: 30 days
- **Staging Logs**: 14 days
- **Development Logs**: 7 days

### Creating Log Alerts

1. Create alert definition:
   ```bash
   make log-alert-create name="Database Errors" pattern="database connection failed" threshold=5 window=5m
   ```

2. Test the alert:
   ```bash
   make log-alert-test name="Database Errors"
   ```

3. Enable the alert:
   ```bash
   make log-alert-enable name="Database Errors"
   ```

## Distributed Tracing

### Accessing Traces

1. Open Jaeger UI: https://jaeger.cuisinecode.com

2. Search for traces:
   - By trace ID
   - By service
   - By operation
   - By duration
   - By tags

3. Export traces for analysis:
   ```bash
   make trace-export trace_id=abc123
   ```

### Analyzing Performance Issues

1. Identify slow endpoints from Grafana dashboards

2. Find corresponding traces in Jaeger

3. Analyze span durations to pinpoint bottlenecks

4. Check for error tags and logs within spans

5. Compare with baseline performance

## Health Checks

### Endpoint Reference

- API Health: https://api.cuisinecode.com/health
- Web Health: https://cc.fx.net/health
- Database Health: Internal (via metrics)

### Manual Health Check

```bash
# Run comprehensive health check
make health-check env=production

# Check specific service
make health-check service=api env=production
```

### Synthetic Monitoring

Uptime checks run every minute from multiple regions:

- Homepage: https://cc.fx.net
- API: https://api.cuisinecode.com/health
- Critical flows (every 5 minutes)

## Capacity Planning

### Monitoring Resource Usage

```bash
# Generate capacity report
make capacity-report timeframe=30d

# Forecast resource needs
make capacity-forecast months=3
```

### Scaling Guidelines

- **CPU Consistently >70%**: Scale horizontally or vertically
- **Memory Consistently >75%**: Increase memory allocation
- **Database Connections >70%**: Increase connection pool or add read replicas
- **Disk Usage >80%**: Increase storage or implement archiving

## Troubleshooting

### Performance Issues

1. Check Grafana dashboards for anomalies
2. Review recent deployments or changes
3. Analyze traces for slow operations
4. Check resource utilization
5. Review logs for errors
6. Check database query performance

### Alert Storms

1. Identify root alert triggering cascade
2. Silence secondary alerts
3. Focus on resolving primary issue
4. Document relationship for future prevention

### False Positives

1. Document the false positive
2. Adjust alert thresholds or conditions
3. Add additional context to alert description
4. Consider implementing better detection logic

## Related Documentation

- [Monitoring Architecture](../monitoring-architecture.md)
- [Alert Reference](../alert-reference.md)
- [Dashboard Guide](../dashboard-guide.md)
- [Log Format Specification](../log-format.md)
