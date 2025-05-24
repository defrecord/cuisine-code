# Cuisine Code Deployment Runbook

## Overview

This runbook details the process for deploying the Cuisine Code application to production.

## Pre-Deployment Checklist

- [ ] All tests passing on CI
- [ ] Code review completed
- [ ] Documentation updated
- [ ] Security review completed (for major releases)
- [ ] Database migration scripts tested
- [ ] Monitoring and alerting verified
- [ ] Rollback plan prepared

## Deployment Window

- **Standard Deployments**: Monday-Thursday, 10:00-14:00 ET
- **Emergency Fixes**: Any time, with proper approval
- **Blackout Periods**: Check the deployment calendar for holidays and freeze periods

## Deployment Process

### 1. Preparation

- **Create Deployment Ticket**:
  ```
  Title: Deploy Cuisine Code v{VERSION} to Production
  Description: 
  - Version: v{VERSION}
  - Key Changes: {SUMMARY OF CHANGES}
  - Risk Assessment: {LOW|MEDIUM|HIGH}
  - Rollback Plan: {ROLLBACK STEPS}
  ```

- **Notify Stakeholders**:
  - Engineering team in #engineering channel
  - Product team in #product channel
  - Support team in #support channel

- **Verify Build Artifacts**:
  - Check that all artifacts are properly built and tagged
  - Verify checksums of Docker images

### 2. Database Migrations

- **Create Backup**:
  ```bash
  make db-backup env=production
  ```

- **Apply Migrations**:
  ```bash
  make db-migrate env=production
  ```

- **Verify Migration Success**:
  ```bash
  make db-status env=production
  ```

### 3. Application Deployment

- **Deploy to Production**:
  ```bash
  make deploy env=production version={VERSION}
  ```

- **Monitor Deployment Progress**:
  - Watch deployment logs
  - Monitor error rates in Grafana
  - Check health endpoints

### 4. Verification

- **Smoke Tests**:
  ```bash
  make test-smoke env=production
  ```

- **Verify Critical Flows**:
  - User authentication
  - Recipe creation
  - Kitchen operations
  - Collaborative features (v4.0+)

- **Monitor Performance**:
  - Response times
  - Database load
  - Memory usage
  - CPU utilization

### 5. Post-Deployment

- **Update Deployment Ticket**:
  - Mark as completed
  - Include deployment timestamp
  - Document any issues encountered

- **Notify Stakeholders**:
  - Announce successful deployment
  - Highlight any known issues or changes in behavior

- **Monitor for Issues**:
  - Watch error rates for next 24 hours
  - Monitor support channels for user-reported issues

## Rollback Procedure

If critical issues are encountered after deployment, follow these steps to rollback:

### 1. Decision to Rollback

- **Criteria for Rollback**:
  - Critical functionality broken
  - Data integrity issues
  - Significant performance degradation
  - Security vulnerability

- **Approval Process**:
  - SEV1/SEV2 incidents: On-call engineer can initiate rollback
  - SEV3/SEV4 issues: Engineering manager approval required

### 2. Rollback Steps

- **Rollback Application**:
  ```bash
  make rollback env=production version={PREVIOUS_VERSION}
  ```

- **Rollback Database** (if needed):
  ```bash
  make db-rollback env=production version={PREVIOUS_VERSION}
  ```

- **Verify Rollback Success**:
  - Run smoke tests
  - Verify critical flows
  - Monitor error rates

### 3. Post-Rollback

- **Update Deployment Ticket**:
  - Document rollback reason
  - Include timestamp and version rolled back to

- **Notify Stakeholders**:
  - Announce rollback and reason
  - Provide ETA for fix if available

## Canary Deployments (v2.0+)

For versions 2.0 and above, canary deployments are supported:

### 1. Deploy to Canary

```bash
make deploy-canary env=production version={VERSION} percentage=10
```

### 2. Monitor Canary

- Watch error rates and performance metrics
- Compare with baseline metrics
- Gradually increase traffic percentage if stable

### 3. Complete Deployment

```bash
make deploy-complete env=production version={VERSION}
```

## Special Considerations

### Configuration Updates

- **Update Environment Variables**:
  ```bash
  make config-update env=production file=config/production.env
  ```

- **Update Feature Flags**:
  ```bash
  make toggle feature={FEATURE_NAME} state={on|off} env=production
  ```

### Infrastructure Updates

For infrastructure changes (Terraform):

```bash
cd terraform
terraform plan -out=plan.tfplan
# Review plan
terraform apply plan.tfplan
```

## Deployment Scripts Reference

```bash
# Standard deployment
make deploy env=production version={VERSION}

# Canary deployment
make deploy-canary env=production version={VERSION} percentage={PERCENTAGE}

# Complete canary deployment
make deploy-complete env=production version={VERSION}

# Rollback deployment
make rollback env=production version={PREVIOUS_VERSION}

# Database migration
make db-migrate env=production

# Database rollback
make db-rollback env=production version={PREVIOUS_VERSION}

# Check deployment status
make deployment-status env=production

# Run smoke tests
make test-smoke env=production
```

## Troubleshooting Common Issues

### Deployment Timeouts

If deployment times out:

1. Check resource availability
2. Verify network connectivity
3. Check for stuck containers or processes
4. Use manual intervention if needed:
   ```bash
   make deployment-debug env=production
   ```

### Database Migration Failures

If migrations fail:

1. Review migration logs
2. Use automated rollback
3. If automated rollback fails, restore from backup:
   ```bash
   make db-restore env=production
make db-restore env=production backup={BACKUP_NAME}
   ```

### Service Health Issues

If services aren't starting properly:

1. Check for resource constraints
2. Verify configuration is correct
3. Check dependent services
4. Review latest logs:
   ```bash
   make logs service={SERVICE_NAME} env=production lines=100
   ```

## Related Documentation

- [CI/CD Pipeline](../ci-cd.md)
- [Monitoring Guide](../monitoring.md)
- [Database Management](../database.md)
- [Infrastructure as Code](../infrastructure.md)
