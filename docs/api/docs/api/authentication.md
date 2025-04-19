# Cuisine Code API Authentication

## API Key Authentication

For server-to-server communication, use API key authentication:

```
Authorization: Bearer YOUR_API_KEY
```

API keys can be generated in the developer dashboard.

## OAuth 2.0 Authentication

For user-specific operations, use OAuth 2.0:

1. Register your application in the developer dashboard
2. Implement the OAuth 2.0 authorization code flow
3. Include the access token in API requests:

```
Authorization: Bearer YOUR_ACCESS_TOKEN
```

## Scopes

The following OAuth scopes are available:

- `kitchen:read` - Read access to kitchen operations
- `kitchen:write` - Write access to kitchen operations
- `recipe:read` - Read access to recipes
- `recipe:write` - Write access to recipes
- `user:read` - Read access to user profile
- `user:write` - Write access to user profile
- `social:read` - Read access to social features
- `social:write` - Write access to social features
```
