# Cuisine Code API Reference

This document provides a reference for the Cuisine Code API.

## Base URL

```
https://api.cuisinecode.com
```

## Authentication

Most API endpoints require authentication. There are two authentication methods:

### API Key Authentication

For server-to-server communication:

```
Authorization: Bearer YOUR_API_KEY
```

### OAuth 2.0 Authentication

For user-specific operations:

1. Redirect users to the authorization endpoint
2. Exchange the authorization code for an access token
3. Include the access token in API requests:

```
Authorization: Bearer YOUR_ACCESS_TOKEN
```

## Endpoints

### Kitchen API

#### Get Stack

```
GET /api/kitchen/stack
```

Returns the current state of the kitchen stack.

**Response**

```json
{
  "stack": [
    {
      "id": "a1b2c3",
      "type": "ingredient",
      "name": "butter",
      "properties": {
        "state": "solid",
        "temperature": 4
      }
    },
    {
      "id": "d4e5f6",
      "type": "ingredient",
      "name": "herbs",
      "properties": {
        "type": "fresh",
        "flavor": "strong"
      }
    }
  ]
}
```

#### Perform Stack Operation

```
POST /api/kitchen/stack
```

Execute a stack operation (push, pop, swap, etc.).

**Request Body**

```json
{
  "operation": "push",
  "item": {
    "name": "butter",
    "properties": {
      "state": "solid",
      "temperature": 4
    }
  }
}
```

**Response**

```json
{
  "result": "success",
  "stack": [
    {
      "id": "a1b2c3",
      "type": "ingredient",
      "name": "butter",
      "properties": {
        "state": "solid",
        "temperature": 4
      }
    }
  ]
}
```

#### Apply Transformation

```
POST /api/kitchen/transform
```

Apply a culinary transformation to ingredients.
**Request Body**

```json
{
  "transformation": "chop",
  "parameters": {
    "style": "fine"
  }
}
```

**Response**

```json
{
  "result": {
    "id": "g7h8i9",
    "type": "preparation",
    "name": "fine-chopped-herbs",
    "properties": {
      "source_items": ["d4e5f6"],
      "preparation": "fine-chopped"
    }
  }
}
```

#### List Transformations

```
GET /api/kitchen/transformations
```

Get a list of available transformations.

**Response**

```json
[
  {
    "id": "chop",
    "name": "Chop",
    "category": "mechanical",
    "description": "Cut ingredient into pieces",
    "parameters": [
      {
        "name": "style",
        "type": "enum",
        "required": false,
        "default": "medium",
        "options": ["rough", "medium", "fine"]
      }
    ]
  },
  {
    "id": "saute",
    "name": "Sauté",
    "category": "thermal",
    "description": "Cook quickly in a small amount of fat",
    "parameters": [
      {
        "name": "temperature",
        "type": "number",
        "required": false,
        "default": 180
      },
      {
        "name": "duration",
        "type": "number",
        "required": false,
        "default": 60
      }
    ]
  }
]
```

### Recipe API

#### List Recipes

```
GET /api/recipes
```

Get a list of available recipes.

**Parameters**

- `category` (optional): Filter by recipe category
- `difficulty` (optional): Filter by difficulty level

**Response**

```json
[
  {
    "id": "herb-butter",
    "name": "Herb Compound Butter",
    "category": "Basics",
    "difficulty": "beginner",
    "description": "A simple herb-infused butter for enhancing dishes."
  },
  {
    "id": "basic-mirepoix",
    "name": "Basic Mirepoix",
    "category": "Basics",
    "difficulty": "beginner",
    "description": "The aromatic flavor base of French cuisine."
  }
]
```

#### Get Recipe

```
GET /api/recipes/{id}
```

Get a specific recipe by ID.

**Response**

```json
{
  "id": "herb-butter",
  "name": "Herb Compound Butter",
  "category": "Basics",
  "difficulty": "beginner",
  "description": "A simple herb-infused butter for enhancing dishes.",
  "ingredients": [
    ["butter", 250, "g"],
    ["herbs", 30, "g"],
    ["garlic", 2, "cloves"],
    ["salt", 5, "g"]
  ],
  "steps": [
    ["push", "butter"],
    ["transform", "soften", {"temperature": 20}],
    ["push", "herbs"],
    ["transform", "chop", {"style": "fine"}],
    ["push", "garlic"],
    ["transform", "mince"],
    ["push", "salt"],
    ["transform", "combine"],
    ["transform", "shape", {"form": "log"}],
    ["transform", "chill", {"duration": 120}]
  ],
  "expected_result": {
    "type": "compound-butter",
    "properties": {
      "state": "solid",
      "flavor": "herb-garlic"
    }
  },
  "created_at": "2025-01-15T10:30:00Z",
  "updated_at": "2025-02-20T14:45:00Z",
  "author": "system"
}
```

#### Create Recipe

```
POST /api/recipes
```

Create a new recipe.

**Request Body**

```json
{
  "name": "Garlic Aioli",
  "category": "Sauces",
  "difficulty": "intermediate",
  "description": "A classic garlic mayonnaise.",
  "ingredients": [
    ["egg-yolk", 2, "large"],
    ["garlic", 4, "cloves"],
    ["lemon-juice", 15, "ml"],
    ["olive-oil", 250, "ml"],
    ["salt", 5, "g"]
  ],
  "steps": [
    ["push", "garlic"],
    ["transform", "mince"],
    ["push", "egg-yolk"],
    ["push", "lemon-juice"],
    ["transform", "combine"],
    ["transform", "whisk", {"duration": 30}],
    ["push", "olive-oil"],
    ["transform", "emulsify", {"speed": "slow"}],
    ["push", "salt"],
    ["transform", "fold"]
  ],
  "expected_result": {
    "type": "sauce",
    "properties": {
      "consistency": "thick",
      "flavor": "garlic"
    }
  }
}
```

**Response**

```json
{
  "id": "garlic-aioli",
  "name": "Garlic Aioli",
  "category": "Sauces",
  "difficulty": "intermediate",
  "description": "A classic garlic mayonnaise.",
  "ingredients": [
    ["egg-yolk", 2, "large"],
    ["garlic", 4, "cloves"],
    ["lemon-juice", 15, "ml"],
    ["olive-oil", 250, "ml"],
    ["salt", 5, "g"]
  ],
  "steps": [
    ["push", "garlic"],
    ["transform", "mince"],
    ["push", "egg-yolk"],
    ["push", "lemon-juice"],
    ["transform", "combine"],
    ["transform", "whisk", {"duration": 30}],
    ["push", "olive-oil"],
    ["transform", "emulsify", {"speed": "slow"}],
    ["push", "salt"],
    ["transform", "fold"]
  ],
  "expected_result": {
    "type": "sauce",
    "properties": {
      "consistency": "thick",
      "flavor": "garlic"
    }
  },
  "created_at": "2025-04-10T09:15:00Z",
  "updated_at": "2025-04-10T09:15:00Z",
  "author": "user-123"
}
```

### User API

#### Get User Profile

```
GET /api/users/profile
```

Get the current user's profile.

**Response**

```json
{
  "id": "user-123",
  "username": "chef_coder",
  "display_name": "Chef Coder",
  "email": "chef@example.com",
  "location": "Boston",
  "bio": "Passionate about cooking and coding.",
  "preferences": {
    "theme": "dark",
    "notifications": true,
    "language": "en"
  },
  "created_at": "2025-01-01T00:00:00Z"
}
```

#### Update Profile

```
PUT /api/users/profile
```

Update the current user's profile.

**Request Body**

```json
{
  "display_name": "Master Chef Coder",
  "location": "New York",
  "bio": "Professional chef and programmer.",
  "preferences": {
    "theme": "light",
    "notifications": true,
    "language": "en"
  }
}
```

**Response**

```json
{
  "id": "user-123",
  "username": "chef_coder",
  "display_name": "Master Chef Coder",
  "email": "chef@example.com",
  "location": "New York",
  "bio": "Professional chef and programmer.",
  "preferences": {
    "theme": "light",
    "notifications": true,
    "language": "en"
  },
  "created_at": "2025-01-01T00:00:00Z"
}
```

#### Get Kitchen Configuration

```
GET /api/users/kitchen
```

Get the current user's kitchen configuration.

**Response**

```json
{
  "id": "kitchen-123",
  "name": "My Boston Kitchen",
  "location": "Boston",
  "equipment": [
    {
      "id": "sink-1",
      "type": "sink",
      "name": "Kitchen Sink",
      "quantity": 1,
      "properties": {}
    },
    {
      "id": "cutting-board-1",
      "type": "cutting-board",
      "name": "Wooden Cutting Board",
      "quantity": 2,
      "properties": {
        "material": "wood",
        "size": "large"
      }
    },
    {
      "id": "chef-knife-1",
      "type": "chef-knife",
      "name": "8-inch Chef's Knife",
      "quantity": 2,
      "properties": {
        "sharpness": "very-sharp"
      }
    },
    {
      "id": "paring-knife-1",
      "type": "paring-knife",
      "name": "Paring Knife",
      "quantity": 1,
      "properties": {}
    },
    {
      "id": "gas-burner-1",
      "type": "gas-burner",
      "name": "Gas Stove Burner",
      "quantity": 1,
      "properties": {
        "btu": 15000
      }
    },
    {
      "id": "refrigerator-1",
      "type": "refrigerator",
      "name": "Refrigerator",
      "quantity": 1,
      "properties": {
        "temperature": 4
      }
    }
  ],
  "layout": {
    "dimensions": {
      "width": 400,
      "height": 300
    },
    "zones": [
      {
        "id": "prep-zone",
        "name": "Prep Area",
        "position": {"x": 50, "y": 50},
        "size": {"width": 200, "height": 100}
      },
      {
        "id": "cooking-zone",
        "name": "Cooking Area",
        "position": {"x": 50, "y": 160},
        "size": {"width": 200, "height": 100}
      }
    ]
  }
}
```

### Social API

#### Get Shared Recipes

```
GET /api/social/shared-recipes
```

Get a list of recipes shared with the current user.

**Response**

```json
[
  {
    "id": "shared-123",
    "recipe": {
      "id": "french-onion-soup",
      "name": "French Onion Soup",
      "category": "Soups",
      "difficulty": "intermediate"
    },
    "shared_by": {
      "id": "user-456",
      "username": "michelin_star",
      "display_name": "Professional Chef"
    },
    "shared_at": "2025-03-15T14:30:00Z",
    "message": "Try this amazing onion soup recipe!"
  }
]
```

#### Share Recipe

```
POST /api/social/share
```

Share a recipe with other users.

**Request Body**

```json
{
  "recipe_id": "garlic-aioli",
  "user_ids": ["user-456", "user-789"],
  "message": "Check out my homemade aioli recipe!"
}
```

**Response**

```json
{
  "success": true,
  "shared_with": ["user-456", "user-789"]
}
```

## Error Handling

All API endpoints use standard HTTP status codes to indicate success or failure:

- `200 OK`: Request successful
- `201 Created`: Resource created successfully
- `400 Bad Request`: Invalid request (details in response body)
- `401 Unauthorized`: Authentication required
- `403 Forbidden`: Insufficient permissions
- `404 Not Found`: Resource not found
- `500 Internal Server Error`: Server error

Error responses include detailed information:

```json
{
  "error": {
    "code": "invalid_input",
    "message": "Invalid parameters provided",
    "details": {
      "field": "difficulty",
      "issue": "must be one of: beginner, intermediate, advanced, expert"
    }
  }
}
```

## Rate Limiting

API requests are subject to rate limiting:

- 60 requests per minute for authenticated users
- 10 requests per minute for unauthenticated requests

Rate limit information is included in response headers:

- `X-RateLimit-Limit`: Maximum number of requests allowed
- `X-RateLimit-Remaining`: Number of requests remaining
- `X-RateLimit-Reset`: Time when the rate limit resets (Unix timestamp)

When rate limits are exceeded, the API returns a `429 Too Many Requests` status code.

## Versioning

The API uses versioning in the URL path:

```
https://api.cuisinecode.com/v1/...
```

The current version is `v1`. When breaking changes are introduced, a new version (e.g., `v2`) will be released while maintaining backward compatibility for existing versions.

## Webhooks

For real-time updates, you can register webhook endpoints:

```
POST /api/webhooks
```

**Request Body**

```json
{
  "url": "https://your-server.com/webhook",
  "events": ["recipe.created", "recipe.shared", "kitchen.updated"],
  "secret": "your-webhook-secret"
}
```

Webhook payloads are signed using the provided secret, allowing you to verify their authenticity.

## SDK Examples

### JavaScript

```javascript
import { CuisineCodeClient } from 'cuisine-code-sdk';

const client = new CuisineCodeClient({
  apiKey: 'your-api-key'
});

// Push ingredients to the stack
await client.kitchen.push({
  name: 'butter',
  properties: { state: 'solid', temperature: 4 }
});

await client.kitchen.push({
  name: 'herbs',
  properties: { type: 'fresh' }
});

// Apply a transformation
const result = await client.kitchen.transform('chop', { style: 'fine' });

console.log(result);
```

### Python

```python
from cuisine_code import CuisineCodeClient

client = CuisineCodeClient(api_key='your-api-key')

# Push ingredients to the stack
client.kitchen.push(
    name='butter',
    properties={'state': 'solid', 'temperature': 4}
)

client.kitchen.push(
    name='herbs',
    properties={'type': 'fresh'}
)

# Apply a transformation
result = client.kitchen.transform('chop', style='fine')

print(result)
```

## Additional Resources

- [Getting Started Guide](./getting-started.md)
- [API Changelog](./api-changelog.md)
- [WebSocket API Documentation](./websocket-api.md)
- [OAuth Integration Guide](./oauth-integration.md)
