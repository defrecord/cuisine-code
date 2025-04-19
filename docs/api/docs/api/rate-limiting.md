# Rate Limiting

The Cuisine Code API implements rate limiting to ensure fair usage and system stability.

## Rate Limit Headers

Rate limit information is included in the response headers:

- `X-RateLimit-Limit`: Maximum number of requests allowed in the current time window
- `X-RateLimit-Remaining`: Number of requests remaining in the current time window
- `X-RateLimit-Reset`: Time when the current rate limit window resets (Unix timestamp)

## Default Limits

- 60 requests per minute for authenticated requests
- 10 requests per minute for unauthenticated requests

## Handling Rate Limiting

When rate limits are exceeded, the API returns a `429 Too Many Requests` status code.

Example handling in JavaScript:

```javascript
fetch('https://api.cuisinecode.com/v1/kitchen/stack')
  .then(response => {
    // Check for rate limiting
    if (response.status === 429) {
      const resetTime = response.headers.get('X-RateLimit-Reset');
      const waitTime = resetTime - Math.floor(Date.now() / 1000);
      console.log(`Rate limit exceeded. Try again in ${waitTime} seconds.`);
      return;
    }
    return response.json();
  })
  .then(data => {
    // Process data
  })
  .catch(error => {
    console.error('Error:', error);
  });
```
