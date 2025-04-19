# Cuisine Code Client SDKs

## JavaScript SDK

```javascript
// Install via npm
// npm install cuisine-code-sdk

// Usage example
import { KitchenClient } from 'cuisine-code-sdk';

const kitchen = new KitchenClient({
  apiKey: 'your-api-key'
});

// Stack operations
kitchen.push('butter')
  .then(() => kitchen.push('herbs'))
  .then(() => kitchen.transform('chop', { style: 'fine' }))
  .then(() => kitchen.transform('combine'))
  .then(result => console.log('Result:', result))
  .catch(error => console.error('Error:', error));
```

## Python SDK

```python
# Install via pip
# pip install cuisine-code-sdk

# Usage example
from cuisinecode import KitchenClient

kitchen = KitchenClient(api_key='your-api-key')

# Stack operations
kitchen.push('butter')
kitchen.push('herbs')
kitchen.transform('chop', style='fine')
result = kitchen.transform('combine')
print(f'Result: {result}')
```

## Ruby SDK

```ruby
# Install via gem
# gem install cuisine_code

# Usage example
require 'cuisine_code'

kitchen = CuisineCode::KitchenClient.new(api_key: 'your-api-key')

# Stack operations
kitchen.push('butter')
kitchen.push('herbs')
kitchen.transform('chop', style: 'fine')
result = kitchen.transform('combine')
puts "Result: #{result}"
```

## Java SDK

```java
// Add dependency to your project
// Maven, Gradle, etc.

// Usage example
import com.cuisinecode.KitchenClient;
import com.cuisinecode.models.*;

public class CookingExample {
    public static void main(String[] args) {
        KitchenClient kitchen = new KitchenClient("your-api-key");
        
        try {
            kitchen.push("butter");
            kitchen.push("herbs");
            
            TransformOptions options = new TransformOptions();
            options.setStyle("fine");
            
            kitchen.transform("chop", options);
            TransformationResult result = kitchen.transform("combine");
            
            System.out.println("Result: " + result.getName());
        } catch (CuisineCodeException e) {
            System.err.println("Error: " + e.getMessage());
        }
    }
}
```
