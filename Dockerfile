# Cuisine Code Dockerfile
# Copyright (c) 2025 Aidan Pace

# Build stage
FROM freebsd:14.0-RELEASE as builder

# Install build dependencies
RUN pkg update && pkg install -y \
    guile3 \
    emscripten \
    node \
    npm \
    gmake \
    git

# Copy source code
WORKDIR /app
COPY . .

# Build the project
RUN gmake deps
RUN gmake all

# Runtime stage - API Server
FROM freebsd:14.0-RELEASE as api

# Install runtime dependencies
RUN pkg update && pkg install -y \
    guile3 \
    nginx

# Copy built files
WORKDIR /app
COPY --from=builder /app/scheme/src ./scheme/src
COPY --from=builder /app/config ./config
COPY --from=builder /app/scripts ./scripts

# Expose API port
EXPOSE 3000

# Start API server
CMD ["guile", "-L", ".", "-e", "main", "scheme/src/server/main.scm"]

# Runtime stage - Web Server
FROM nginx:alpine as web

# Copy built web files
COPY --from=builder /app/web/src /usr/share/nginx/html/
COPY --from=builder /app/web/wasm /usr/share/nginx/html/wasm/
COPY --from=builder /app/config/nginx.conf /etc/nginx/conf.d/default.conf

# Expose web port
EXPOSE 80

# Start web server
CMD ["nginx", "-g", "daemon off;"]
