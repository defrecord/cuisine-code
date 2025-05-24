/**
 * Development Server for Cuisine Code
 * Copyright (c) 2025 Aidan Pace
 */

const http = require('http');
const fs = require('fs');
const path = require('path');
const { exec } = require('child_process');

// Configuration
const config = {
  port: 3000,
  webRoot: './web/src',
  apiProxy: {
    host: 'localhost',
    port: 3001
  },
  mimeTypes: {
    '.html': 'text/html',
    '.css': 'text/css',
    '.js': 'text/javascript',
    '.json': 'application/json',
    '.png': 'image/png',
    '.jpg': 'image/jpeg',
    '.gif': 'image/gif',
    '.svg': 'image/svg+xml',
    '.wasm': 'application/wasm',
    '.ico': 'image/x-icon'
  },
  livereload: true
};

// Create HTTP server
const server = http.createServer((req, res) => {
  // Log request
  console.log(`${new Date().toISOString()} - ${req.method} ${req.url}`);
  
  // Parse URL
  const parsedUrl = new URL(req.url, `http://${req.headers.host}`);
  let pathname = parsedUrl.pathname;
  
  // Normalize pathname
  pathname = pathname === '/' ? '/index.html' : pathname;
  
  // Handle API requests
  if (pathname.startsWith('/api/')) {
    proxyRequest(req, res, pathname);
    return;
  }
  
  // Serve static files
  serveStaticFile(res, pathname);
});

/**
 * Proxy API requests to the API server
 */
function proxyRequest(req, res, pathname) {
  const options = {
    hostname: config.apiProxy.host,
    port: config.apiProxy.port,
    path: pathname,
    method: req.method,
    headers: req.headers
  };
  
  const proxyReq = http.request(options, (proxyRes) => {
    res.writeHead(proxyRes.statusCode, proxyRes.headers);
    proxyRes.pipe(res, { end: true });
  });
  
  proxyReq.on('error', (err) => {
    console.error(`Proxy error: ${err.message}`);
    res.writeHead(502);
    res.end('Bad Gateway');
  });
  
  req.pipe(proxyReq, { end: true });
}

/**
 * Serve a static file
 */
function serveStaticFile(res, pathname) {
  // Determine file path
  const filePath = path.join(config.webRoot, pathname);
  const ext = path.extname(filePath);
  
  // Check if file exists
  fs.access(filePath, fs.constants.R_OK, (err) => {
    if (err) {
      res.writeHead(404);
      res.end('404 Not Found');
      return;
    }
    
    // Read and serve the file
    fs.readFile(filePath, (err, data) => {
      if (err) {
        res.writeHead(500);
        res.end('500 Internal Server Error');
        return;
      }
      
      // Determine content type
      const contentType = config.mimeTypes[ext] || 'application/octet-stream';
      
      // Add livereload script if enabled and serving HTML
      if (config.livereload && contentType === 'text/html') {
        const livereloadScript = `
          <script>
            // Livereload
            const ws = new WebSocket('ws://localhost:35729/livereload');
            ws.onmessage = function(event) {
              if (event.data === 'reload') {
                window.location.reload();
              }
            };
          </script>
        </body>`;
        
        data = data.toString().replace('</body>', livereloadScript);
      }
      
      // Send response
      res.writeHead(200, { 'Content-Type': contentType });
      res.end(data);
    });
  });
}

// Start the server
server.listen(config.port, () => {
  console.log(`\n🍳 Cuisine Code development server running at http://localhost:${config.port}/`);
  console.log(`   Serving files from: ${path.resolve(config.webRoot)}`);
  console.log(`   API requests proxied to: http://${config.apiProxy.host}:${config.apiProxy.port}/api/`);
  
  // Start API server if not already running
  startAPIServer();
  
  // Start livereload server if enabled
  if (config.livereload) {
    startLivereloadServer();
  }
});

/**
 * Start the API server
 */
function startAPIServer() {
  console.log('\n📡 Starting API server...');
  
  const apiProcess = exec('guile -L . -e main scheme/src/server/main.scm', (error) => {
    if (error) {
      console.error(`\n❌ API server error: ${error.message}`);
      return;
    }
  });
  
  apiProcess.stdout.on('data', (data) => {
    console.log(`API: ${data.trim()}`);
  });
  
  apiProcess.stderr.on('data', (data) => {
    console.error(`API ERR: ${data.trim()}`);
  });
  
  // Handle server shutdown
  process.on('SIGINT', () => {
    console.log('\n🛑 Shutting down servers...');
    apiProcess.kill();
    process.exit();
  });
}

/**
 * Start the livereload server
 */
function startLivereloadServer() {
  console.log('\n🔄 Starting livereload server...');
  
  const WebSocket = require('ws');
  const chokidar = require('chokidar');
  
  // Create WebSocket server
  const wss = new WebSocket.Server({ port: 35729 });
  
  // Watch for file changes
  const watcher = chokidar.watch(config.webRoot, {
    ignored: /(^|[\/\\])\../, // Ignore dotfiles
    persistent: true
  });
  
  watcher.on('change', (path) => {
    console.log(`File changed: ${path}`);
    
    // Notify all clients
    wss.clients.forEach((client) => {
      if (client.readyState === WebSocket.OPEN) {
        client.send('reload');
      }
    });
  });
  
  console.log('   Livereload server running on ws://localhost:35729/livereload');
}
