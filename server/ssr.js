const fs = require("node:fs");
const http = require("node:http");
const path = require("node:path");
const docsifyConfig = require("./ssr-config");

let Renderer;
try {
  Renderer = require("docsify-server-renderer");
} catch (error) {
  console.error("Missing dependency: docsify-server-renderer. Run `npm install` before starting SSR.");
  process.exit(1);
}

const docsDir = path.resolve(__dirname, "../docs");
const templatePath = path.join(docsDir, "index.ssr.html");
const port = Number(process.env.PORT || 3000);

process.chdir(docsDir);

const renderer = new Renderer({
  template: fs.readFileSync(templatePath, "utf8"),
  config: docsifyConfig,
});

const mimeTypes = {
  ".css": "text/css; charset=utf-8",
  ".html": "text/html; charset=utf-8",
  ".js": "text/javascript; charset=utf-8",
  ".json": "application/json; charset=utf-8",
  ".md": "text/markdown; charset=utf-8",
  ".pdf": "application/pdf",
  ".svg": "image/svg+xml; charset=utf-8",
  ".webmanifest": "application/manifest+json; charset=utf-8",
};

function getStaticFile(urlPath) {
  const decodedPath = decodeURIComponent(urlPath);
  const filePath = path.normalize(path.join(docsDir, decodedPath));
  return filePath === docsDir || filePath.startsWith(`${docsDir}${path.sep}`) ? filePath : null;
}

function isStaticRequest(urlPath) {
  return Boolean(path.extname(urlPath));
}

function sendFile(res, filePath) {
  fs.readFile(filePath, (error, content) => {
    if (error) {
      res.writeHead(error.code === "ENOENT" ? 404 : 500);
      res.end(error.code === "ENOENT" ? "Not found" : "Internal server error");
      return;
    }

    res.writeHead(200, {
      "Content-Type": mimeTypes[path.extname(filePath)] || "application/octet-stream",
    });
    res.end(content);
  });
}

async function renderPage(res, url) {
  try {
    const routePath = decodeURIComponent(url.pathname);
    const html = await renderer.renderToString(routePath + url.search);
    res.writeHead(200, { "Content-Type": "text/html; charset=utf-8" });
    res.end(html);
  } catch (error) {
    res.writeHead(500, { "Content-Type": "text/plain; charset=utf-8" });
    res.end(`SSR render failed: ${error.message}`);
  }
}

http
  .createServer((req, res) => {
    const url = new URL(req.url, `http://${req.headers.host || "localhost"}`);

    if (isStaticRequest(url.pathname)) {
      const filePath = getStaticFile(url.pathname);
      if (!filePath) {
        res.writeHead(403);
        res.end("Forbidden");
        return;
      }
      sendFile(res, filePath);
      return;
    }

    renderPage(res, url);
  })
  .listen(port, () => {
    console.log(`Docsify SSR server running at http://localhost:${port}`);
  });
