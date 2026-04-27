const fs = require("node:fs");
const http = require("node:http");
const path = require("node:path");

const docsDir = path.resolve(__dirname, "../docs");
const port = Number(process.env.PORT || 3000);

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

function getFilePath(urlPath) {
  const decodedPath = decodeURIComponent(urlPath);
  const pathname = decodedPath === "/" ? "/index.html" : decodedPath;
  const filePath = path.normalize(path.join(docsDir, pathname));
  return filePath === docsDir || filePath.startsWith(`${docsDir}${path.sep}`) ? filePath : null;
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

http
  .createServer((req, res) => {
    const url = new URL(req.url, `http://${req.headers.host || "localhost"}`);
    const filePath = getFilePath(url.pathname);

    if (!filePath) {
      res.writeHead(403);
      res.end("Forbidden");
      return;
    }

    sendFile(res, filePath);
  })
  .listen(port, () => {
    console.log(`Docsify static server running at http://localhost:${port}`);
  });
