import { createHash } from "node:crypto";
import { readdir, stat, writeFile } from "node:fs/promises";
import path from "node:path";

const root = process.cwd();
const docsDir = path.join(root, "docs");
const output = path.join(docsDir, "pwa-cache-manifest.json");
const includedExtensions = new Set([
  ".css",
  ".gif",
  ".html",
  ".ico",
  ".jpeg",
  ".jpg",
  ".js",
  ".json",
  ".map",
  ".md",
  ".png",
  ".pdf",
  ".svg",
  ".webmanifest",
  ".webp",
  ".woff",
  ".woff2",
]);
const excludedNames = new Set([".DS_Store", "index_old.html", "pwa-cache-manifest.json", "sw.js"]);
const excludedDirs = new Set([".git"]);

function toWebPath(file) {
  return "./" + path.relative(docsDir, file).split(path.sep).join("/");
}

async function walk(dir, result) {
  const entries = await readdir(dir, { withFileTypes: true });

  for (const entry of entries) {
    const fullPath = path.join(dir, entry.name);

    if (entry.isDirectory()) {
      if (!excludedDirs.has(entry.name)) {
        await walk(fullPath, result);
      }

      continue;
    }

    if (!entry.isFile()) continue;
    if (excludedNames.has(entry.name)) continue;
    if (!includedExtensions.has(path.extname(entry.name).toLowerCase())) continue;

    result.push(fullPath);
  }
}

const files = [];
await walk(docsDir, files);

files.sort(function (a, b) {
  return toWebPath(a).localeCompare(toWebPath(b));
});

const hash = createHash("sha256");
const manifestFiles = [];

for (const file of files) {
  const fileStat = await stat(file);
  const webPath = toWebPath(file);

  hash.update(webPath);
  hash.update(String(fileStat.size));
  hash.update(String(Math.floor(fileStat.mtimeMs)));
  manifestFiles.push(webPath);
}

const manifest = {
  version: hash.digest("hex").slice(0, 16),
  files: manifestFiles,
};

await writeFile(output, JSON.stringify(manifest, null, 2) + "\n");
console.log(`Wrote ${path.relative(root, output)} with ${manifestFiles.length} files.`);
