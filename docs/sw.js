const CACHE_VERSION = "notes-pwa-v4";
const PRECACHE = `${CACHE_VERSION}-precache`;
const RUNTIME = `${CACHE_VERSION}-runtime`;

const CDN_HOSTS = new Set([
  "cdnjs.cloudflare.com",
  "cdn.jsdelivr.net",
  "fonts.googleapis.com",
  "fonts.gstatic.com",
  "unpkg.com",
]);

const APP_SHELL = [
  "./",
  "./index.html",
  "./manifest.webmanifest",
  "./README.md",
  "./_coverpage.md",
  "./_navbar.md",
  "./_sidebar.md",
  "./_css/dashboard.min.css",
  "./_css/theme-custom.min.css",
  "./icons/pwa-icon.svg",
  "./plugins/docsify/lib/plugins/prismjs-class.js",
  "./plugins/docsify/lib/plugins/docsify-sidebar-collapse.min.js",
  "./plugins/docsify/lib/plugins/docsify-header-collapse.min.js",
  "./plugins/docsify/lib/plugins/pdfobject.min.js",
  "./plugins/docsify/lib/plugins/docsify-pdf-embed.min.js",
  "./plugins/docsify/lib/plugins/d3.min.js",
  "./plugins/docsify/lib/plugins/docsify-mermaid.min.js",
  "./plugins/docsify/lib/plugins/docsify-mermaid-zoom.min.js",
  "./plugins/docsify/lib/plugins/docsify-image-caption.min.js",
  "./plugins/docsify/lib/plugins/zoom-image.min.js",
  "./plugins/docsify/lib/plugins/search-lazy.js",
  "./plugins/docsify/lib/plugins/docsify-tabs.min.js",
  "./plugins/docsify/lib/plugins/docsify-tabs-fix.js",
  "./plugins/docsify/lib/plugins/docsify-dashboard.js",
  "./plugins/docsify/lib/plugins/code-button.js",
  "./plugins/docsify/lib/plugins/docsify-responsive-tables.js",
  "./plugins/docsify/lib/plugins/docsify-back-to-top.js",
  "./plugins/docsify/lib/plugins/docsify-hide-code.js",
  "./plugins/docsify/lib/plugins/docsify-inline-code-highing.js",
  "./plugins/docsify/lib/plugins/pangu.min.js",
  "./plugins/docsify/lib/plugins/docsify-spacing.js",
];

function isSameOrigin(url) {
  return url.origin === self.location.origin;
}

function isCacheableRequest(request) {
  if (request.method !== "GET") return false;
  const url = new URL(request.url);
  return isSameOrigin(url) || CDN_HOSTS.has(url.hostname);
}

function getCacheKey(request) {
  const url = new URL(request.url);
  if (isSameOrigin(url)) {
    url.searchParams.delete("cache-bust");
  }
  return new Request(url.href, { method: "GET" });
}

async function putCache(request, response) {
  if (!response || (!response.ok && response.type !== "opaque")) return;
  const cache = await caches.open(RUNTIME);
  await cache.put(getCacheKey(request), response.clone());
}

async function staleWhileRevalidate(request) {
  const cacheKey = getCacheKey(request);
  const cached = await caches.match(cacheKey);

  const fetched = fetch(request, { cache: "no-store" })
    .then((response) => {
      putCache(request, response).catch(() => {});
      return response;
    })
    .catch(() => null);

  if (cached) return cached;
  return (await fetched) || Response.error();
}

async function networkFirst(request) {
  try {
    const response = await fetch(request, { cache: "no-store" });
    await putCache(request, response);
    return response;
  } catch (error) {
    const cached = await caches.match(getCacheKey(request));
    return cached || caches.match("./index.html");
  }
}

self.addEventListener("install", (event) => {
  event.waitUntil(
    caches
      .open(PRECACHE)
      .then((cache) => cache.addAll(APP_SHELL))
      .then(() => self.skipWaiting())
  );
});

self.addEventListener("activate", (event) => {
  event.waitUntil(
    caches
      .keys()
      .then((keys) =>
        Promise.all(
          keys
            .filter((key) => key !== PRECACHE && key !== RUNTIME)
            .map((key) => caches.delete(key))
        )
      )
      .then(() => self.clients.claim())
  );
});

self.addEventListener("fetch", (event) => {
  const { request } = event;
  if (!isCacheableRequest(request)) return;

  if (request.mode === "navigate") {
    event.respondWith(networkFirst(request));
    return;
  }

  event.respondWith(staleWhileRevalidate(request));
});
