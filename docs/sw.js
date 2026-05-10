const CACHE_PREFIX = "notes-docs-";
const MANIFEST_URL = "./pwa-cache-manifest.json";
const CORE_ASSETS = ["./", "./index.html", "./README.md", "./_sidebar.md", "./manifest.webmanifest"];

let cacheNamePromise = null;

function toScopedUrl(url) {
  return new URL(url, self.registration.scope).href;
}

async function readPrecacheManifest() {
  try {
    const response = await fetch(toScopedUrl(MANIFEST_URL), { cache: "no-store" });

    if (!response.ok) throw new Error("manifest request failed");

    return response.json();
  } catch (error) {
    return {
      version: "fallback",
      files: CORE_ASSETS,
    };
  }
}

async function getCacheName() {
  if (!cacheNamePromise) {
    cacheNamePromise = readPrecacheManifest().then(function (manifest) {
      return CACHE_PREFIX + (manifest.version || "fallback");
    });
  }

  return cacheNamePromise;
}

async function openCurrentCache() {
  return caches.open(await getCacheName());
}

async function precache() {
  const manifest = await readPrecacheManifest();
  const cache = await openCurrentCache();
  const files = Array.from(new Set(CORE_ASSETS.concat(manifest.files || [])));

  await Promise.all(
    files.map(function (file) {
      return cache.add(toScopedUrl(file)).catch(function () {
        return null;
      });
    })
  );
}

function isSameOrigin(request) {
  return new URL(request.url).origin === self.location.origin;
}

function isNavigationRequest(request) {
  return request.mode === "navigate" || (request.headers.get("accept") || "").indexOf("text/html") >= 0;
}

function isDocumentData(url) {
  return /\.(?:md|html?|json|webmanifest)$/i.test(url.pathname);
}

function isStaticAsset(url) {
  return /\.(?:css|js|mjs|svg|png|jpe?g|gif|webp|ico|woff2?|ttf|map)$/i.test(url.pathname);
}

function isCriticalAsset(url) {
  return /\.(?:css|js|mjs)$/i.test(url.pathname);
}

async function networkFirst(request, fallbackToIndex) {
  const cache = await openCurrentCache();

  try {
    const response = await fetch(request, { cache: "no-store" });

    if (response && response.ok) {
      cache.put(request, response.clone());
    }

    return response;
  } catch (error) {
    return (
      (await cache.match(request)) ||
      (await cache.match(request, { ignoreSearch: true })) ||
      (fallbackToIndex ? await cache.match(toScopedUrl("./index.html")) : null) ||
      Response.error()
    );
  }
}

async function staleWhileRevalidate(request) {
  const cache = await openCurrentCache();
  const cached = await cache.match(request);
  const update = fetch(request)
    .then(function (response) {
      if (response && response.ok) {
        cache.put(request, response.clone());
      }

      return response;
    })
    .catch(function () {
      return null;
    });

  return cached || update || Response.error();
}

self.addEventListener("install", function (event) {
  event.waitUntil(
    precache().then(function () {
      return self.skipWaiting();
    })
  );
});

self.addEventListener("activate", function (event) {
  event.waitUntil(
    getCacheName()
      .then(function (currentCacheName) {
        return caches.keys().then(function (keys) {
          return Promise.all(
            keys.map(function (key) {
              if (key.indexOf(CACHE_PREFIX) === 0 && key !== currentCacheName) {
                return caches.delete(key);
              }

              return null;
            })
          );
        });
      })
      .then(function () {
        return self.clients.claim();
      })
  );
});

self.addEventListener("fetch", function (event) {
  const request = event.request;
  const url = new URL(request.url);

  if (request.method !== "GET" || !isSameOrigin(request)) return;

  if (isNavigationRequest(request)) {
    event.respondWith(networkFirst(request, true));
    return;
  }

  if (isDocumentData(url)) {
    event.respondWith(networkFirst(request, false));
    return;
  }

  if (isCriticalAsset(url)) {
    event.respondWith(networkFirst(request, false));
    return;
  }

  if (isStaticAsset(url)) {
    event.respondWith(staleWhileRevalidate(request));
  }
});

self.addEventListener("message", function (event) {
  if (event.data && event.data.type === "SKIP_WAITING") {
    self.skipWaiting();
  }
});
