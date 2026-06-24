(function () {
  var DEFAULT_CONFIG = {
    selector: "h1,h2,h3,h4,h5,h6",
  };
  var config = Object.assign({}, DEFAULT_CONFIG, (window.$docsify && window.$docsify.headingNumber) || {});

  function getHeadingLevel(heading) {
    return Number(heading.tagName.slice(1));
  }

  function getNumberTarget(heading) {
    return heading.querySelector("a.anchor") || heading;
  }

  function removeExisting(section) {
    section.querySelectorAll(".heading-number").forEach(function (number) {
      number.remove();
    });
  }

  function getHeadingText(heading) {
    var clone = heading.cloneNode(true);

    clone.querySelectorAll(".heading-number").forEach(function (number) {
      number.remove();
    });

    return (clone.textContent || "").replace(/\s+/g, " ").trim();
  }

  function normalizeNumberParts(value) {
    var parts = value.replace(/．/g, ".").split(".");

    if (!parts.length) return null;

    parts = parts.map(function (part) {
      return Number(part);
    });

    return parts.every(function (part) {
      return Number.isFinite(part);
    })
      ? parts
      : null;
  }

  function getContentNumberParts(heading) {
    var text = getHeadingText(heading);
    var match;

    match = text.match(/^\s*[（(]\s*(\d+)\s*[）)]/);
    if (match) return normalizeNumberParts(match[1]);

    match = text.match(/^\s*(\d+(?:[.．]\d+)+)(?=$|[^\d])/);
    if (match) return normalizeNumberParts(match[1]);

    match = text.match(/^\s*(\d+)\s*[.．、)）]/);
    if (match) return normalizeNumberParts(match[1]);

    return null;
  }

  function getMinLevel(headings) {
    return headings.reduce(function (level, heading) {
      return Math.min(level, getHeadingLevel(heading));
    }, 6);
  }

  function getBaseLevel(headingInfos, minLevel) {
    var baseCounts = {};
    var bestLevel = null;
    var bestCount = 0;

    headingInfos.forEach(function (info) {
      var baseLevel;

      if (!info.contentNumberParts) return;

      baseLevel = info.level - info.contentNumberParts.length + 1;
      if (baseLevel < 1 || baseLevel > 6) return;

      baseCounts[baseLevel] = (baseCounts[baseLevel] || 0) + 1;
    });

    Object.keys(baseCounts).forEach(function (level) {
      var numberLevel = Number(level);
      var count = baseCounts[level];

      if (count > bestCount || (count === bestCount && (bestLevel === null || numberLevel < bestLevel))) {
        bestLevel = numberLevel;
        bestCount = count;
      }
    });

    return bestLevel || minLevel;
  }

  function fillMissingAncestors(counters, baseLevel, level) {
    for (var fillLevel = baseLevel; fillLevel < level; fillLevel += 1) {
      if (counters[fillLevel] === 0) counters[fillLevel] = 1;
    }
  }

  function resetDescendants(counters, level) {
    for (var resetLevel = level + 1; resetLevel <= 6; resetLevel += 1) {
      counters[resetLevel] = 0;
    }
  }

  function getNumberPath(counters, baseLevel, level, parts) {
    var depth = level - baseLevel + 1;
    var numberParts = parts.slice();

    if (numberParts.length > depth) {
      numberParts = numberParts.slice(numberParts.length - depth);
    }

    if (numberParts.length < depth) {
      numberParts = counters.slice(baseLevel, level).map(function (part) {
        return part || 1;
      }).concat(numberParts);
    }

    return numberParts;
  }

  function applyContentNumber(counters, baseLevel, level, parts) {
    var numberParts = getNumberPath(counters, baseLevel, level, parts);

    numberParts.forEach(function (part, index) {
      counters[baseLevel + index] = part;
    });

    resetDescendants(counters, level);
  }

  function getNextMissingNumberPath(counters, baseLevel, level) {
    var nextCounters = counters.slice();

    fillMissingAncestors(nextCounters, baseLevel, level);
    nextCounters[level] += 1;

    return nextCounters.slice(baseLevel, level + 1);
  }

  function isBeforeNumberPath(path, targetPath) {
    var length = Math.min(path.length, targetPath.length);

    for (var index = 0; index < length; index += 1) {
      if (path[index] !== targetPath[index]) {
        return path[index] < targetPath[index];
      }
    }

    return false;
  }

  function formatNumberPath(path) {
    return path.join(".") + ".";
  }

  function generateMissingNumber(counters, baseLevel, level) {
    var numberPath;

    fillMissingAncestors(counters, baseLevel, level);
    counters[level] += 1;
    resetDescendants(counters, level);

    numberPath = counters.slice(baseLevel, level + 1);

    return formatNumberPath(numberPath);
  }

  function insertNumber(heading, numberText) {
    var target = getNumberTarget(heading);
    var number = document.createElement("span");

    number.className = "heading-number";
    number.setAttribute("aria-hidden", "true");
    number.textContent = numberText;
    target.insertBefore(number, target.firstChild);
  }

  function numberHeadings() {
    var section = document.querySelector(".markdown-section");
    var headings;
    var headingInfos;
    var minLevel;
    var baseLevel;
    var firstContentInfo;
    var firstContentPath;
    var hasSeenContentNumber = false;
    var counters = [0, 0, 0, 0, 0, 0, 0];

    if (!section) return;

    removeExisting(section);
    headings = Array.prototype.slice.call(section.querySelectorAll(config.selector));

    if (!headings.length) return;

    minLevel = getMinLevel(headings);
    headingInfos = headings.map(function (heading) {
      return {
        heading: heading,
        level: getHeadingLevel(heading),
        contentNumberParts: getContentNumberParts(heading),
      };
    });
    baseLevel = getBaseLevel(headingInfos, minLevel);
    firstContentInfo = headingInfos.find(function (info) {
      return info.level >= baseLevel && info.contentNumberParts;
    });
    firstContentPath = firstContentInfo
      ? getNumberPath(counters, baseLevel, firstContentInfo.level, firstContentInfo.contentNumberParts)
      : null;

    headingInfos.forEach(function (info) {
      var numberText;
      var nextNumberPath;

      if (info.level < baseLevel) return;

      if (info.contentNumberParts) {
        applyContentNumber(counters, baseLevel, info.level, info.contentNumberParts);
        hasSeenContentNumber = true;
        return;
      }

      if (!hasSeenContentNumber && firstContentPath) {
        nextNumberPath = getNextMissingNumberPath(counters, baseLevel, info.level);
        if (!isBeforeNumberPath(nextNumberPath, firstContentPath)) return;
      }

      numberText = generateMissingNumber(counters, baseLevel, info.level);
      insertNumber(info.heading, numberText);
    });
  }

  window.$docsify = window.$docsify || {};
  window.$docsify.plugins = (window.$docsify.plugins || []).concat(function (hook) {
    hook.doneEach(function () {
      window.requestAnimationFrame(numberHeadings);
    });
  });
})();
