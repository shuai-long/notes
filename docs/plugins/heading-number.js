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
    var minLevel;
    var counters = [0, 0, 0, 0, 0, 0, 0];

    if (!section) return;

    removeExisting(section);
    headings = Array.prototype.slice.call(section.querySelectorAll(config.selector));

    if (!headings.length) return;

    minLevel = headings.reduce(function (level, heading) {
      return Math.min(level, getHeadingLevel(heading));
    }, 6);

    headings.forEach(function (heading) {
      var level = getHeadingLevel(heading);
      var numberText;

      counters[level] += 1;

      for (var fillLevel = minLevel; fillLevel < level; fillLevel += 1) {
        if (counters[fillLevel] === 0) counters[fillLevel] = 1;
      }

      for (var resetLevel = level + 1; resetLevel <= 6; resetLevel += 1) {
        counters[resetLevel] = 0;
      }

      numberText = counters.slice(minLevel, level + 1).join(".") + ".";
      insertNumber(heading, numberText);
    });
  }

  window.$docsify = window.$docsify || {};
  window.$docsify.plugins = (window.$docsify.plugins || []).concat(function (hook) {
    hook.doneEach(function () {
      window.requestAnimationFrame(numberHeadings);
    });
  });
})();
