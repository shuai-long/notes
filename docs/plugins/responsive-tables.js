(function () {
  var DEFAULT_CONFIG = {
    longTextLength: 18,
    selector: ".markdown-section table",
  };
  var config = Object.assign({}, DEFAULT_CONFIG, (window.$docsify && window.$docsify.responsiveTables) || {});

  function normalizeText(text) {
    return (text || "").replace(/\s+/g, " ").trim();
  }

  function getHeaders(table) {
    var firstRow = table.querySelector("thead tr") || table.querySelector("tr");

    if (!firstRow) return [];

    return Array.prototype.slice.call(firstRow.children).map(function (cell) {
      return normalizeText(cell.textContent);
    });
  }

  function getVisualLength(text) {
    var length = 0;

    normalizeText(text).split("").forEach(function (char) {
      length += /[\u2E80-\u2EFF\u2F00-\u2FDF\u3040-\u30FF\u3100-\u312F\u31A0-\u31BF\u3400-\u4DBF\u4E00-\u9FFF\uF900-\uFAFF]/.test(char) ? 2 : 1;
    });

    return length;
  }

  function getColumnLengths(table) {
    var lengths = [];

    table.querySelectorAll("tbody tr").forEach(function (row) {
      Array.prototype.slice.call(row.children).forEach(function (cell, index) {
        lengths[index] = Math.max(lengths[index] || 0, getVisualLength(cell.textContent));
      });
    });

    return lengths;
  }

  function markCellAlignment(cell, isLong) {
    cell.classList.toggle("table-cell-long", isLong);
    cell.classList.toggle("table-cell-short", !isLong);
  }

  function wrapCellContent(cell) {
    var wrapper = cell.querySelector(":scope > .responsive-table-value");
    var fragment;

    if (wrapper) return;

    wrapper = document.createElement("span");
    wrapper.className = "responsive-table-value";
    fragment = document.createDocumentFragment();

    while (cell.firstChild) {
      fragment.appendChild(cell.firstChild);
    }

    wrapper.appendChild(fragment);
    cell.appendChild(wrapper);
  }

  function enhanceTable(table) {
    var headers = getHeaders(table);
    var columnLengths = getColumnLengths(table);
    var longTextLength = Number(config.longTextLength || DEFAULT_CONFIG.longTextLength);

    if (!headers.length) return;

    table.classList.add("responsive-table");
    table.querySelectorAll("th").forEach(function (cell) {
      cell.classList.add("table-heading-cell");
    });
    table.querySelectorAll("tbody tr").forEach(function (row) {
      Array.prototype.slice.call(row.children).forEach(function (cell, index) {
        cell.setAttribute("data-label", headers[index] || "");
        markCellAlignment(cell, (columnLengths[index] || 0) > longTextLength);
        wrapCellContent(cell);
      });
    });
  }

  function enhanceAll() {
    document.querySelectorAll(config.selector).forEach(enhanceTable);
  }

  window.$docsify = window.$docsify || {};
  window.$docsify.plugins = (window.$docsify.plugins || []).concat(function (hook) {
    hook.doneEach(function () {
      window.requestAnimationFrame(enhanceAll);
    });
  });
})();
