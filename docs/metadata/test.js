function plugin(t, e) { const n = "\x3c!-- toc --\x3e"; const a = '<div class=\'toc-page-div\'></div><div class=\'toc-paginator-div\'><div class=\'tocPaginatorLeftButtonDiv toc-paginator-button-div\'><?xml version="1.0" encoding="UTF-8"?><svg width="20px" height="20px" stroke-width="1.5" viewBox="0 0 24 24" fill="none" xmlns="http://www.w3.org/2000/svg" color="var(--theme-color,#ea6f5a)"><path d="M15 6l-6 6 6 6" stroke="var(--theme-color,#ea6f5a)" stroke-width="1.5" stroke-linecap="round" stroke-linejoin="round"></path></svg></div><div class=\'toc-paginator-input\'></div><div class=\'tocPaginatorRightButtonDiv toc-paginator-button-div\'><?xml version="1.0" encoding="UTF-8"?><svg width="20px" height="20px" stroke-width="1.5" viewBox="0 0 24 24" fill="none" xmlns="http://www.w3.org/2000/svg" color="var(--theme-color,#ea6f5a)"><path d="M9 6l6 6-6 6" stroke="var(--theme-color,#ea6f5a)" stroke-width="1.5" stroke-linecap="round" stroke-linejoin="round"></path></svg></div></div>'; const i = ["README", "PersonalTen", "PersonalRecords"]; const o = ["jpg", "gif", "png", "webp", "jpeg"]; const s = 8; let r = false; let l = []; let c = 1; let g = 1; function d(t, e) { return Math.floor(Math.random() * (e - t + 1)) + t } function u(t) { if (Number.isNaN(t - "0")) { return "&nbsp" } if (t.length !== 8) { return "&nbsp" } return t.substring(0, 4) + "-" + t.substring(4, 6) + "-" + t.substring(6, 8) } function f() { if (r) { document.body.classList.add("force-close") } else { document.body.classList.remove("force-close") } } function p(t) { var e = new XMLHttpRequest; e.open("HEAD", t, false); e.send(); return e.status != 404 } function v(n) { var a = ""; o.some(t => { var e = p(n + "." + t); if (e) { a = t } return e }); return a } function h() { r = false } function m(t, e) { return t.replace(n, a) } function b() { pages = Array.from(document.getElementsByClassName("sidebar-nav")[0].getElementsByTagName("a")); pages.shift(); pages = pages.filter(e => { var n = false; i.forEach(t => { if (e.href.indexOf(t) > 0) { n = true } }); return !n }); pages.sort((t, e) => { aDate = t.href.substring(t.href.length - 8); bDate = e.href.substring(e.href.length - 8); if (Number.isNaN(aDate - "0")) { aDate = "-1" } if (Number.isNaN(bDate - "0")) { bDate = "-1" } return bDate - aDate }); l = pages; g = Math.ceil(pages.length / s); D() } function D() { tocPageDiv = document.getElementsByClassName("toc-page-div")[0]; tocPageDiv.innerHTML = ""; if (c < 1) { c = 1 } if (c > g) { c = g } let t = l.slice((c - 1) * s, c * s); t.forEach(t => { pageHref = t.href; tmp = pageHref.replace("#/", ""); pagePictureHref = tmp.substring(0, tmp.lastIndexOf("/")) + "/_media" + tmp.substring(tmp.lastIndexOf("/")) + "/cover-picture"; pagePictureHref += "." + "jpg"; pageHrefDiv = "<a class='toc-page-display-a' href=" + pageHref + "><div class='toc-page-display-div'><div class='toc-page-display-title-img'><img class='ignore-view-full-image-img' src='" + pagePictureHref + "' loading='lazy' onerror='this.src=\"_media/defaultImg/picture-2.gif\"'></div><div class='toc-page-display-title-div'>" + t.innerHTML + "</div><div class='toc-page-display-date-div'>" + u(tmp.substr(tmp.length - 8)) + "</div></div></a>"; tocPageDiv.innerHTML += pageHrefDiv }); tocPaginatorInputDiv = document.getElementsByClassName("toc-paginator-input"); if (tocPaginatorInputDiv.length > 0) { tocPaginatorInputDiv = tocPaginatorInputDiv[0]; if (tocPaginatorInputDiv.hasChildNodes()) { tocPaginatorInputDiv.childNodes[0].value = c } } document.scrollingElement.scrollTop = 0 } function y() { tocPaginatorDiv = document.getElementsByClassName("toc-paginator-div")[0]; tocPaginatorInputDiv = document.getElementsByClassName("toc-paginator-input")[0]; tocPaginatorLeftButtonDiv = document.getElementsByClassName("tocPaginatorLeftButtonDiv")[0]; tocPaginatorRightButtonDiv = document.getElementsByClassName("tocPaginatorRightButtonDiv")[0]; tocPaginatorLeftButtonDiv.onclick = function (t) { if (c > 1) { c -= 1; D() } }; tocPaginatorRightButtonDiv.onclick = function (t) { if (c < g) { c += 1; D() } }; tocPaginatorInputDiv.innerHTML = "<input class='tocPaginatorInputBox' type='number' value='" + c + "' min='1' max='" + g + "'></input><span>/</span><span>" + g + "</span>"; tocPaginatorInput = tocPaginatorInputDiv.childNodes[0]; tocPaginatorInput.onchange = function () { c = this.value; D(); this.value = c } } t.beforeEach(function (t) { r = t.includes(n); if (r) { t = m(t, e) } return t }); t.doneEach(function () { if (r) { b(); y() } f(); h(); document.scrollingElement.scrollTop = 0; let e = location.href.split("#")[1]; if (e != "/") { Array.from(document.getElementsByClassName("sidebar-nav")[0].getElementsByTagName("a")).some(t => { if (t.href.split("#")[1] === e) { if (document.title != t.textContent) { document.title = t.textContent } return true } return false }) } }) }
function dashboardPlugin(t, e) {
  let a;
  let c = false;
  const n = e.config.dashboard || {};
  const i = n.numTabContent || 3;
  const o = n.metadataUrl || "metadata/posts";
  const s = n.sort || false;
  const r = n.theme || "default";
  const l = n.tagboardTheme || "default";
  const dataCache = {};

  const g = t => {
    if (dataCache[t]) return dataCache[t];
    let e = new XMLHttpRequest;
    e.open("GET", `${t}.json`, false);
    e.send(null);
    const data = JSON.parse(e.response);
    return dataCache[t] = data;
  };

  function d(t) {
    t.sort((t, e) => {
      let n = t.time.replace(/\./g, "/");
      let a = e.time.replace(/\./g, "/");
      let i = new Date(n);
      let o = new Date(a); return o - i
    }); return t
  }

  function u(t, e) {
    if (Object.keys(t).indexOf(e) > -1) {
      return true
    }
    return false
  }

  function generateDashboard(data, itemsPerPage, theme) {
    let html = '';
    let pageNumber = 1;
    const totalPages = Math.ceil(data.length / itemsPerPage);
    for (let i = 0; i < data.length; i += itemsPerPage) {
      const pageData = data.slice(i, i + itemsPerPage);
      html += `\n\n#### **${pageNumber}**\n\n<div class="toc-page-div">\n`;
      pageData.forEach(item => {
        html += f(item, theme);
      });
      html += `</div>\n\n`;
      pageNumber++;
    }
    return html;
  }

  function f(t, e) {
    c = u(t, "subtitle");
    let n = "";
    if (c) {
      var {
        time: a,
        title: i,
        subtitle: o,
        tag: s,
        image: r,
        href: l } = t
    }
    else {
      var {
        time: a,
        title: i,
        tag: s,
        image: r,
        href: l } = t;
      if (e === "list") {
        o = "&nbsp;";
        c = true
      }
    } if (Array.isArray(s)) {
      s = s.join(" ⋅ ")
    } n += `<a class="toc-page-display-a" id="${e}" href="${l}" target="_blank">
            <div class="toc-page-display-div" id="${e}">
                <div class="toc-page-display-title-img" id="${e}">
                    <center>
                        <img class="ignore-view-full-image-img" src="${r}">
                    </center>
                </div>
                <div class="toc-page-display-title-div" id="${e}">
                    ${i}
                </div>`; if (c) {
      n += `
                <div class="toc-page-display-subtitle-div" id="${e}">
                    ${o}
                </div>`} n += `
                <div class="toc-page-display-date-div" id="${e}">
                    ${a} &nbsp;&nbsp; ${s}
                </div>
            </div>
        </a>`; return n
  }
  function p(e) {
    return a.filter(t => {
      if (!t || !t.tag) return false;
      return t.tag.includes(e)
    })
  }
  function v() {
    const e = [];
    a.forEach(t => {
      if (typeof t.tag === "object" && t.tag.length > 0) {
        t.tag.forEach(t => { if (!e.includes(t)) { e.push(t) } })
      } else if (
        typeof t.tag === "string" && t.tag.length > 0) {
        if (!e.includes(t.tag)) {
          e.push(t.tag)
        }
      }
    }); return e
  }
  t.init(() => {
    try {
      a = g(o);
      if (s) { a = d(a) }
    }
    catch (t) {
      c
      onsole.error(`Failed to fetch ${o}.json.`, t)
    }
    tagList = v()
  });

  // 修改后的 beforeEach 处理
  t.beforeEach(content => {
    const dashboardRegExp = /<!--\s*dashboard:\s*([^\s]+)\s*-->/g;
    let match;

    while ((match = dashboardRegExp.exec(content)) !== null) {
      const [fullMatch, metadataUrl] = match;
      try {
        let data = g(metadataUrl);
        if (s) data = d(data);
        const dashboardHTML = generateDashboard(data, i, r);
        content = content.replace(fullMatch, dashboardHTML);
      } catch (err) {
        console.error(`Failed to process ${metadataUrl}:`, err);
        content = content.replace(fullMatch, "<!-- Dashboard load failed -->");
      }
    }
    return content;
  });

  function h() {
    const t = window.location.href;
    hasTags = t.includes("tags");
    tagPageDiv = document.getElementsByClassName("markdown-section")[0];
    if (!hasTags) {
      return
    }
    var n = t.split("?tag=")[1];
    if (n.includes("%20")) {
      n = n.replace(/%20/g, " ")
    }
    const a = p(n);
    if (a && a.length > 0) {
      let e = `<h1>${n}</h1>\n<hr>`;
      e += `\n<div class="toc-page-div">\n`;
      for (let t = 0; t < a.length; t++) {
        e += f(a[t], l)
      } e += `\n</div>\n`; tagPageDiv.innerHTML = e + tagPageDiv.innerHTML
    } else {
      tagBoardContent = `<h1>Tags</h1>\n<hr>`;
      tagBoardContent += `\n${tagList.map(t => `<a href="#/tags?tag=${t}"target="_blank">${t}</a>`).join(" | ")}\n`;
      tagPageDiv.innerHTML = tagBoardContent + tagPageDiv.innerHTML
    }
    window.addEventListener("hashchange", () => { location.reload() })
  }
  function m() {
    let e = document.querySelector(".sidebar-nav");
    if (e.innerHTML.includes("\x3c!-- tag-list --\x3e")) {
      let t = `<div class="tag-container"><div class="tag-list">`;
      t += `${tagList.map(t => `<a class="tag-element" href="#/tags?tag=${t}" target="_blank">${t}</a>`).join("\n")}`;
      t += `</div></div>`; e.innerHTML = e.innerHTML.replace("\x3c!-- tag-list --\x3e", t)
    }
  } t.doneEach(() => { h(); m() })
}
window.$docsify.plugins = [].concat([plugin, dashboardPlugin], window.$docsify.plugins);