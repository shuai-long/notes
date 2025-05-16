function plugin(t, e) {
  function n(t) {
    return Number.isNaN(t - "0")
      ? "&nbsp"
      : 8 !== t.length
        ? "&nbsp"
        : t.substring(0, 4) + "-" + t.substring(4, 6) + "-" + t.substring(6, 8);
  }
  function a() {
    p
      ? document.body.classList.add("force-close")
      : document.body.classList.remove("force-close");
  }
  function i() {
    p = !1;
  }
  function o(t, e) {
    return t.replace(l, g);
  }
  function s() {
    (pages = Array.from(
      document
        .getElementsByClassName("sidebar-nav")[0]
        .getElementsByTagName("a")
    )),
      pages.shift(),
      (pages = pages.filter((t) => {
        var e = !1;
        return (
          d.forEach((n) => {
            t.href.indexOf(n) > 0 && (e = !0);
          }),
          !e
        );
      })),
      pages.sort(
        (t, e) => (
          (aDate = t.href.substring(t.href.length - 8)),
          (bDate = e.href.substring(e.href.length - 8)),
          Number.isNaN(aDate - "0") && (aDate = "-1"),
          Number.isNaN(bDate - "0") && (bDate = "-1"),
          bDate - aDate
        )
      ),
      (v = pages),
      (m = Math.ceil(pages.length / u)),
      r();
  }
  function r() {
    (tocPageDiv = document.getElementsByClassName("toc-page-div")[0]),
      (tocPageDiv.innerHTML = ""),
      h < 1 && (h = 1),
      h > m && (h = m);
    let t = v.slice((h - 1) * u, h * u);
    t.forEach((t) => {
      (pageHref = t.href),
        (tmp = pageHref.replace("#/", "")),
        (pagePictureHref =
          tmp.substring(0, tmp.lastIndexOf("/")) +
          "/_media" +
          tmp.substring(tmp.lastIndexOf("/")) +
          "/cover-picture"),
        (pagePictureHref += ".jpg"),
        (pageHrefDiv =
          "<a class='toc-page-display-a' href=" +
          pageHref +
          "><div class='toc-page-display-div'><div class='toc-page-display-title-img'><img class='ignore-view-full-image-img' src='" +
          pagePictureHref +
          "' loading='lazy' onerror='this.src=\"_media/defaultImg/picture-2.gif\"'></div><div class='toc-page-display-title-div'>" +
          t.innerHTML +
          "</div><div class='toc-page-display-date-div'>" +
          n(tmp.substr(tmp.length - 8)) +
          "</div></div></a>"),
        (tocPageDiv.innerHTML += pageHrefDiv);
    }),
      (tocPaginatorInputDiv = document.getElementsByClassName(
        "toc-paginator-input"
      )),
      tocPaginatorInputDiv.length > 0 &&
      ((tocPaginatorInputDiv = tocPaginatorInputDiv[0]),
        tocPaginatorInputDiv.hasChildNodes() &&
        (tocPaginatorInputDiv.childNodes[0].value = h)),
      (document.scrollingElement.scrollTop = 0);
  }
  function c() {
    (tocPaginatorDiv = document.getElementsByClassName("toc-paginator-div")[0]),
      (tocPaginatorInputDiv = document.getElementsByClassName(
        "toc-paginator-input"
      )[0]),
      (tocPaginatorLeftButtonDiv = document.getElementsByClassName(
        "tocPaginatorLeftButtonDiv"
      )[0]),
      (tocPaginatorRightButtonDiv = document.getElementsByClassName(
        "tocPaginatorRightButtonDiv"
      )[0]),
      (tocPaginatorLeftButtonDiv.onclick = function (t) {
        h > 1 && ((h -= 1), r());
      }),
      (tocPaginatorRightButtonDiv.onclick = function (t) {
        h < m && ((h += 1), r());
      }),
      (tocPaginatorInputDiv.innerHTML =
        "<input class='tocPaginatorInputBox' type='number' value='" +
        h +
        "' min='1' max='" +
        m +
        "'></input><span>/</span><span>" +
        m +
        "</span>"),
      (tocPaginatorInput = tocPaginatorInputDiv.childNodes[0]),
      (tocPaginatorInput.onchange = function () {
        (h = this.value), r(), (this.value = h);
      });
  }
  const l = "<!-- toc -->",
    g =
      '<div class=\'toc-page-div\'></div><div class=\'toc-paginator-div\'><div class=\'tocPaginatorLeftButtonDiv toc-paginator-button-div\'><?xml version="1.0" encoding="UTF-8"?><svg width="20px" height="20px" stroke-width="1.5" viewBox="0 0 24 24" fill="none" xmlns="http://www.w3.org/2000/svg" color="var(--theme-color,#ea6f5a)"><path d="M15 6l-6 6 6 6" stroke="var(--theme-color,#ea6f5a)" stroke-width="1.5" stroke-linecap="round" stroke-linejoin="round"></path></svg></div><div class=\'toc-paginator-input\'></div><div class=\'tocPaginatorRightButtonDiv toc-paginator-button-div\'><?xml version="1.0" encoding="UTF-8"?><svg width="20px" height="20px" stroke-width="1.5" viewBox="0 0 24 24" fill="none" xmlns="http://www.w3.org/2000/svg" color="var(--theme-color,#ea6f5a)"><path d="M9 6l6 6-6 6" stroke="var(--theme-color,#ea6f5a)" stroke-width="1.5" stroke-linecap="round" stroke-linejoin="round"></path></svg></div></div>',
    d = ["README", "PersonalTen", "PersonalRecords"],
    u = 8;
  let p = !1,
    v = [],
    h = 1,
    m = 1;
  t.beforeEach(function (t) {
    return (p = t.includes(l)), p && (t = o(t, e)), t;
  }),
    t.doneEach(function () {
      p && (s(), c()), a(), i(), (document.scrollingElement.scrollTop = 0);
      let t = location.href.split("#")[1];
      "/" != t &&
        Array.from(
          document
            .getElementsByClassName("sidebar-nav")[0]
            .getElementsByTagName("a")
        ).some(
          (e) =>
            e.href.split("#")[1] === t &&
            (document.title != e.textContent &&
              (document.title = e.textContent),
              !0)
        );
    });
}

function dashboardPlugin(t, e) {
  function n(t) {
    return (
      t.sort((t, e) => {
        let n = t.time.replace(/\./g, "/"),
          a = e.time.replace(/\./g, "/"),
          i = new Date(n),
          o = new Date(a);
        return o - i;
      }),
      t
    );
  }

  function a(t, e) {
    return Object.keys(t).indexOf(e) > -1;
  }

  function i(t, e, n) {
    let a = "",
      i = 1;
    Math.ceil(t.length / e);
    for (let s = 0; s < t.length; s += e) {
      const r = t.slice(s, s + e);
      (a += `\n\n#### **${i}**\n\n<div class="toc-page-div">\n`),
        r.forEach((t) => {
          a += o(t, n);
        }),
        (a += "</div>\n\n"),
        i++;
    }
    return a;
  }

  function o(t, e) {
    d = a(t, "subtitle");
    let n = "";
    if (d)
      var { time: i, title: o, subtitle: s, tag: r, image: c, href: l } = t;
    else {
      var { time: i, title: o, tag: r, image: c, href: l } = t;
      "list" === e && ((s = "&nbsp;"), (d = !0));
    }

    // 处理标签为列表
    let tagsHtml = '';
    if (r) {
      const tags = Array.isArray(r) ? r : typeof r === 'string' ? r.split(' ⋅ ') : [];
      tagsHtml = `<div class="toc-tags-container"><ul style="list-style: none;padding: 0;margin-bottom: 0.5em;display: flex;gap: 8px;flex-wrap: wrap;">${tags.map(tag => `<li style="background: #f0f0f0;padding: 4px 8px;border-radius: 4px;font-size: calc(1rem * 0.9);margin-right: .5em;">${tag}</li>`).join('')}</ul></div>`;
    }

    // 修改后的HTML结构
    n += `<a class="toc-page-display-a" id="${e}" href="${l}"><div class="toc-page-display-div" id="${e}"><div class="toc-page-display-title-img" id="${e}"><center><img class="ignore-view-full-image-img" src="${c}"></center></div><div class="toc-content-wrapper" style="flex: 1;padding-left: 15px;"><div class="toc-title-time" style="display: flex;justify-content: flex-start;align-items: self-end;margin-bottom: 8px;"><div class="toc-page-title" style="font-size: calc(1rem *1.2);">${o}</div><div class="toc-page-time" style="padding-left: .5rem;padding-bottom: 0.2rem;color: #969391;font-size: calc(1rem *0.8);">${i}</div></div>${d ? `<div class="toc-page-subtitle">${s}</div>` : ''}${tagsHtml}</div></div></a>`;
    return n;
  }

  function s(t) {
    return g.filter((e) => !(!e || !e.tag) && e.tag.includes(t));
  }
  function r() {
    const t = [];
    return (
      g.forEach((e) => {
        "object" == typeof e.tag && e.tag.length > 0
          ? e.tag.forEach((e) => {
            t.includes(e) || t.push(e);
          })
          : "string" == typeof e.tag &&
          e.tag.length > 0 &&
          (t.includes(e.tag) || t.push(e.tag));
      }),
      t
    );
  }
  function c() {
    const t = window.location.href;
    if (
      ((hasTags = t.includes("tags")),
        (tagPageDiv = document.getElementsByClassName("markdown-section")[0]),
        !hasTags)
    )
      return;
    var e = t.split("?tag=")[1];
    e.includes("%20") && (e = e.replace(/%20/g, " "));
    const n = s(e);
    if (n && n.length > 0) {
      let t = `<h1>${e}</h1>\n<hr>`;
      t += '\n<div class="toc-page-div">\n';
      for (let e = 0; e < n.length; e++) t += o(n[e], m);
      (t += "\n</div>\n"), (tagPageDiv.innerHTML = t + tagPageDiv.innerHTML);
    } else
      (tagBoardContent = "<h1>Tags</h1>\n<hr>"),
        (tagBoardContent += `\n${tagList
          .map((t) => `<a href="#/tags?tag=${t}"target="_blank">${t}</a>`)
          .join(" | ")}\n`),
        (tagPageDiv.innerHTML = tagBoardContent + tagPageDiv.innerHTML);
    window.addEventListener("hashchange", () => {
      location.reload();
    });
  }
  let g,
    d = !1;
  const u = e.config.dashboard || {},
    v = u.sort || !1,
    h = u.theme || "default",
    m = u.tagboardTheme || "default",
    f = {},
    b = (t) => {
      if (f[t]) return f[t];
      let e = new XMLHttpRequest();
      e.open("GET", `${t}.json`, !1), e.send(null);
      const n = JSON.parse(e.response);
      return (f[t] = n);
    };
  t.beforeEach((t) => {
    const e = [],
      a = "___CODE_BLOCK_";
    t = t.replace(
      /```[\s\S]*?```/g,
      (t) => (e.push(t), `${a}${e.length - 1}___`)
    );
    const o = /<!--\s*dashboard:\s*([^\s]+)(?:\s+(.+?))?\s*-->/g;
    return (
      (t = t.replace(o, (t, e, a) => {
        try {
          const t = {};
          a &&
            a.split(/\s+/).forEach((e) => {
              const [n, a] = e.split("=");
              n && void 0 !== a && (t[n] = a);
            });
          const o = t.numTabContent
            ? parseInt(t.numTabContent, 10)
            : u.numTabContent || 3,
            s = t.theme || h;
          let r = b(e);
          return v && (r = n(r)), i(r, o, s);
        } catch (t) {
          return (
            console.error(`Failed to process ${e}:`, t),
            "<!-- Dashboard load failed -->"
          );
        }
      })),
      (t = t.replace(
        new RegExp(`${a}(\\d+)___`, "g"),
        (t, n) => e[parseInt(n, 10)]
      )),
      t
    );
  }),
    t.doneEach(() => {
      c();
    });
}
window.$docsify.plugins = [].concat(
  [plugin, dashboardPlugin],
  window.$docsify.plugins
);
