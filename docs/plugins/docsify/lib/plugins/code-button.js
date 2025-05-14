(window.$docsify = window.$docsify || {}),
  (window.$docsify.plugins = [].concat(function (hook, vm) {
    // 新增行号样式
    const lineNumberStyles = `
       .code-line-numbers {
         position: absolute;
         left: 0;
         top: 2.15em;
         padding: var(--code-block-padding);
         border-right: 1px solid;
         text-align: right;
         user-select: none;
         line-height: 1.5;
       }
       pre[data-linenos] {
         position: relative;
         padding-left: 4em !important;
       }
       pre[data-linenos] code {
         counter-reset: linenumber;
       }
       pre[data-linenos] code > .line {
         counter-increment: linenumber;
       }
       pre[data-linenos] code > .line::before {
         content: counter(linenumber);
         display: inline-block;
         width: 2em;
         padding-right: 0.5em;
         text-align: right;
         user-select: none;
       }
     `;

    // 插入行号样式
    const style = document.createElement('style');
    style.textContent = lineNumberStyles;
    document.head.appendChild(style);

    function createButton(e, t) {
      const n = document.createElement("button");
      return (
        (n.className = "fa-solid " + e),
        (n.style.background = "none"),
        (n.style.border = "none"),
        (n.style.cursor = "pointer"),
        (n.style.color = "#666"),
        (n.style.fontSize = "14px"),
        n.addEventListener("click", t),
        n
      );
    }
    function showToast(e, t = !1) {
      const n = document.createElement("div");
      (n.textContent = e),
        (n.style.position = "fixed"),
        (n.style.bottom = "20px"),
        (n.style.left = "50%"),
        (n.style.transform = "translateX(-50%)"),
        (n.style.background = t ? "#ff4444" : "#4CAF50"),
        (n.style.color = "white"),
        (n.style.padding = "8px 16px"),
        (n.style.borderRadius = "20px"),
        (n.style.fontSize = "14px"),
        (n.style.boxShadow = "0 2px 4px rgba(0,0,0,0.2)"),
        document.body.appendChild(n),
        setTimeout(() => n.remove(), 1500);
    }
    function runHtmlCss(e) {
      const t = document.createElement("iframe");
      (t.srcdoc = e), showCodeResult(t);
    }
    function runJavaScript(code) {
      const result = eval(code);
      showCodeResult(result);
    }
    function runHtmlCss(e) {
      const t = document.createElement("iframe");
      (t.style.cssText =
        "width: 100%;height: 300px;border: 1px solid #e1e4e8;border-radius: 6px;background: white;margin: 10px 0;"),
        (t.srcdoc = `<!DOCTYPE html><html><head><style>body {padding: 20px;font-family: -apple-system, BlinkMacSystemFont, "Segoe UI", Roboto;line-height: 1.6; }</style></head><body>${e}</body></html>`),
        showCodeResult(t);
    }
    function showCodeResult(e) {
      const t = document.createElement("div");
      t.style.cssText =
        "position: fixed;top: 0;left: 0;width: 100%;height: 100%;background: rgba(0,0,0,0.5);display: flex;justify-content: center;align-items: center;backdrop-filter: blur(2px);z-index: 9999;";
      const n = document.createElement("div");
      n.style.cssText =
        "background: white;padding: 24px;border-radius: 12px;width: min(90%, 800px);max-height: 80vh;box-shadow: 0 8px 32px rgba(0,0,0,0.2);position: relative;";
      const o = document.createElement("div");
      o.style.cssText =
        "display: flex;justify-content: space-between;align-items: center;margin-bottom: 16px;padding-bottom: 8px;border-bottom: 1px solid #eee;";
      const s = document.createElement("h3");
      (s.textContent = "运行结果"),
        (s.style.cssText =
          "margin: 0;font-size: 18px;color: #333;\n    ");
      const c = document.createElement("button");
      (c.innerHTML = '<i class="fa-solid fa-xmark"></i>'),
        (c.style.cssText =
          "background: none;border: none;cursor: pointer;color: #666;font-size: 16px;padding: 4px;"),
        (c.onclick = () => t.remove()),
        o.append(s, c),
        n.append(o, e),
        t.append(n),
        document.body.appendChild(t);
    }

    // 新增行号生成函数
    function addLineNumbers(preElement, codeElement) {
      // 创建行号容器
      const lineNumbers = document.createElement('div');
      lineNumbers.className = 'code-line-numbers';

      // 计算行数
      const lines = codeElement.textContent.split('\n');
      const lineCount = lines.length;

      // 生成行号
      lineNumbers.innerHTML = Array.from(
        { length: lineCount },
        (_, i) => `<span>${i + 1}</span>`
      ).join('\n');

      // 添加行号到代码块
      preElement.insertBefore(lineNumbers, codeElement);

      // 为代码块添加data属性用于样式控制
      preElement.setAttribute('data-linenos', '');
    }

    hook.doneEach(function () {
      document.querySelectorAll("pre > code").forEach((e) => {
        const t = e.parentElement;

        // 新增行号添加逻辑
        if (!t.querySelector('.code-line-numbers')) {
          addLineNumbers(t, e);
        }

        if (t.querySelector(".code-buttons")) return;
        const n = (e.className.match(/language-(\w+)/) || e.className.match(/lang-(\w+)/) || [])[1],
          o = e.textContent,
          s = document.createElement("div");
        (s.className = "code-buttons"),
          (s.style.cssText =
            "  top: 8px;  right: 8px;  display: flex;  gap: 8px;  opacity: 0.6;  transition: opacity 0.2s;  z-index: 1;  background: rgba(255,255,255,0.8);  padding: 4px;\n    margin-left: -4.01em;   "),
          (t.style.position = "relative");
        const c = t.querySelector("::after");
        c && ((c.style.right = "auto"), (c.style.left = "8px")),
          t.addEventListener("mouseenter", () => (s.style.opacity = "1")),
          t.addEventListener("mouseleave", () => (s.style.opacity = "0.6"));
        const a = createButton("fa-copy", () => {
          navigator.clipboard.writeText(o), showToast("Copied!");
        }),
          d = createButton("fa-download", () => {
            const e = new Blob([o], { type: "text/plain" }),
              t = URL.createObjectURL(e),
              s = document.createElement("a");
            (s.href = t),
              (s.download = `code.${n || "txt"}`),
              s.click(),
              URL.revokeObjectURL(t);
          }),
          r = ["html", "css", "javascript", "js"];
        if ((s.append(d, a), r.includes(n))) {
          const e = createButton("fa-play", () => {
            try {
              ["html", "css"].includes(n)
                ? runHtmlCss(o)
                : ["javascript", "js"].includes(n) && runJavaScript(o);
            } catch (e) {
              showToast(`Error: ${e.message}`, !0);
            }
          });
          s.append(e);
        }
        t.prepend(s);
      });
    });
  }, window.$docsify.plugins));
