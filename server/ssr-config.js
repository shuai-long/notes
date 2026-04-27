module.exports = {
  name: "notes",
  repo: "https://github.com/shuai-long/notes",
  basePath: "/",
  maxLevel: 6,
  loadSidebar: true,
  subMaxLevel: 4,
  loadNavbar: true,
  coverpage: false,
  autoHeader: false,
  onlyCover: false,
  auto2top: true,
  "flexible-alerts": {
    style: "flat",
  },
  mermaidConfig: {
    querySelector: ".mermaid",
  },
  mermaidZoom: {
    minimumScale: 1,
    maximumScale: 5,
    zoomPannel: true,
  },
  hideCode: {
    scroll: true,
    height: 500,
    minLines: 28,
  },
  pagination: {
    previousText: "Pre",
    nextText: "Next",
    crossChapter: true,
    crossChapterText: true,
  },
  themeable: {
    readyTransition: true,
    responsiveTables: false,
  },
  prism: {
    copySuccessText: "复制成功",
    maxMatchBracesChars: 12000,
    showLanguage: {
      useDataAttribute: true,
      mapping: {
        js: "JavaScript",
        py: "Python",
        sh: "Shell",
      },
    },
  },
  codeButton: {
    lineNumbers: true,
    maxLineNumbers: 500,
  },
  spacing: {
    selector: ".markdown-section",
  },
  dashboard: {
    numTabContent: 3,
    metadataUrl: "metadata/posts",
    sort: false,
    theme: "default",
    tagboardTheme: "default",
  },
  requestHeaders: {
    "cache-control": "max-age=600",
  },
  search: {
    maxAge: 86400000,
    paths: "auto",
    placeholder: "搜索",
    noData: "No Results",
    depth: 4,
    hideOtherSidebarContent: false,
    namespace: "Docsify-Guide",
  },
};
