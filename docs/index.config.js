module.exports = {
  template: './index.html',
  maxAge: 60 * 60 * 1000,
  config: {
    basePath: 'https://shuai-long.github.io/notes/',
    "files": [
      "docs"
    ],
    auto2top: true,
    coverpage: true,
    executeScript: true,
    loadSidebar: true,
    loadNavbar: true,
    mergeNavbar: true,
    maxLevel: 4,
    subMaxLevel: 2,
    name: 'docsify',
    search: {
      noData: {
        '/de-de/': 'Keine Ergebnisse!',
        '/zh-cn/': '没有结果!',
        '/': 'No results!'
      },
      paths: 'auto',
      placeholder: {
        '/de-de/': 'Suche',
        '/zh-cn/': '搜索',
        '/': 'Search'
      }
    }
  }
}