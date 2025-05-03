// docsify-pre-class-plugin.js
const preClassPlugin = function(hook, vm) {
    hook.doneEach(() => {
        // 从 docsify 配置中获取类名配置
        const className = vm.config.preClass;

        if (className) {
            document.querySelectorAll('pre').forEach(pre => {
                // 保留原有类名并添加新类名
                pre.className = [pre.className, className]
                    .filter(Boolean)
                    .join(' ')
                    .trim();
            });
        }
    });
};

// 初始化配置
window.$docsify = window.$docsify || {};
window.$docsify.plugins = [preClassPlugin].concat(window.$docsify.plugins || []);