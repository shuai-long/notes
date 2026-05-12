import difflib
import os
import subprocess
import sys
from pathlib import Path
from urllib.parse import quote

EXCLUDE = ['_css', 'metadata', 'plugins', '_coverpage.md', '_navbar.md', '_sidebar.md', 'tags.md', 'readme.md', '_glossary.md']

def read_lines(path):
    """读取文本文件快照"""
    path = Path(path)

    if not path.is_file():
        return None

    return path.read_text(encoding='utf-8').splitlines()

def print_group(title, lines):
    if not lines:
        return

    print(f"  {title}:")
    for line in lines:
        print(f"    {line}")

def print_file_diff(label, before, after):
    """按新增、修改、删除输出文件差异"""
    added = []
    modified = []
    deleted = []

    before = before or []
    after = after or []

    for tag, i1, i2, j1, j2 in difflib.SequenceMatcher(a=before, b=after).get_opcodes():
        if tag == 'equal':
            continue

        if tag == 'insert':
            added.extend(after[j1:j2])
            continue

        if tag == 'delete':
            deleted.extend(before[i1:i2])
            continue

        if tag == 'replace':
            old_lines = before[i1:i2]
            new_lines = after[j1:j2]
            pair_count = min(len(old_lines), len(new_lines))

            for index in range(pair_count):
                modified.append(f"{old_lines[index]} -> {new_lines[index]}")

            deleted.extend(old_lines[pair_count:])
            added.extend(new_lines[pair_count:])

    print(f"\n{label} 差异:")
    if not added and not modified and not deleted:
        print("  无变化")
        return

    print_group("新增", added)
    print_group("修改", modified)
    print_group("删除", deleted)

def update_pwa_cache_manifest():
    """调用项目脚本刷新 PWA 缓存清单"""
    repo_root = Path(__file__).resolve().parent.parent
    script_path = repo_root / 'scripts' / 'build-pwa-manifest.mjs'

    if not script_path.is_file():
        print(f"错误: 未找到 PWA 缓存清单生成脚本: {script_path}")
        sys.exit(1)

    try:
        subprocess.run(['node', str(script_path)], cwd=repo_root, check=True)
    except FileNotFoundError:
        print("错误: 未找到 node，无法更新 pwa-cache-manifest.json")
        sys.exit(1)
    except subprocess.CalledProcessError as error:
        print(f"错误: 更新 pwa-cache-manifest.json 失败，退出码: {error.returncode}")
        sys.exit(error.returncode)

def is_hidden(path):
    """判断隐藏文件/目录（跨平台支持）"""
    name = os.path.basename(path)
    if os.name == 'nt':
        try:
            attrs = os.stat(path).st_file_attributes
            return attrs & 2  # FILE_ATTRIBUTE_HIDDEN
        except AttributeError:
            pass
    return name.startswith('.')

def generate_readme(root_path, exclude, encode_flag):
    """生成目录结构的Markdown内容"""
    md_lines = ['<!-- _sidebar.md -->']
    exclude_lower = [e.lower() for e in exclude]
    process_directory(root_path, root_path, exclude_lower, 1, md_lines, encode_flag)
    return '\n'.join(md_lines)

def process_directory(root_dir, current_dir, exclude_lower, level, md_lines, encode_flag):
    """递归处理目录结构"""
    try:
        items = os.listdir(current_dir)
    except PermissionError:
        return

    # 分类和过滤项目
    dirs, files = [], []
    for item in items:
        full_path = os.path.join(current_dir, item)
        if item.lower() in exclude_lower or is_hidden(full_path):
            continue
        
        if os.path.isdir(full_path):
            dirs.append(item)
        elif os.path.isfile(full_path) and item.lower().endswith('.md'):
            files.append(item)

    # 处理目录（添加空格前缀）
    for dir_name in sorted(dirs, key=str.lower):
        dir_path = os.path.join(current_dir, dir_name)
        indent = '  ' * (level - 1)
        start_len = len(md_lines)
        md_lines.append(f"{indent}- \u00A0{dir_name}\u00A0")  # 添加空格前缀
        process_directory(root_dir, dir_path, exclude_lower, level + 1, md_lines, encode_flag)
        if len(md_lines) == start_len + 1:  # 空目录处理
            del md_lines[-1]

    # 处理文件（条件转码）
    for file_name in sorted(files, key=str.lower):
        rel_path = os.path.relpath(current_dir, root_dir)
        rel_path = '' if rel_path == '.' else rel_path
        
        # 构建链接路径
        raw_parts = ['', *rel_path.split(os.sep), file_name] if rel_path else ['', file_name]
        encoded_parts = []
        
        for part in raw_parts:
            if not part:
                continue
            if encode_flag:
                encoded = quote(part)
            else:
                encoded = part.replace(' ', '%20')  # 强制转码空格
            encoded_parts.append(encoded)
        
        encoded_link = '/'.join(encoded_parts)
        indent = '  ' * (level - 1)
        display_name = os.path.splitext(file_name)[0]
        md_lines.append(f"{indent}- [{display_name}]({encoded_link})")

if __name__ == '__main__':
    if len(sys.argv) < 2:
        print("用法: python sidebarn.py <目标目录> [encode]")
        sys.exit(1)

    target_dir = sys.argv[1]
    encode_flag = len(sys.argv) > 2 and sys.argv[2].lower() in ['encode', 'true', '1']
    sidebar_path = Path(target_dir) / '_sidebar.md'
    pwa_manifest_path = Path(__file__).resolve().parent / 'pwa-cache-manifest.json'

    if not os.path.isdir(target_dir):
        print(f"错误: {target_dir} 不是有效目录")
        sys.exit(1)

    sidebar_before = read_lines(sidebar_path)
    pwa_manifest_before = read_lines(pwa_manifest_path)

    # 生成并保存文件
    with open(sidebar_path, 'w', encoding='utf-8') as f:
        f.write(generate_readme(target_dir, EXCLUDE, encode_flag))
    
    print(f"侧边栏已生成: {sidebar_path}")
    update_pwa_cache_manifest()
    print_file_diff('_sidebar.md', sidebar_before, read_lines(sidebar_path))
    print_file_diff('pwa-cache-manifest.json（由 scripts/build-pwa-manifest.mjs 更新）', pwa_manifest_before, read_lines(pwa_manifest_path))
