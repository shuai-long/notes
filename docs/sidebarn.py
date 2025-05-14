import os
import sys
from urllib.parse import quote

EXCLUDE = ['_css', 'metadata', 'plugins', '_coverpage.md', '_navbar.md', '_sidebar.md', 'tags.md', 'readme.md']

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
        md_lines.append(f"{indent}-  {dir_name}")  # 添加空格前缀
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
        print("用法: python generate_readme.py <目标目录> [encode]")
        sys.exit(1)

    target_dir = sys.argv[1]
    encode_flag = len(sys.argv) > 2 and sys.argv[2].lower() in ['encode', 'true', '1']

    if not os.path.isdir(target_dir):
        print(f"错误: {target_dir} 不是有效目录")
        sys.exit(1)

    # 生成并保存文件
    with open(os.path.join(target_dir, '_sidebar.md'), 'w', encoding='utf-8') as f:
        f.write(generate_readme(target_dir, EXCLUDE, encode_flag))
    
    print(f"侧边栏已生成: {os.path.join(target_dir, '_sidebar.md')}")