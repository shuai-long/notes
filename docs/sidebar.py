import os
from urllib.parse import quote

# 推荐全小写格式
EXCLUDE = ['_css', 'metadata', 'plugins', '_coverpage.md', '_navbar.md', '_sidebar.md', 'tags.md', 'readme.md', ]  

def is_hidden(path):
    """判断是否为隐藏文件/目录（支持跨平台）"""
    name = os.path.basename(path)
    # Windows隐藏属性检测
    if os.name == 'nt':
        try:
            attrs = os.stat(path).st_file_attributes
            if attrs & 2:  # FILE_ATTRIBUTE_HIDDEN
                return True
        except AttributeError:
            pass
    # Unix-like系统隐藏文件检测
    return name.startswith('.')

def generate_readme(root_path, exclude):
    """生成目录结构的Markdown内容"""
    md_lines = ['<!-- _sidebar.md -->']
    root_name = os.path.basename(root_path.rstrip(os.sep))
    
    # 预处理排除列表为小写
    exclude_lower = [e.lower() for e in exclude]
    process_directory(root_path, root_path, exclude_lower, 1, md_lines)
    return '\n'.join(md_lines)

def process_directory(root_dir, current_dir, exclude_lower, level, md_lines):
    """递归处理目录结构"""
    try:
        items = os.listdir(current_dir)
    except PermissionError:
        return

    filtered = []
    for item in items:
        full_path = os.path.join(current_dir, item)
        
        # 统一小写比较排除项
        if item.lower() in exclude_lower or is_hidden(full_path):
            continue
        
        # 保留目录或.md文件
        if os.path.isdir(full_path):
            # 在 process_directory 函数开头添加：
            print(f"正在扫描：{item}")
            filtered.append((item, 'dir'))
        elif os.path.isfile(full_path) and item.lower().endswith('.md'):
            print(f"正在扫描：{item}")
            filtered.append((item, 'file'))

    # 分类处理（目录优先）
    dirs = sorted(
        [i[0] for i in filtered if i[1] == 'dir'],
        key=lambda x: x.lower()
    )
    files = sorted(
        [i[0] for i in filtered if i[1] == 'file'],
        key=lambda x: x.lower()
    )

    # 处理目录结构
    for dir_name in dirs:
        dir_path = os.path.join(current_dir, dir_name)
        indent = '  ' * (level - 1)
        md_lines.append(f"{indent}- {dir_name}")
        process_directory(root_dir, dir_path, exclude_lower, level + 1, md_lines)

    # # 处理文件链接
    # for file_name in files:
    #     rel_dir = os.path.relpath(current_dir, root_dir)
    #     rel_dir = '' if rel_dir == '.' else rel_dir
        
    #     # 保留原始文件名大小写
    #     link_path = os.path.join('/', rel_dir, file_name).replace('\\', '/')
    #     indent = '  ' * (level - 1)
    #     display_name = os.path.splitext(file_name)[0]
    #     md_lines.append(f"{indent}- [{display_name}]({link_path})")
    
    # 处理文件链接
    for file_name in files:
        rel_dir = os.path.relpath(current_dir, root_dir)
        rel_dir = '' if rel_dir == '.' else rel_dir
        
        # 生成编码后的链接路径
        raw_link = os.path.join('/', rel_dir, file_name).replace('\\', '/')
        parts = raw_link.split('/')
        encoded_parts = [quote(part) for part in parts if part]  # 跳过空部分
        encoded_link = '/' + '/'.join(encoded_parts)
        
        indent = '  ' * (level - 1)
        display_name = os.path.splitext(file_name)[0]
        md_lines.append(f"{indent}- [{display_name}]({encoded_link})")

if __name__ == '__main__':
    import sys
    if len(sys.argv) < 2:
        print("Usage: python generate_readme.py <target_directory>")
        sys.exit(1)

    target_dir = sys.argv[1]
    if not os.path.isdir(target_dir):
        print(f"Error: {target_dir} is not a valid directory")
        sys.exit(1)

    # 生成并写入README
    readme_content = generate_readme(target_dir, EXCLUDE)
    readme_path = os.path.join(target_dir, '_sidebar.md')
    
    with open(readme_path, 'w', encoding='utf-8') as f:
        f.write(readme_content)
    
    print(f"Successfully generated at: {readme_path}")