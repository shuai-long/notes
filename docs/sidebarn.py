import os
from urllib.parse import quote

EXCLUDE = ['_css', 'metadata', 'plugins', '_coverpage.md', '_navbar.md', '_sidebar.md', 'tags.md', 'readme.md']

def is_hidden(path):
    """Check if the file/directory is hidden"""
    name = os.path.basename(path)
    if os.name == 'nt':
        try:
            attrs = os.stat(path).st_file_attributes
            if attrs & 2:
                return True
        except AttributeError:
            pass
    return name.startswith('.')

def generate_readme(root_path, exclude):
    """Generate Markdown content with headings"""
    md_lines = ['<!-- _sidebar.md -->']
    exclude_lower = [e.lower() for e in exclude]
    process_directory(root_path, root_path, exclude_lower, 1, md_lines)
    return '\n'.join(md_lines)

def process_directory(root_dir, current_dir, exclude_lower, level, md_lines):
    """Recursively process directories and files"""
    try:
        items = os.listdir(current_dir)
    except PermissionError:
        return

    filtered = []
    for item in items:
        full_path = os.path.join(current_dir, item)
        if item.lower() in exclude_lower or is_hidden(full_path):
            continue
        if os.path.isdir(full_path):
            filtered.append((item, 'dir'))
        elif os.path.isfile(full_path) and item.lower().endswith('.md'):
            filtered.append((item, 'file'))

    files = sorted(
        [i[0] for i in filtered if i[1] == 'file'],
        key=lambda x: x.lower()
    )
    dirs = sorted(
        [i[0] for i in filtered if i[1] == 'dir'],
        key=lambda x: x.lower()
    )

    # Process files first
    for file_name in files:
        rel_dir = os.path.relpath(current_dir, root_dir)
        rel_dir = '' if rel_dir == '.' else rel_dir
        raw_link = os.path.join('/', rel_dir, file_name).replace('\\', '/')
        parts = raw_link.split('/')
        encoded_parts = [quote(part) for part in parts if part]
        encoded_link = '/' + '/'.join(encoded_parts)
        display_name = os.path.splitext(file_name)[0]
        md_lines.append(f"[{display_name}]({encoded_link})")

    # Process directories
    for dir_name in dirs:
        dir_path = os.path.join(current_dir, dir_name)
        header = f"{'#' * level} {dir_name}"
        start_len = len(md_lines)
        md_lines.append(header)
        process_directory(root_dir, dir_path, exclude_lower, level + 1, md_lines)
        if len(md_lines) == start_len + 1:
            del md_lines[start_len]

if __name__ == '__main__':
    import sys
    if len(sys.argv) < 2:
        print("Usage: python generate_readme.py <target_directory>")
        sys.exit(1)

    target_dir = sys.argv[1]
    if not os.path.isdir(target_dir):
        print(f"Error: {target_dir} is not a valid directory")
        sys.exit(1)

    readme_content = generate_readme(target_dir, EXCLUDE)
    readme_path = os.path.join(target_dir, '_sidebar.md')
    
    with open(readme_path, 'w', encoding='utf-8') as f:
        f.write(readme_content)
    
    print(f"Successfully generated at: {readme_path}")