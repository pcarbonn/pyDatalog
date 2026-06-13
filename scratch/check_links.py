import re
import urllib.request
import urllib.error
from pathlib import Path
import json

def extract_links(content):
    # Match markdown links: [text](link)
    markdown_links = re.findall(r'\[[^\]]+\]\(([^)]+)\)', content)
    # Also match raw urls in text
    raw_urls = re.findall(r'(https?://[^\s"\'\)\]]+)', content)
    return set(markdown_links + raw_urls)

def check_url(url):
    # Only verify github.com links to avoid hitting restricted external domains
    if "github.com" not in url:
        return "Skipped (External Domain)"
    
    url_clean = url.split('#')[0].strip()
    try:
        req = urllib.request.Request(
            url_clean, 
            headers={'User-Agent': 'Mozilla/5.0'}
        )
        with urllib.request.urlopen(req, timeout=5) as response:
            return f"OK ({response.status})"
    except urllib.error.HTTPError as e:
        return f"Failed ({e.code})"
    except Exception as e:
        return f"Error ({e})"

docs_dir = Path("/home/pcarbonn/Documents/repos/pyDatalog/docs")
all_files = list(docs_dir.glob("**/*.ipynb")) + list(docs_dir.glob("**/*.md"))

print("Starting Link Check...\n")

for f in all_files:
    if f.suffix == '.ipynb':
        try:
            with open(f, 'r', encoding='utf-8') as file:
                data = json.load(file)
            content = ""
            for cell in data.get('cells', []):
                if cell.get('cell_type') == 'markdown':
                    content += "".join(cell.get('source', [])) + "\n"
        except Exception as e:
            print(f"Error reading {f.name}: {e}")
            continue
    else:
        content = f.read_text(encoding='utf-8')
    
    links = extract_links(content)
    if not links:
        continue
        
    print(f"File: {f.relative_to(docs_dir.parent)}")
    for link in sorted(links):
        link = link.strip()
        if not link:
            continue
        if link.startswith('http://') or link.startswith('https://'):
            status = check_url(link)
            print(f"  - [Web] {link} -> {status}")
        else:
            # Anchor links within the same file or other files
            if link.startswith('#'):
                print(f"  - [Anchor] {link} -> Checked (Internal)")
            else:
                # Relative file check
                # Strip queries/anchors
                rel_path = link.split('#')[0].split('?')[0]
                target_file = f.parent / rel_path
                if target_file.exists():
                    print(f"  - [Local] {link} -> OK (Exists)")
                else:
                    print(f"  - [Local] {link} -> BROKEN (Not Found)")
    print()
