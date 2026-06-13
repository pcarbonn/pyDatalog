import json
from pathlib import Path

docs_dir = Path("/home/pcarbonn/Documents/repos/pyDatalog/docs")
all_files = list(docs_dir.glob("**/*.ipynb")) + list(docs_dir.glob("**/*.md"))

for f in all_files:
    if f.suffix == '.ipynb':
        try:
            with open(f, 'r', encoding='utf-8') as file:
                data = json.load(file)
            
            modified = False
            for cell in data.get('cells', []):
                if cell.get('cell_type') == 'markdown':
                    source = cell.get('source', [])
                    new_source = []
                    for line in source:
                        if 'pyDatalog/examples/' in line:
                            line = line.replace('pyDatalog/examples/', 'examples/')
                            modified = True
                        new_source.append(line)
                    cell['source'] = new_source
            
            if modified:
                with open(f, 'w', encoding='utf-8') as file:
                    json.dump(data, file, indent=1)
                print(f"Updated: {f.name}")
        except Exception as e:
            print(f"Error updating {f.name}: {e}")
    else:
        try:
            content = f.read_text(encoding='utf-8')
            if 'pyDatalog/examples/' in content:
                new_content = content.replace('pyDatalog/examples/', 'examples/')
                f.write_text(new_content, encoding='utf-8')
                print(f"Updated: {f.name}")
        except Exception as e:
            print(f"Error updating {f.name}: {e}")
