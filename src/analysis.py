import re
from pathlib import Path


builtin = [
    'Prelude.hs',
    'Data/List.hs',
    'Data/Maybe.hs',
    'Control/Monad/State.hs'
]

example_dir = Path(__file__).parent.parent / 'tmp' / 'example'
hs_modules = [f for f in example_dir.rglob('*.hs') if f.relative_to(example_dir).as_posix() not in builtin]



def number_of_files():
    print("There are", len(hs_modules), "files in total")

def get_themes():
    for f in hs_modules:
        rel_name = f.relative_to(example_dir).as_posix()
        if rel_name not in builtin:
            print(rel_name)
            text = f.read_text()
            m = re.search(r'--\s*theme:\s*.*', text)
            print(m.group(0))

def oracles():
    oracles = []
    non_oracles = []
    for f in hs_modules:
        rel_name = f.relative_to(example_dir).as_posix()
        if rel_name not in builtin:
            text = f.read_text()
            m = re.search(r'--\s*oracle:\s*.*', text)
            oracle = m.group(0).split(':')[-1].strip()
            is_oracle = True if oracle == 'true' else False if oracle == 'false' else None
            if is_oracle:
                oracles.append(rel_name)
            else:
                non_oracles.append(rel_name)
    print(f"Oracle files: {len(oracles)}")
    print(f"None-Oracle files: {len(non_oracles)}")


if __name__ == "__main__":
    number_of_files()
    oracles()