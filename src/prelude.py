from pathlib import Path
from typing import Iterator

def generate_prelude() -> Iterator[str]:
    prelude = Path(__file__).parent.parent / 'prolog' / 'prelude.pl'
    text = prelude.read_text()
    for line in text.splitlines():
        if line.lstrip().startswith('%'):
            continue
        else:
            functor_index = line.find('(')
            offset = len('type_of_')
            functor = line[offset:functor_index]
            yield functor


if __name__ == '__main__':
    generate_prelude()
