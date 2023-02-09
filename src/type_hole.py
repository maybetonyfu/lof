from pydantic import BaseModel
from src.haskell import Span
from subprocess import run
from pathlib import Path
from os import linesep
import re



def sort_loc(locs: list[Replace]):
    return sorted(sorted(locs, key=lambda rep: rep.slice[0][1], reverse=True), key=lambda rep: rep.slice[0][0], reverse=True)





    def set_original(self, text: str):
        self.original = text

    def set_type_signature(self, sig: str):
        self.signature = sig

    def set_kind(self, kind: str):
        self.kind = kind


class TypeHoles:
    def __init__(self, replaces: list[Replace], file: str):
        self.project_dir = Path(__file__).parent.parent
        self.target_file = self.project_dir / "example" / file
        self.original_file_content = self.target_file.read_text()

        self.holes = []
        for i, loc in enumerate(sort_loc(replaces)):
            self.holes.append(Hole(original="", hole_id=i, loc=loc.slice, signature="", kind="", error_id=loc.error_id))

        self.replaced_file_content = ""

    def insert_type_holes(self):
        text = self.original_file_content
        for hole in self.holes:
            lines = text.splitlines()
            from_line = hole.loc[0][0] - 1
            from_col = hole.loc[0][1] - 1
            to_col = hole.loc[1][1] - 1
            for line_number, line in enumerate(lines):
                if line_number == from_line:
                    old = line[from_col:to_col]
                    hole.set_original(old)
                    new = f'_{hole.hole_id}'
                    new_line = line[:from_col] + new + line[to_col:]
                    text = '\n'.join(lines[:line_number] + [new_line] + lines[line_number + 1:])
                else:
                    continue
        self.replaced_file_content = text
        # print(text)
        self.target_file.write_text(text)

    def run_ghc(self):
        result = run(['ghc', '-fno-code', '-fforce-recomp', '-main-is', '"undefined"', str(self.target_file)],
                     shell=True, capture_output=True)
        output = result.stderr.decode()
        # print(output)

        for hole in self.holes:
            needle = f'_{hole.hole_id}'
            reg_hole = re.compile(fr"Found hole: {needle} :: (.*)(\r\n?|\n)", flags=re.MULTILINE)
            match_groups = reg_hole.search(output)
            # print(match_groups.groups())
            if match_groups is None:
                reg_wildcard = re.compile(fr"Found type wildcard `{needle}' standing for `(.*)'", flags=re.MULTILINE)
                match_groups = reg_wildcard.search(output)
                if match_groups is None:
                    hole.set_type_signature("a different type expression")
                    hole.set_kind('Rename')
                else:
                    type_sig = match_groups.groups()[0].strip()
                    hole.set_type_signature(type_sig)
                    hole.set_kind('Type wildcard')

            else:
                type_sig = match_groups.groups()[0].strip()
                hole.set_type_signature(type_sig)
                hole.set_kind('Type hole')

    def reset_file_content(self):
        self.target_file.write_text(self.original_file_content)

    def execute(self) -> list[Hole]:
        self.insert_type_holes()
        self.run_ghc()
        self.reset_file_content()
        return self.holes


if __name__ == "__main__":
    span1 = Replace(error_id=1, slice=((1, 6), (1, 10)))
    # span2 = ((2, 5), (2, 6))
    typehole = TypeHoles(replaces=[span1], file="Test.hs")
    typehole.execute()
    print(typehole.holes)
