from pathlib import Path
from platform import platform
from subprocess import run

import ujson

if __name__ == "__main__":
    to_check_file = "Test.hs"
    project_dir = Path(__file__).parent.parent
    base_dir = Path(__file__).parent.parent / "tmp" / "test"
    parser_bin = str(project_dir / "bin" / "haskell-parser.exe") if platform() == 'Windows' else str(
        project_dir / "bin" / "haskell-parser")
    result = run(f'{parser_bin} {base_dir}', shell=True, check=True, capture_output=True)
    parsed_data = ujson.loads(result.stdout)
    print(ujson.dumps([ f['ast'] for f in parsed_data['contents'] if f['file'] == "Test.hs"][0]))