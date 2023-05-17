from itertools import chain
from platform import platform
from subprocess import run
import base64
import ujson
from fastapi import FastAPI, Body
from fastapi.responses import HTMLResponse, PlainTextResponse
from fastapi.staticfiles import StaticFiles
from pathlib import Path
from src.haskell import System, Error, Rule, Diagnosis, gather_type_synonym
from pydantic import BaseModel
from src.prolog import Prolog, PlInterface
import shutil
from jinja2 import FileSystemLoader, Environment
app = FastAPI()

file_loader = FileSystemLoader('./src/templates')
env = Environment(loader=file_loader)

@app.post('/api/file/{file_path:path}', response_class=PlainTextResponse)
async def save_file(file_path: str, user_id: str, file_content: str = Body()):
    file = Path(__file__).parent.parent / "tmp" / user_id / file_path
    file.write_text(file_content)
    return 'OK'


app.mount("/static", StaticFiles(directory="./client/output"), name="static")

@app.get("/", response_class=HTMLResponse)
async def start():

    files = ['a.txt', 'hello.js']
    text = "hello world"
    return env.get_template('start.html').render(data={'files': files, "text": text, 'user_id': None})


@app.get("/editor/{user_id}", response_class=HTMLResponse)
async def editor_ls(user_id: str):
    source_dir = Path(__file__).parent.parent / "example"
    destination_dir = Path(__file__).parent.parent / "tmp" / user_id
    if not destination_dir.exists():
        (Path(__file__).parent.parent / "tmp").mkdir(exist_ok=True, parents=True)
        shutil.copytree(source_dir, destination_dir)
    files = destination_dir.rglob("*.hs")
    simplified_files = sorted([str(f.relative_to(destination_dir).as_posix()) for f in files])

    return env.get_template('start.html').render(data={
        'files': simplified_files,
        "text": None,
        'user_id': user_id,
        'opened_file': None,
        'diagnoses': [],
        'diagnoses_json': '[]'
    })


class Response(BaseModel):
    user_id: str
    diagnoses: list[Diagnosis]
    max_line_number: int
    max_col_number: int
    file: str
    file_list: list[str]
    text: str


@app.get("/editor/{user_id}/{file_path:path}", response_class=HTMLResponse)
async def editor(user_id: str, file_path: str):
    source_dir = Path(__file__).parent.parent / "example"
    destination_dir = Path(__file__).parent.parent / "tmp" / user_id
    if not destination_dir.exists():
        (Path(__file__).parent.parent / "tmp").mkdir(exist_ok=True, parents=True)
        shutil.copytree(source_dir, destination_dir)

    files = destination_dir.rglob("*.hs")
    simplified_files = sorted([str(f.relative_to(destination_dir).as_posix()) for f in files])

    text = (destination_dir / file_path).read_text()
    lines = text.splitlines()
    number_of_lines = len(lines)
    number_of_cols = max(*[len(l) for l in lines])

    project_dir = Path(__file__).parent.parent
    base_dir = project_dir / "tmp" / user_id
    parser_bin = str(project_dir / "bin" / "haskell-parser.exe") if platform() == 'Windows' else str(
        project_dir / "bin" / "haskell-parser")

    result = run(f'{parser_bin} {base_dir}', shell=True, check=True, capture_output=True)
    parsed_data = ujson.loads(result.stdout)
    synonyms = list(chain(*[gather_type_synonym(r['ast']) for r in parsed_data['contents']]))
    call_graphs = {}
    free_vars = {}
    diagnoses : list[Diagnosis] = []
    for file_id, parse_result in enumerate(parsed_data['contents']):
        ast = parse_result['ast']
        file_ = Path(parse_result['file']).as_posix()
        module_name = parse_result['moduleName']
        if file_ == file_path:
            continue
        else:
            prolog_file = file_[:-3] + '.pl'
            with Prolog(interface=PlInterface.File, file=base_dir / prolog_file) as prolog:
                system = System(
                    ast=ast,
                    file_id=file_id,
                    hs_file=base_dir / file_,
                    prolog_instance=prolog,
                    base_dir=base_dir,
                    module_name=module_name,
                    synonyms=synonyms,
                )
                system.marshal()
                system.generate_only()
                call_graphs.update(system.call_graph.graph)
                free_vars.update(system.free_vars)

    for file_id, parse_result in enumerate(parsed_data['contents']):
        ast = parse_result['ast']
        file_ = Path(parse_result['file']).as_posix()
        module_name = parse_result['moduleName']
        if file_ == file_path:
            prolog_file = file_[:-3] + '.pl'
            with Prolog(interface=PlInterface.File, file=base_dir / prolog_file) as prolog:
                system = System(
                    base_dir=base_dir,
                    ast=ast,
                    file_id=file_id,
                    hs_file=base_dir / file_,
                    prolog_instance=prolog,
                    module_name=module_name,
                    synonyms=synonyms,
                )

                system.marshal()
                system.free_vars.update(free_vars)
                diagnoses = system.type_check()


    response = Response(
        text=base64.b64encode(text.encode()).decode('utf-8'),
        user_id=user_id,
        diagnoses=diagnoses,
        max_line_number=number_of_lines,
        max_col_number=number_of_cols,
        file=file_path,
        file_list=simplified_files
    )
    return env.get_template('start.html').render(
        data=response.json())


