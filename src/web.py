from itertools import chain
from platform import platform
from subprocess import run

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


@app.get("/api/ls")
async def get_dir(user_id: str | None = None):
    source_dir = Path(__file__).parent.parent / "example"
    destination_dir = Path(__file__).parent.parent / "tmp" / user_id
    (Path(__file__).parent.parent / "tmp").mkdir(exist_ok=True, parents=True)
    shutil.copytree(source_dir, destination_dir)
    return [str(p.relative_to(destination_dir).as_posix()) for p in destination_dir.rglob("*.hs")]


class TypeCheckResult(BaseModel):
    errors: list[Error]
    rules: list[Rule]


@app.get("/api/type_check/{file_path:path}")
async def typecheck(file_path: str, user_id: str) -> list[Diagnosis]:
    project_dir = Path(__file__).parent.parent
    base_dir = project_dir / "tmp" / user_id
    parser_bin = str(project_dir / "bin" / "haskell-parser.exe") if platform() == 'Windows' else str(
        project_dir / "bin" / "haskell-parser")

    result = run(f'{parser_bin} {base_dir}', shell=True, check=True, capture_output=True)
    parsed_data = ujson.loads(result.stdout)
    synonyms = list(chain(*[gather_type_synonym(r['ast']) for r in parsed_data['contents']]))

    call_graphs = {}
    free_vars = {}

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
                return diagnoses


@app.get('/api/file/{file_path:path}')
async def open_file(file_path: str, user_id: str):
    file = Path(__file__).parent.parent / "tmp" / user_id / file_path
    text = file.read_text()
    return PlainTextResponse(content=text)


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


@app.get("/editor/{user_id}/{file_path:path}", response_class=HTMLResponse)
async def editor(user_id: str, file_path: str, error_id: int | None = None, cause_id: int | None = None):
    source_dir = Path(__file__).parent.parent / "example"
    destination_dir = Path(__file__).parent.parent / "tmp" / user_id
    if not destination_dir.exists():
        (Path(__file__).parent.parent / "tmp").mkdir(exist_ok=True, parents=True)
        shutil.copytree(source_dir, destination_dir)

    files = destination_dir.rglob("*.hs")
    simplified_files = sorted([str(f.relative_to(destination_dir).as_posix()) for f in files])
    if file_path == 'empty':
        return env.get_template('start.html').render(data={
            'files': simplified_files,
            "text": None,
            'user_id': user_id,
            'opened_file': None,
            'diagnoses': [],
            'error_id': error_id,
            'cause_id': cause_id,
            'diagnoses_json': '[]'
        })

    else:
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

        return env.get_template('start.html').render(
            data={
                'files': simplified_files,
                "text": text,
                'user_id': user_id,
                'opened_file': file_path,
                'number_of_lines': number_of_lines,
                'number_of_cols': number_of_cols,
                'diagnoses': diagnoses,
                'error_id': error_id,
                'cause_id': cause_id,
                'diagnoses_json': [ d.json() for d in diagnoses ]
            })



# @app.get("/", response_class=HTMLResponse)
# async def home():
#     return """
#     <!DOCTYPE html>
#     <html>
#         <head>
#             <title>Editor</title>
#             <link rel="icon" type="image/png" sizes="32x32" href="/static/favicon-32x32.png">
#             <link rel="icon" type="image/png" sizes="16x16" href="/static/favicon-16x16.png">
#             <link rel="preconnect" href="https://fonts.googleapis.com">
#             <link rel="preconnect" href="https://fonts.gstatic.com" crossorigin>
#             <link href="https://fonts.googleapis.com/css2?family=JetBrains+Mono&display=swap" rel="stylesheet">
#             <link rel="stylesheet" href="/static/css/style.css">
#         </head>
#         <body>
#             <div id="react-root"></div>
#             <script src="/static/js/main.js"></script>
#         </body>
#     </html>
#     """
