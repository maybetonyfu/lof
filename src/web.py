from platform import platform
from subprocess import run

import ujson
from fastapi import FastAPI, Body
from fastapi.responses import HTMLResponse, PlainTextResponse
from fastapi.staticfiles import StaticFiles
from pathlib import Path
from src.haskell import System, Error, Rule, Diagnosis
from pydantic import BaseModel
from src.prolog import Prolog, PlInterface
import shutil

app = FastAPI()


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
    asts = [c['ast'] for c in parsed_data['contents']]
    files = [Path(c['file']).as_posix() for c in parsed_data['contents']]
    queries = []
    for ast, file in zip(asts, files):
        if file == file_path:
            continue
        else:
            prolog_file = file[:-3] + '.pl'
            with Prolog(interface=PlInterface.File, file=base_dir / prolog_file) as prolog:
                system = System(
                    ast=ast,
                    hs_file=base_dir / file,
                    prolog_instance=prolog,
                    base_dir=base_dir
                )
                system.marshal()
                queries.extend(system.prolog.queries)
                system.generate_intermediate({r.rid for r in system.rules})

    for ast, file in zip(asts, files):
        if file == file_path:
            prolog_file = file[:-3] + '.pl'
            with Prolog(interface=PlInterface.File, file=base_dir / prolog_file) as prolog:
                system = System(
                    ast=ast,
                    hs_file=base_dir / file,
                    prolog_instance=prolog,
                    base_dir=base_dir
                )
                system.marshal()
                system.prolog.queries.extend(queries)
                diagnoses = system.type_check()
                # print(diagnoses)
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
async def home():
    return """
    <!DOCTYPE html>
    <html>
        <head>
            <title>Editor</title>
            <link rel="icon" type="image/png" sizes="32x32" href="/static/favicon-32x32.png">
            <link rel="icon" type="image/png" sizes="16x16" href="/static/favicon-16x16.png">
           
            <link rel="preconnect" href="https://fonts.googleapis.com">
            <link rel="preconnect" href="https://fonts.gstatic.com" crossorigin>
            <link href="https://fonts.googleapis.com/css2?family=JetBrains+Mono&display=swap" rel="stylesheet">
            <link rel="stylesheet" href="/static/css/style.css">
        </head>
        <body>
            <div id="react-root"></div>
            <script src="/static/js/main.js"></script>
        </body>
    </html>
    """
