from fastapi import FastAPI, Body
from fastapi.responses import HTMLResponse, PlainTextResponse
from fastapi.staticfiles import StaticFiles
from pathlib import Path
from src.haskell import System, Error, Rule, TypeSig, Diagnosis
from pydantic import BaseModel
from src.prolog import Prolog, PlInterface
import shutil

app = FastAPI()


@app.get("/api/ls")
async def get_dir(user_id: str | None = None):
    source_dir = Path(__file__).parent.parent / "example"
    destination_dir = Path(__file__).parent.parent / "tmp" / user_id
    (Path(__file__).parent.parent / "tmp" ).mkdir(exist_ok=True, parents=True)
    shutil.copytree(source_dir, destination_dir)
    return [str(p.relative_to(destination_dir)) for p in destination_dir.rglob("*.hs")]


class TypeCheckResult(BaseModel):
    errors: list[Error]
    rules: list[Rule]


@app.get("/api/type_check")
async def typecheck(user_id: str) -> list[Diagnosis]:
    prolog_file = (Path(__file__).parent.parent / "tmp" / user_id / 'program.pl').as_posix()
    with Prolog(interface=PlInterface.Console, file=prolog_file) as prolog:
        base_dir = Path(__file__).parent.parent / "tmp" / user_id
        system = System(str(base_dir), prolog)
        errors = system.type_check()
        return errors


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
