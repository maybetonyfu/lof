from fastapi import FastAPI, Body
from fastapi.responses import HTMLResponse, PlainTextResponse
from fastapi.staticfiles import StaticFiles
from pathlib import Path
from src.haskell import System, TCError, Rule, TypeSig
from pydantic import BaseModel
from src.prolog import Prolog, PlInterface

app = FastAPI()

items = {}


@app.on_event("startup")
async def startup_event():
    with Prolog(interface=PlInterface.Console) as prolog:
        base_dir = Path(__file__).parent.parent / "example"
        system = System(str(base_dir), prolog)
        items["system"] = system
        items['base_dir'] = base_dir


@app.get("/api/ls")
def get_dir():
    return [str(p.relative_to(items['base_dir'])) for p in items['base_dir'].rglob("*.hs")]


class TypeCheckResult(BaseModel):
    errors: list[TCError]
    rules: list[Rule]


@app.get("/api/type_check")
def typecheck() -> TypeCheckResult:
    system = items["system"]
    errors = system.type_check()
    return TypeCheckResult(errors=errors, rules=system.rules)


@app.get("/api/infer/{error_id}/{mcs_id}")
def infer(error_id: int, mcs_id: int) -> list[TypeSig]:
    system = items["system"]
    types = system.infer_type(error_id, mcs_id)
    return types


@app.get('/api/file/{file_path:path}')
def open_file(file_path: str):
    text = Path(items['base_dir'] / file_path).read_text()
    return PlainTextResponse(content=text)


@app.post('/api/file/{file_path:path}', response_class=PlainTextResponse)
def save_file(file_path: str, file_content: str = Body()):
    Path(items['base_dir'] / file_path).write_text(file_content)
    text = Path(items['base_dir'] / file_path).read_text()
    return text


app.mount("/static", StaticFiles(directory="./client/output"), name="static")

@app.get("/", response_class=HTMLResponse)
def home():
    return """
    <!DOCTYPE html>
    <html>
        <head>
            <title>Editor</title>
            <link rel="icon" type="image/png" sizes="32x32" href="/static/favicon-32x32.png">
            <link rel="icon" type="image/png" sizes="16x16" href="/static/favicon-16x16.png">
            <link rel="stylesheet" href="/static/css/style.css">
        </head>
        <body>
            <div id="react-root"></div>
            <script src="/static/js/main.js"></script>
        </body>
    </html>
    """
