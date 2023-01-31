from fastapi import FastAPI, Body
from fastapi.responses import HTMLResponse, PlainTextResponse
from fastapi.staticfiles import StaticFiles
from pathlib import Path
from src.haskell import System, TError, Span
from src.type_hole import Hole, TypeHoles, Replace
from pydantic import BaseModel

app = FastAPI()
base_dir = Path(__file__).parent.parent / "example"


@app.get("/api/dir")
def get_dir():
    return [str(p.relative_to(base_dir)) for p in base_dir.rglob("*.hs")]


@app.get("/api/typecheck")
def typecheck() -> list[TError]:
    system = System(str(base_dir))
    result = system.type_check()
    return result


class TypeHoleRequest(BaseModel):
    file: str
    replaces: list[Replace]


@app.post("/api/typehole")
def type_hole(hole_request: TypeHoleRequest) -> list[Hole]:
    type_holes = TypeHoles(replaces=hole_request.replaces, file=hole_request.file)
    return type_holes.execute()


@app.get('/api/file/{file_path:path}')
def open_file(file_path: str):
    text = Path(base_dir / file_path).read_text()
    return PlainTextResponse(content=text)


@app.post('/api/file/{file_path:path}')
def save_file(file_path: str, file_content: str = Body()):
    Path(base_dir / file_path).write_text(file_content)
    text = Path(base_dir / file_path).read_text()
    return PlainTextResponse(content=text)


app.mount("/static", StaticFiles(directory="./client/output"), name="static")


@app.get("/", response_class=HTMLResponse)
def home():
    return """
    <!DOCTYPE html>
    <html>
        <head>
            <title>Editor</title>
            <link rel="icon" href="https://fav.farm/💩" /> 
            <script src="https://cdn.tailwindcss.com"></script>
            <link rel="stylesheet" href="/static/css/style.css">
        </head>
        <body>
            <div id="react-root"  class="h-full"></div>
            <script src="/static/js/main.js"></script>
        </body>
    </html>
    """
