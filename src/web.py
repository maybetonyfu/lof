from fastapi import FastAPI, Body
from fastapi.responses import HTMLResponse, PlainTextResponse
from fastapi.staticfiles import StaticFiles
from pathlib import Path
from src.haskell import System, TError, Span
from pydantic import BaseModel

app = FastAPI()

items = {}

@app.on_event("startup")
async def startup_event():
    base_dir = Path(__file__).parent.parent / "example"
    system = System(str(base_dir))
    items["system"] = system
    items['base_dir'] = base_dir


@app.get("/api/dir")
def get_dir():
    return [str(p.relative_to(items['base_dir'])) for p in items['base_dir'].rglob("*.hs")]


@app.get("/api/typecheck")
def typecheck() -> list[TError]:
    system = items["system"]
    result = system.type_check()
    return result


class Replace(BaseModel):
    error_id: int
    slice: Span


class Hole(BaseModel):
    original: str
    hole_id: int
    loc: Span
    signature: str
    kind: str
    error_id: int



@app.get("/api/suggestion/{error_id}/{mcs_id}")
def suggestion(error_id: int, mcs_id: int) -> list[str]:
    system = items["system"]
    suggestions = system.inference(error_id, mcs_id)
    return suggestions


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
            <link rel="icon" href="https://fav.farm/💩" /> 
            <link  rel="stylesheet" href="https://fonts.googleapis.com/css?family=Roboto:300,400,500,700&display=swap"/>
            <link rel="stylesheet" href="/static/css/style.css">
        </head>
        <body>
            <div id="react-root"></div>
            <script src="/static/js/main.js"></script>
        </body>
    </html>
    """
