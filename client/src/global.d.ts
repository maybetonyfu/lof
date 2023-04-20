export type Span = [[number, number], [number, number]]
export type Loc = [string, Span]


export interface Diagnosis {
    causes: Cause []
    locs: Loc[]
    decls: Decl[]
}

export interface Decl {
    name: string,
    display_name: string,
    type: string | null,
    loc: Loc
}

export interface Cause {
    suggestions: Suggestion[],
    locs: Loc[],
    decls: Decl[]
}

export interface Fix {
    fix_type: "Type" | "Term"
    original_text: string
    inferred_type: string
    is_mismatch_decl: boolean
    mismatch_decl: string | null
    mismatch_usage_type: string | null
    mismatch_usage_loc: Loc  | null

}

export interface Suggestion {
    text: string
    title: string
}

export interface FileStore {
    fileList: string[],
    buffer: string,
    openedFile: null | string,
    prolog: boolean,
    showProlog: () => Promise<void>,
    writeFile: () => Promise<void>,
    readFile: (name: string) => Promise<void>,
    setFileList: () => Promise<void>
    setBuffer: (string) => void
}

export interface DebuggerStore {
    chooseFix: (errorId: number, causeId: number | null) => void,
    chooseError: (number) => void,
    typeCheck: (file: string) => Promise<void>,
    searchType: (string) => Promise<string[]>
    togglePreview: () => void,
    isLoading: boolean,
    previewEnabled: boolean,
    errors: Diagnosis[],
    activeErrorId: null | number,
    activeCauseId: null | number,
    // highlights: Span[],
    suggestions: string[],

}

export interface EditorStore {
    highlights: Highlight[],
    dirty: bool,
    previousHighlights: null | Highlight[]
    setHighlights: () => void
    pushHighlights: (highlights: Highlight[]) => void
    popHighlights: () => void
    setDirty: (bool) => void
}


export interface Highlight {
    span: Span,
    marker: string
}
export type AppStore = DebuggerStore & FileStore & EditorStore
