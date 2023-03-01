export type Span = [[number, number], [number, number]]



export interface Diagnosis {
    causes: Cause []
    locs: Span[]
    decls: Decl[]
}

export interface Decl {
    name: string,
    type: string | null,
    loc: Span
}

export interface Cause {
    suggestions: Fix[],
    locs: Span[],
    decls: Decl[]
}

export interface Fix {
    fix_type: "Type" | "Term"
    original_text: string
    inferred_type: string
    is_mismatch_decl: boolean
    mismatch_decl: string | null
    mismatch_usage_type: string | null
    mismatch_usage_loc: Span  | null

}

export interface FileStore {
    fileList: string[],
    buffer: string,
    openedFile: null | string,
    writeFile: (f: string) => Promise<void>,
    readFile: (name: string) => Promise<void>,
    setFileList: () => Promise<void>

}

export interface DebuggerStore {
    chooseFix: (errorId: number, causeId: number | null) => void,
    chooseError: (number) => void,
    typeCheck: () => Promise<void>,
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
    previousHighlights: null | Highlight[]
    setHighlights: () => void
    pushHighlights: (highlights: Highlight[]) => void
    popHighlights: () => void
}


export interface Highlight {
    span: Span,
    marker: string
}
export type AppStore = DebuggerStore & FileStore & EditorStore
