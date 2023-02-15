export interface Term {
    value: string
    kind: "Var" | "Atom" | "Struct" | "Array"
}

export interface RuleSet {
    setId: number,
    rules: number[],

}

export type Span = [[number, number], [number, number]]

export interface Rule {
    rid: number,
    loc: Span,
    watch: Term[],
    head: string,
    src_text: string | null,
    type: string
}

export interface TCResponse {
    errors: TypeError[],
    rules: Rule[]
}

export interface TypeError {
    error_id: number,
    mus_list: RuleSet[],
    mcs_list: RuleSet[],
    mss_list: RuleSet[],
}

export interface TypeSig {
    var: str,
    type: str,
}

export interface AppStore {
    chooseFix: (number, number) => Promise<void>
    writeFile: (string) => Promise<void>,
    typeCheck: () => Promise<void>,
    fileList: string[],
    openedFile: null | string,
    buffer: string,
    isLoading: boolean,
    errors: TypeError[],
    current_error: null | number,
    fix: null | number,
    rules: Rule[],
    errorDefs: ErrorDef[],
    highlights: Span[],
    suggestion: string[]
}

interface ErrorDef {
    def: string,
    rule: Rule,
    usages: Rule[]
}
