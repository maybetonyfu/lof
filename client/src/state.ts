import {create, StateCreator} from 'zustand'
import {FileStore, DebuggerStore, ErrorDef, Rule, RuleSet, TCResponse, TypeError, TypeSig} from "./global";

const isLowerCase = (str: string) => /^[a-z]*$/.test(str)


const useFileStore: StateCreator<
    DebuggerStore & FileStore,
    [],
    [],
    FileStore
> = (set, get) => ({
    fileList: [],
    openedFile: null,
    buffer: "",
    readFile: async (file: string) => {
        let response = await fetch('/api/file/' + file)
        let file_text: string = await response.text()
        set({
            openedFile: file,
            buffer: file_text
        })
    },

    writeFile: async (content: string) => {
        let openedFile = get().openedFile
        let response = await fetch('/api/file/' + openedFile, {
            method: "POST",
            body: content
        })
        let file_text: string = await response.text()
        set({
            buffer: file_text
        })
    },
    setFileList: async () => {
        let response = await fetch('/api/ls')
        let fileList: string[] = await response.json()
        set({fileList})
    },

})

const useDebuggerStore: StateCreator<
    DebuggerStore & FileStore,
    [],
    [],
    DebuggerStore
> = (set, get) => ({
    isLoading: false,
    errors: [],
    currentError: null,
    fix: null,
    rules: [],
    errorDefs: [],
    highlights: [],
    suggestion: [],
    typeCheck: async () => {
        set({isLoading: true, highlights: [], fix: null})
        let response = await fetch('/api/type_check')
        let tcResponse: TCResponse = await response.json()
        let errorDefs: ErrorDef[] = tcResponse.errors.flatMap(error => {
            let error_mus = new Set(error.mus_list.flatMap(mus => mus.rules))
            let mus_rules = tcResponse.rules.filter(rule => error_mus.has(rule.rid))
            let def_rules = mus_rules.filter(rule => rule.type === 'Def')
            return def_rules.map(rule => {
                let def = rule.src_text as string
                let usages = mus_rules.filter(r => r.src_text === def && r.type === "Var")
                return {
                    rule: rule,
                    def: def,
                    usages: usages
                }
            })
        })

        set({
            isLoading: false,
            errors: tcResponse.errors,
            rules: tcResponse.rules,
            errorDefs: errorDefs
        })
    },

    setHighlight: (spans: [[number, number], [number, number]][]) => {
        set({highlights: spans})
    },

    chooseFix: async (error: number, fix: number) => {
        set({highlights: []})
        set({fix, currentError: error})
        let errors: TypeError[] = get().errors
        let errorDefs = get().errorDefs
        let currentError = errors[error]
        let currentMcs: RuleSet = currentError.mcs_list.find(f => f.setId == fix) || {setId: -1, rules: []}
        let mcsRules = currentMcs.rules
        let rules = get().rules.filter(rule => mcsRules.includes(rule.rid))
        let locs = rules.map(rule => rule.loc)
        set({highlights: locs})
        let response = await fetch(`/api/infer/${currentError?.error_id}/${currentMcs.setId}`)
        let typeSigs: TypeSig[] = await response.json()
        let suggestion: string[] = rules.map((rule: Rule) => {
            return rule.watch.map(t => {
                let ts = typeSigs.find(ts => ts.var === t.value) as TypeSig
                let toType = ts.type
                let isTypeVar = isLowerCase(toType)
                let ruleHead = rule.head
                let isDef = errorDefs.find((ed: ErrorDef) => ed.def === ruleHead)
                return suggest(rule.type, rule.src_text as string, toType, isTypeVar, isDef)
            }).join(',')
        })
        set({suggestion})
    }
})


function suggest(ruleType: string,
                 fromExp: string,
                 toType: string,
                 isTypeVar: boolean,
                 isDef: ErrorDef | undefined
): string {
    let additional: string = '';

    if (isDef) {
        let def: string = isDef.def
        // let [line, col] = isDef.rule.loc[0]
        let usageDetail: string;
        if (isDef.usages.length !== 0) {
            let [line, _] = isDef.usages[0].loc[0]
            usageDetail = `usage on line ${line}`
        } else {
            usageDetail = `other definitions`
        }
        additional = `, so that the type of ${def} match its ${usageDetail}`
    }
    if (ruleType === "Lit") {
        let effect = isTypeVar ? "an expression of a different type" : `an instance of type ${toType}`
        return `Change ${fromExp} to ${effect}${additional}.`
    } else if (ruleType === "Type") {
        let effect = isTypeVar ? "a different type" : `${toType}`
        return `Change ${fromExp} to ${effect}${additional}.`

    } else if (ruleType === "Var") {
        let effect = isTypeVar ? "an expression of a different type" : `an instance of type ${toType}`
        return `Change ${fromExp} to ${effect}${additional}.`

    } else if (ruleType === "App") {
        let effect = isTypeVar ? "an expression of a different type" : `an instance of type ${toType}`
        return `Change ${fromExp} to ${effect}${additional}.`
    }
    return ''
}

const useAppStore = create<DebuggerStore & FileStore>()((...a) => ({
    ...useDebuggerStore(...a),
    ...useFileStore(...a),
}))
export default useAppStore
