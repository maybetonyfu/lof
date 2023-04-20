import {create, StateCreator} from 'zustand'
import {FileStore, DebuggerStore, Diagnosis, EditorStore, Span, Highlight} from "./global";
import {customAlphabet} from 'nanoid'

const nanoid = customAlphabet('1234567890abcdefghijklmnopqrstuvwxyz', 15)

const user_id = nanoid()
const useFileStore: StateCreator<
    DebuggerStore & FileStore & EditorStore,
    [],
    [],
    FileStore
> = (set, get) => ({
    fileList: [],
    openedFile: null,
    prolog: false,
    buffer: "",
    setBuffer: (buffer: string) => set({buffer, dirty: true}),
    readFile: async (file: string) => {

        let response = await fetch(`/api/file/${file}?user_id=${user_id}`)
        let file_text: string = await response.text()
        set({
            openedFile: file,
            buffer: file_text,
            dirty: false
        })
    },
    showProlog: async () => {
        if (get().prolog) {
            let file = (get().openedFile) as string
            let response = await fetch(`/api/file/${file}?user_id=${user_id}`)
            let file_text: string = await response.text()
            set({
                buffer: file_text,
                prolog: false
            })
        } else {
            let currentFile = get().openedFile
            if (currentFile !== null) {
                let prologFilePath = currentFile.substr(0, currentFile.lastIndexOf(".")) + ".pl";
                let response = await fetch(`/api/file/${prologFilePath}?user_id=${user_id}`)
                let prologText: string = await response.text()
                set({
                    buffer: prologText,
                    prolog: true,
                })

            }
        }

    },
    writeFile: async () => {
        let openedFile = get().openedFile
        let buffer = get().buffer
        console.log(buffer)
        await fetch(`/api/file/${openedFile}?user_id=${user_id}`, {
            method: "POST",
            body: buffer
        })
        return undefined
    },
    setFileList: async () => {
        let response = await fetch(`/api/ls?user_id=${user_id}`, {})
        let fileList: string[] = await response.json()
        set({fileList})
    },

})

const useDebuggerStore: StateCreator<
    DebuggerStore & FileStore & EditorStore,
    [],
    [],
    DebuggerStore
> = (set, get) => ({
    isLoading: false,
    errors: [],
    activeErrorId: null,
    activeCauseId: null,
    suggestions: [],
    previewEnabled: false,
    typeCheck: async (file) => {
        set({
            isLoading: true,
            highlights: [],
            activeErrorId: null,
            activeCauseId: null,
            dirty: false
        })
        console.log(file)
        let response = await fetch(`/api/type_check/${file}?user_id=${user_id}`)
        let diagnoses: Diagnosis[] = await response.json()
        let activeErrorId = diagnoses.length > 0 ? 0 : null
        let activeCauseId = 0
        set({
            isLoading: false,
            errors: diagnoses,
            activeErrorId,
            activeCauseId
        })
    },

    togglePreview: () => {
        let previewEnabled = !get().previewEnabled
        set({previewEnabled: previewEnabled})
    },

    chooseError: (error: number) => {
        set({activeErrorId: error, activeCauseId: 0})
    },

    chooseFix: (error: number, cause: number | null) => {
        set({activeCauseId: cause, activeErrorId: error})
    },

    searchType: async (type) => {
        if (type === 'Bool') {
            return Promise.resolve(['True', 'False'])
        } else if (type === 'Int') {
            return Promise.resolve(['1', '2', '3', '4'])
        } else if (type === 'Char') {
            return Promise.resolve(["'a'", "'b'", "'c'"])
        } else if (type === '[Char]') {
            return Promise.resolve(['"hello"', '"12345"'])
        } else if (type === 'Float') {
            return Promise.resolve(['1.5', '0.0', '3.1415'])
        } else {
            let query = encodeURIComponent(type + '+Prelude+base')
            let response = await fetch(`https://hoogle.haskell.org/?mode=json&format=text&hoogle=${query}&count=5`)
            let types: { item: string }[] = await response.json()
            return types.map(type => type.item)
        }
    }
})

const useEditorStore: StateCreator<
    DebuggerStore & FileStore & EditorStore,
    [],
    [],
    EditorStore
> = (set, get) => ({
    highlights: [],

    previousHighlights: null,
    dirty: false,
    setDirty(dirty) {
        set({dirty})
    },
    setHighlights() {
        let activeErrorId = get().activeErrorId
        let activeCauseId = get().activeCauseId
        if (activeErrorId !== null) {
            let error = get().errors[activeErrorId]
            let all_locs = error.locs
            let cause_loc = activeCauseId === null ? [] : error.causes[activeCauseId].locs
            let highlights_all = all_locs
                .filter(loc => !cause_loc.some(cause_loc => intersection(loc[1], cause_loc[1])))
                .map(loc => {
                    return {span: loc[1], marker: 'marker-mute'}
                })
            let highlights_cause = cause_loc.map(loc => {
                return {span: loc[1], marker: 'marker-active'}
            })
            set({highlights: [...highlights_cause, ...highlights_all,]})
        }
    },
    pushHighlights(highlights: Highlight[]) {
        let currentHighlights = get().highlights
        let newCurrent = currentHighlights.filter(span => !highlights.some(newHl => intersection(
            span.span, newHl.span
        )))
        set({previousHighlights: currentHighlights, highlights: [...newCurrent, ...highlights]})
    },
    popHighlights() {
        let previous = get().previousHighlights
        if (previous !== null) {
            set({previousHighlights: null, highlights: previous})
        }
    }

})

function intersection(a: Span, b: Span): boolean {
    let [[aFromL, aFromC], [aToL, aToC]] = a
    let [[bFromL, bFromC], [bToL, bToC]] = b
    return (aFromL === bToL && aFromC <= bToC) ||
        (bFromL === aToL && bFromC <= aToC)
}

const useAppStore = create<DebuggerStore & FileStore & EditorStore>()((...a) => ({
    ...useDebuggerStore(...a),
    ...useFileStore(...a),
    ...useEditorStore(...a)
}))
export default useAppStore
