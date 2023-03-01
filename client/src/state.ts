import {create, StateCreator} from 'zustand'
import {FileStore, DebuggerStore, Diagnosis, EditorStore, Span, Highlight} from "./global";

const useFileStore: StateCreator<
    DebuggerStore & FileStore & EditorStore,
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
    typeCheck: async () => {
        set({
            isLoading: true,
            highlights: [],
            activeErrorId: null,
            activeCauseId: null
        })
        let response = await fetch('/api/type_check')
        let diagnoses: Diagnosis[] = await response.json()
        let activeErrorId = diagnoses.length > 0 ? 0 : null
        let activeCauseId = null
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
        set({activeErrorId: error, activeCauseId: null})
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
    setHighlights() {
        let activeErrorId = get().activeErrorId
        let activeCauseId = get().activeCauseId
        if (activeErrorId !== null) {
            let error = get().errors[activeErrorId]
            let all_locs = error.locs
            let cause_loc = activeCauseId === null ? [] : error.causes[activeCauseId].locs
            let highlights_all = all_locs
                .filter(span => !cause_loc.some(cause_span => intersection(span, cause_span)))
                .map(span => {
                    return {span, marker: 'marker-mute'}
                })
            let highlights_cause = cause_loc.map(span => {
                return {span, marker: 'marker-active'}
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
