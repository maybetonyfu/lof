import {create} from 'zustand'
import {Hole, TypeError, RuleSet} from "./global";


const useAppStore = create((set, get) => ({
    fileList: [],
    openedFile: null,
    buffer: "",
    isLoading: false,
    errors: [],
    fix: null,
    highlights: [],
    replacements: [],
    readFile: async (file: string) => {
        let response = await fetch('/api/file/' + file)
        let file_text: string = await response.text()
        set({
            openedFile: file,
            buffer: file_text
        })
    },
    writeFile: async (content: string) => {
        let openedFile = (get() as any).openedFile
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
        let response = await fetch('/api/dir')
        let fileList: string[] = await response.json()
        set({fileList})
    },

    type_check: async () => {
        set({isLoading: true, highlights: [], fix: null})
        let response = await fetch('/api/typecheck')
        let typeErrors: TypeError[] = await response.json()
        set({isLoading: false, errors: typeErrors})
    },

    setHighlight: (spans: [[number, number], [number, number]][]) => {
        set({highlights: spans})
    },

    chooseFix: async (fix: number) => {
        set({highlights: [], replacements: []})
        set({fix})
        let errors: TypeError[] = (get() as any).errors
        let currentError = errors
            .find(error => error.mcs_list.map(mcs => mcs.setId).includes(fix))
        let currentMcs: RuleSet = currentError?.mcs_list.find(f => f.setId == fix) || {setId: -1, rules: []}
        let sliceIds = currentMcs.rules
        let slices =
            currentError?.slices
                .filter(s => sliceIds.includes(s.slice_id))
                .map(s => s.loc)
        let replaces = errors.flatMap(error => {
            if (error.error_id === currentError?.error_id) {
                return slices?.map(slice => ({error_id: error.error_id, slice}))
            } else {
                let sliceIds = error.mcs_list[0].rules
                let slices = error.slices.filter(s => sliceIds.includes(s.slice_id)).map(s => s.loc)
                return slices.map(slice => ({error_id: error.error_id, slice}))
            }
        })
        set({highlights: slices})
        let response = await fetch('/api/typehole', {
            method: 'POST',
            headers: {
                'Content-Type': 'application/json'
            },
            body: JSON.stringify({
                file: (get() as any).openedFile,
                replaces: replaces,
            })
        })
        let holes: Hole[] = await response.json()
        holes = holes.filter((hole: Hole) => hole.error_id === currentError?.error_id)
        set({replacements: holes})
    }
}))

export default useAppStore
