import React from 'react';
import CodeMirror from '@uiw/react-codemirror';
import {EditorView} from "codemirror";
import useAppStore from "./state";
import {Decoration, DecorationSet, keymap} from "@codemirror/view";
import {
    ChangeDesc,
    EditorState,
    RangeSet,
    StateEffect,
    StateEffectType,
    StateField,
    Transaction
} from "@codemirror/state";
import {AppStore} from "./global";

const themeExt = EditorView.theme({
    "&": {height: "100%", fontSize: '17px'},
    ".marker0": {background: "#d5f200"},
    ".marker1": {background: "#ffdddf"},
    ".marker2": {background: "#ffe0b2"},
    ".marker3": {background: "#5dffa2"},
    ".marker4": {background: "#c6e0ff"},
    ".marker5": {background: "#00ffe4"},
})

const keymapExt = keymap.of([{
    key: "Mod-s",
    preventDefault: true,
    run: (view: EditorView) => {
        let newText = view.state.doc.sliceString(0, view.state.doc.length)
        useAppStore.getState().writeFile(newText)
            .then(_ => useAppStore.getState().typeCheck())
        return true
    }
}])

interface HighlightSpec {
    from: number,
    to: number,
    marker: string
}

const hlEffect: StateEffectType<HighlightSpec> = StateEffect.define({
    map: ({from, to, marker}: HighlightSpec, change: ChangeDesc) => ({
        from: change.mapPos(from),
        to: change.mapPos(to),
        marker
    })
});

const highlightExt: StateField<DecorationSet> = StateField.define({
    create: () => Decoration.none,
    update: (field: RangeSet<Decoration>, transaction: Transaction) => {
        let newField = RangeSet.empty
        for (let e of transaction.effects) {
            if (e.is(hlEffect)) {
                newField = newField.update({
                    add: [
                        Decoration.mark({
                            class: e.value.marker
                        }).range(e.value.from, e.value.to)
                    ]
                });
            }
        }
        return newField;
    },
    provide: (field) => EditorView.decorations.from(field)
});


function App() {
    let document = useAppStore((state: AppStore) => state.buffer)
    let editorRef: React.MutableRefObject<null | EditorView> = React.useRef(null)
    let highlights = useAppStore((state: AppStore) => state.highlights)

    React.useEffect(() => {
        if (editorRef.current === null) return;
        let doc = editorRef.current.state.doc;
        let effects: StateEffect<HighlightSpec>[] = highlights.map((hl) => {
            let [fromL, fromC] = hl[0]
            let [toL, toC] = hl[1]
            const startPos = doc.line(fromL).from + fromC - 1
            const endPos = doc.line(toL).from + toC - 1
            return hlEffect.of({from: startPos, to: endPos, marker: `marker0`})
        });

        editorRef.current.dispatch({effects: effects})
    }, [highlights])

    return (
        <CodeMirror
            value={document}
            height="100vh"
            extensions={[themeExt, keymapExt, highlightExt]}
            onCreateEditor={(view: EditorView, _) => {
                editorRef.current = view
            }}
        />
    );
}

export default App;
