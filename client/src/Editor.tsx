import React, {useEffect, useRef, useState} from "react"
import {basicSetup, EditorView} from "codemirror";
import {EditorState, StateEffect, StateField, RangeSet, StateEffectType} from "@codemirror/state";
import {Decoration, keymap} from "@codemirror/view";
import useAppStore from "./state"
import Box from '@mui/material/Box';
export default () => {
    let dom = useRef(null)
    let document = useAppStore((state: any) => state.buffer)
    let writeFile = useAppStore((state: any) => state.writeFile)
    let type_check = useAppStore((state: any) => state.type_check)
    let highlights = useAppStore((state: any) => state.highlights)

    let [view, setView] = useState(null);

    const hlEffect: StateEffectType<any> = StateEffect.define({
        map: ({from, to, marker}, change) => ({
            from: change.mapPos(from),
            to: change.mapPos(to),
            marker
        })
    });

    const hlEffectRef = useRef(hlEffect)
    const saveDocument = (view: any) => {
        writeFile(view.state.doc.sliceString(0, view.state.doc.length))
            .then(type_check)
        return true
    }

    let defaultTheme = EditorView.theme({
        "&": {height: "100%", fontSize: '18px'}
    })

    useEffect(() => {
        const editorTheme = EditorView.baseTheme({
            ".marker0": {background: "#d5f200"},
            ".marker1": {background: "#ffdddf"},
            ".marker2": {background: "#ffe0b2"},
            ".marker3": {background: "#5dffa2"},
            ".marker4": {background: "#c6e0ff"},
            ".marker5": {background: "#00ffe4"},
        });

        const defaultKeymap = keymap.of([{
            key: "Mod-s",
            preventDefault: true,
            run: saveDocument
        }])

        const highlightField = StateField.define({
            create: () => Decoration.none,
            update: (field, transaction) => {
                let newField = RangeSet.empty
                for (let e of transaction.effects) {
                    if (e.is(hlEffectRef.current)) {
                        newField = newField.update({
                            add: [
                                Decoration.mark({class: (e as any).value.marker}).range((e as any).value.from, (e as any).value.to)
                            ]
                        });
                    }
                }
                return newField;
            },
            provide: (field) => EditorView.decorations.from(field)
        });

        let state = EditorState.create({
            doc: document,
            extensions: [basicSetup, defaultTheme, defaultKeymap, editorTheme, highlightField]
        });

        let v = new EditorView({
            state,
            parent: dom.current
        });
        setView(v)
    }, [])

    useEffect(() => {
        if (view) {
            view.dispatch({
                changes: {from: 0, to: view.state.doc.length}
            })
            view.dispatch({
                changes: {from: 0, insert: document}
            })
        }
    }, [document])

    useEffect(() => {
        if (view == null) return;
        let doc = view.state.doc;
        let hlEffects = highlights.map((hl) => {
            let [fromL, fromC] = hl[0]
            let [toL, toC] = hl[1]
            const startPos = doc.line(fromL).from + fromC - 1
            const endPos = doc.line(toL).from + toC - 1
            return hlEffectRef.current.of({ from: startPos, to: endPos, marker: `marker0` })
        });

        (view as any).dispatch({ effects: hlEffects })
    }, [highlights])

    return (
        <Box ref={dom} id="editor" sx={{height: '100%'}} />

        // <div ref={dom} id="editor" className="h-full ">
        // </div>
    )

}
