import React, {useEffect, useRef, useState} from "react"
import {basicSetup, EditorView} from "codemirror";
import {EditorState, StateEffect, StateField, RangeSet} from "@codemirror/state";
import {Decoration, keymap} from "@codemirror/view";
import {useQuery, useQueryClient} from 'react-query'
import {fetchData} from "./query";

export default () => {
    let dom = useRef(null)
    const {isLoading, isError, data, error} = useQuery('data', fetchData)
    let [document, setDocument] = useState("hello world")
    let [view, setView] = useState(null);
    const saveDocument = (view) => {
        console.log("Save document")
    }
    let defaultTheme = EditorView.theme({
        "&": {
            height: "100%"
        }
    })


    useEffect(() => {
        console.log("I have bben run")
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

        const higlihtField = StateField.define({
            create: () => Decoration.none,
            update: (field, transaction) => {
                let newfield = RangeSet.empty
                for (let e of transaction.effects) {
                    if (e.is(hlEffectRef.current)) {
                        newfield = newfield.update({
                            add: [
                                Decoration.mark({class: e.value.marker}).range(e.value.from, e.value.to)
                            ]
                        });
                    }
                }
                return newfield;
            },
            provide: (field) => EditorView.decorations.from(field)
        });

        let state = EditorState.create({
            doc: document,
            extensions: [basicSetup, defaultTheme, defaultKeymap, editorTheme, higlihtField]
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
        changes: { from: 0, to: view.state.doc.length }
      })
      view.dispatch({
        changes: { from: 0, insert: data.file_content }
      })
    }
  }, [data?.file_content])



    return (
        <div ref={dom} id="editor" className="h-full ">
        </div>
    )

}
