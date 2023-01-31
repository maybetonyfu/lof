import React, {ReactHTMLElement} from "react"
import useAppStore from "./state";
    import {DocumentIcon} from "@heroicons/react/24/outline"

function FileEntry({file, active}: any) {
    let readFile = useAppStore((state: any) => state.readFile)
    return (active ?
            <div
                className={'bg-amber-100  px-1 py-0.5 h-8 items-center flex'}>
                <DocumentIcon className={'h-6'}/>
                <span className={'ml-2'}>{file}</span></div> :
            <div onClick={() => readFile(file)}
                 className={'bg-stone-200  px-1 py-0.5 h-8 items-center flex'}>
                <DocumentIcon className={'h-6'}/>
                <span className={'ml-2'}>{file}</span></div>
    )
}

function Sidebar() {
    let fileList = useAppStore((state: any) => state.fileList)
    let openedFile = useAppStore((state: any) => state.openedFile)
    return (<div>
        {fileList.map(f => (<FileEntry file={f} active={openedFile == f} key={{f}}></FileEntry>))}
    </div>)
}

export default Sidebar
