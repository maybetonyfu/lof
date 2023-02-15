import React from "react"
import useAppStore from "./state";
import {
    Button,
    VStack,
    Text,
} from '@chakra-ui/react'

function Entry({file}: any) {
    let openedFile = useAppStore((state: any) => state.openedFile)
    let readFile = useAppStore((state: any) => state.readFile)
    let active = openedFile === file
    return (<Button w={'100%'} colorScheme={'gray'} onClick={_ => readFile(file)}>
        {file}
    </Button>)
}

function Sidebar() {
    let fileList = useAppStore((state: any) => state.fileList)
    return (
        <VStack spacing={1}>
            {fileList.map((f: string) => <Entry key={f} file={f}></Entry>)}
        </VStack>
    )
}

export default Sidebar
