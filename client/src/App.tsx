import React, {useEffect} from "react"
import Sidebar from "./Sidebar"
import Debugger from "./Debugger";
import Editor from "./Editor";
import useAppStore from "./state";
import {Box, Flex} from "@chakra-ui/react";
import Header from './Header'
const App = () => {
    const setFileList = useAppStore((state: any) => state.setFileList)
    useEffect(() => {
        setFileList().then((_:any) => {});
    }, [])
    return (
        <Flex h={'100%'} direction={'column'} justify={'stretch'}>
            <Header></Header>
            <Flex  h={'100%'}>
                <Box w={150} p={2}>
                    <Sidebar/>
                </Box>
                <Box flex={1}>
                    <Editor/>
                </Box>
                <Box w={400} p={2}>
                    <Debugger/>
                </Box>
            </Flex>
        </Flex>


    )
};

export default App
