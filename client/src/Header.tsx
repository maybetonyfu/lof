import React from "react"
import {Box, Button, Flex} from "@chakra-ui/react";
import useAppStore from "./state";
import {AppStore} from "./global";
function Header () {
    let writeFile = useAppStore((state: AppStore) => state.writeFile)
    let typeCheck = useAppStore((state: AppStore) => state.typeCheck)
    let setHighlights = useAppStore((state: AppStore) => state.setHighlights)
    let  isLoading = useAppStore((state: AppStore) => state.isLoading)
    return (<Flex w={'100%'}  py={1} bg={'blackAlpha.100'} justify={'space-between'}>
        <Box></Box>
        <Flex px={8}><Button
            disabled={isLoading}
            onClick={_ => {
                writeFile().then(typeCheck).then(setHighlights)}}
            colorScheme={'blue'} size={'sm'}>Save</Button></Flex>
    </Flex>)
}
export default Header
