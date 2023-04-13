import React from "react"
import useAppStore from "./state";
import {AppStore, Diagnosis, Cause, Suggestion} from "./global";
import {
    Box,
    Tabs,
    TabList,
    TabPanels,
    Tab,
    TabPanel,
    VStack,
    Text,
    Skeleton,
    Stack,
    Flex, HStack, Spacer,
} from "@chakra-ui/react";
import Success from "./Success";

const Suggestions = () => {
    let errors = useAppStore((state: AppStore) => state.errors)
    let activeErrorId = useAppStore((state: AppStore) => state.activeErrorId)
    let activeCauseId = useAppStore((state: AppStore) => state.activeCauseId)
    if (activeErrorId !== null && activeCauseId !== null) {
        let suggestions = errors[activeErrorId].causes[activeCauseId].suggestions
        return (<VStack>
            {suggestions.map((suggestion: Suggestion, i: number) => (
                <Box w="100%" bg={"white"} lineHeight={'180%'} px={3} py={1.5}
                     dangerouslySetInnerHTML={{__html: suggestion.text}} key={i}></Box>))}
        </VStack>)

    } else {
        return (<> </>)
    }
}

const ResultTypes = () => {
    let errors = useAppStore((state: AppStore) => state.errors)
    let activeErrorId = useAppStore((state: AppStore) => state.activeErrorId)
    let activeCauseId = useAppStore((state: AppStore) => state.activeCauseId)
    if (activeErrorId !== null && activeCauseId !== null) {
        let decls = errors[activeErrorId].causes[activeCauseId].decls
        return (<Flex direction={'column'} align={'flex-start'}>
            {decls.map(decl => (
                <Text key={decl.name} my={1} px={3} py={0.5} borderRadius={'lg'} bg={'white'}
                      fontFamily={'JetBrains Mono'}>
                    {decl.display_name} :: {decl.type}
                </Text>))}

        </Flex>)
    } else {
        return null
    }
}

const Explanation = () => {
    let errors = useAppStore((state: AppStore) => state.errors)
    let activeErrorId = useAppStore((state: AppStore) => state.activeErrorId)
    if (activeErrorId === null) {
        return null
    }
    let currentError = errors[activeErrorId]
    let decls = currentError.decls.map(d => d.display_name)

    return (<Text as={'span'}>The error occurs because the{' '}
            {decls.length === 1 ? 'expression' : 'expressions'}{' '}
            {decls.map(decl => <Text
                key={decl}
                color={'white'}
                as={'span'} px={2} mx={0.5}
                borderRadius={'sm'} display={'inline-block'} bg={"blackAlpha.700"}>{decl}</Text>)} can be inferred
            to have conflicting types.
        </Text>
    )


}

const Diagnosis = ({diagnosis, errorId}: { diagnosis: Diagnosis, errorId: number }) => {
    let chooseFix = useAppStore((state: AppStore) => state.chooseFix)
    let activeErrorId = useAppStore((state: AppStore) => state.activeErrorId)
    let activeCauseId = useAppStore((state: AppStore) => state.activeCauseId)
    let setHighlights = useAppStore((state: AppStore) => state.setHighlights)
    return (
        <TabPanel>
            <Explanation/>
            <Text mt={4} mb={2}>Possible places to fix this error:</Text>
            <VStack w={'100%'}>
                {diagnosis.causes.map((cause: Cause, i: number) => {
                    let active = activeCauseId === i && activeErrorId === errorId
                    return (
                        <VStack w={'100%'} key={i} spacing={0} boxShadow={'md'} border={1}>
                            <Box
                                onClick={_ => {
                                    chooseFix(errorId, i);
                                    setHighlights()
                                }}
                                w={'100%'} h={8} bg={active ? 'blackAlpha.800' : 'blackAlpha.600'}>
                                <Flex justify={'start'} h={8} p={1} align={'middle'}>
                                    <HStack>
                                        {cause.suggestions.map((suggestion, i) => (
                                            <Text
                                                bg={active ? 'blue.500' : 'gray.100'}
                                                color={active ? 'white' : 'black'}
                                                key={i} px={2}>
                                                {suggestion.title}
                                            </Text>))}
                                    </HStack>
                                    <Spacer/>

                                </Flex>
                            </Box>

                            {
                                active ? <Box w={'100%'} p={1} bg={'blackAlpha.200'}>
                                    <Text my={1}>To fix this error:</Text>
                                    <Suggestions/>
                                    <Text my={2}>With this change, the following types will be
                                        inferred:</Text>
                                    <ResultTypes/>
                                </Box> : <></>
                            }
                        </VStack>

                    )
                })}
            </VStack>
        </TabPanel>
    )
}
const Debugger = () => {
    const errors = useAppStore((state: AppStore) => state.errors)
    const chooseError = useAppStore((state: AppStore) => state.chooseError)
    const setHighlights = useAppStore((state: AppStore) => state.setHighlights)
    const isLoading = useAppStore((state: AppStore) => state.isLoading)

    if (isLoading) {
        return <Stack>
            <Skeleton height='45px'/>
            <Skeleton height='120px'/>
        </Stack>
    }

    if (errors.length === 0) {
        return <Success></Success>
    }

    return (
        <Box>
            <Tabs onChange={(index) => {
                chooseError(index);
                setHighlights()
            }}>
                <TabList>
                    {errors.map((_, i: number) => <Tab key={i}> Error {i + 1} </Tab>)}
                </TabList>
                <TabPanels>
                    {errors.map((diagnosis: Diagnosis, i) => <Diagnosis
                        key={i}
                        diagnosis={diagnosis}
                        errorId={i}
                    />)}
                </TabPanels>
            </Tabs>
        </Box>)

};

export default Debugger
