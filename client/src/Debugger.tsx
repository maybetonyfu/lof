import React from "react"
import useAppStore from "./state";
import {AppStore, Diagnosis, Cause, Fix, Span, Decl, Loc} from "./global";
import {
    Box,
    Tabs,
    TabList,
    TabPanels,
    Tab,
    TabPanel,
    VStack,
    Button,
    Text,
    Skeleton,
    Stack,
    Popover,
    PopoverTrigger,
    PopoverContent,
    PopoverHeader,
    PopoverBody,
    PopoverArrow,
    PopoverCloseButton,
    Heading, Flex, HStack, Spacer, Icon,
} from "@chakra-ui/react";
import {BiFileFind} from 'react-icons/bi'
import {FaRegQuestionCircle} from 'react-icons/fa'
import Success from "./Success";

const Code = ({children, ...props}: any) => {
    return <Text
        as={'span'}
        borderRadius={'md'}
        display={'inline-block'}
        h={6}
        lineHeight={6}
        px={0.5}
        fontFamily={'JetBrains Mono'} {...props}>{children}</Text>
}

const Type = ({sig, ...props}: any) => {
    let [fills, setFills] = React.useState<string[]>([])
    let searchType = useAppStore((state: AppStore) => state.searchType)
    React.useEffect(() => {
        searchType(sig).then(types => setFills(types))
    }, [sig])
    return (<Popover>
        <PopoverTrigger>
            <Box display={'inline-block'}>
                <Flex bg={'blackAlpha.300'} w={'fit-content'} borderRadius={'md'} h={6} align={'center'} px={0.5}>
                    <Text fontFamily={'JetBrains Mono'} display={'inline-block'}>
                        {sig}</Text>
                    <Icon boxSize={4} as={FaRegQuestionCircle} color={'backAlpha.700'}/>
                </Flex>
            </Box>

        </PopoverTrigger>
        <PopoverContent color='white' bg='blue.800' borderColor='blue.800'>
            <PopoverArrow bg='blue.800'/>
            <PopoverCloseButton/>
            <PopoverHeader>Type: {sig}</PopoverHeader>
            <PopoverBody>
                <Text>Possible values:</Text>
                {
                    fills.map((fill, i) => {
                        return <Text key={i} {...props}>{fill}</Text>
                    })

                }</PopoverBody>
        </PopoverContent>
    </Popover>)
}

const Hint = ({fix}: { fix: Fix }) => {
    let pushHighlights = useAppStore((state: AppStore) => state.pushHighlights)
    let popHighlights = useAppStore((state: AppStore) => state.popHighlights)
    let errors = useAppStore((state: AppStore) => state.errors)
    let activeErrorId = useAppStore((state: AppStore) => state.activeErrorId)
    let decls = errors[activeErrorId as number].decls
    let fixType = fix.fix_type
    let originalText = fix.original_text
    let inferredType = fix.inferred_type
    let isDecl = fix.is_mismatch_decl
    let mismatch_decl = fix.mismatch_decl === null ? null : fix.mismatch_decl.split('_')[0]
    let usageType = fix.mismatch_usage_type
    let usageLoc = fix.mismatch_usage_loc
    let condition = null
    let before = <Code mx={1} bg={'blue.500'} color={'white'}>{originalText}</Code>
    let after;
    if (isDecl && usageType !== null && usageLoc !== null) {
        after = fixType === 'Term' ? 'a different expression' : 'a different type'
        condition = (<>, so that
            <Code
                _hover={{background: 'yellow.300'}}
                onMouseEnter={() => {
                    let loc = decls.find(decl => decl.name === fix.mismatch_decl)?.loc as Loc
                    pushHighlights([{span: loc[1] as Span, marker: 'marker-secondary'}])
                }}
                onMouseLeave={() => {
                    popHighlights()
                }}
                mx={1} bg={'blackAlpha.300'}> {mismatch_decl} </Code> can be used as
            <Type sig={usageType}/>
            on
            <Code bg={'blackAlpha.300'}
                  _hover={{
                      background: "yellow.300",
                  }}
                  onMouseEnter={() => {
                      console.log('mouse over')
                      pushHighlights([{span: (usageLoc as Loc)[1], marker: 'marker-secondary'}])
                  }}
                  onMouseLeave={() => {
                      popHighlights()
                  }}
            ><Icon as={BiFileFind}/> line {usageLoc[0][0]}</Code>
        </>)


    } else {
        if (fixType === 'Term') {
            after = (<>an instance of <Type sig={inferredType}/></>)

        } else {
            after = (<Type sig={inferredType}/>)
        }
    }
    return (<Text w={'100%'} bg={'white'} lineHeight={8} p={1.5} borderRadius={'lg'}>
        Change
        {before}
        to {after}{condition}
    </Text>)

}
const Suggestions = () => {
    let errors = useAppStore((state: AppStore) => state.errors)
    let activeErrorId = useAppStore((state: AppStore) => state.activeErrorId)
    let activeCauseId = useAppStore((state: AppStore) => state.activeCauseId)
    if (activeErrorId !== null && activeCauseId !== null) {
        let suggestions = errors[activeErrorId].causes[activeCauseId].suggestions
        return (<VStack>
            {suggestions.map((fix: Fix, i: number) => <Hint fix={fix} key={i}/>)}
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
                    {decl.name} :: {decl.type}
                </Text>))}

        </Flex>)
    } else {
        return null
    }
}

const Explanation = () => {
    let errors = useAppStore((state: AppStore) => state.errors)
    let activeErrorId = useAppStore((state: AppStore) => state.activeErrorId)
    if (activeErrorId !== null) {
        let currentError = errors[activeErrorId]
        let decls = currentError.decls.map(d => d.name)

        return (<Text as={'span'}>The error occurs because the{' '}
                {decls.length === 1 ? 'expression' : 'expressions'}{' '}
                {decls.map(decl => <Text
                    key ={decl}
                    color={'white'}
                    as={'span'} px={2}
                    borderRadius={'sm'} display={'inline-block'} bg={"blackAlpha.700"}>{decl}</Text>)} can be inferred
                to have conflicting types.
            </Text>
        )
    } else return null

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
                                                {suggestion.original_text}
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

    if (errors.length > 0) {
        return (
            <Box>
                <Tabs onChange={(index) => {
                    chooseError(index);
                    setHighlights()
                }

                }>
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
    } else {
        return <Success></Success>
    }

};

export default Debugger
