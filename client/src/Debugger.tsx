import React from "react"
import useAppStore from "./state";
import {AppStore, Diagnosis, Cause, Fix, Span, Decl} from "./global";
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
const pluralize = (word: string, n: number) => {
    if (n == 1) {
        return word
    } else {
        return word + 's'
    }
}

const Code = ({children, ...props}: any) => {
    return <Text
        display={'inline-block'}
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
            <HStack bg={'blackAlpha.300'} spacing={0.5} display={'inline-block'} px={1}>

                <Text fontFamily={'JetBrains Mono'} px={0.5}  mx={1} display={'inline-block'}>
                    {sig}</Text>
                <Icon as={FaRegQuestionCircle} color={'backAlpha.700'} />
            </HStack>

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
                    console.log('mouse over')
                    let loc = decls.find(decl => decl.name ===  fix.mismatch_decl)?.loc
                    pushHighlights([{span: loc as Span, marker: 'marker-secondary'}])
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
                    pushHighlights([{span: usageLoc as Span, marker: 'marker-secondary'}])
                }}
                onMouseLeave={() => {
                    popHighlights()
                }}
            ><Icon as={BiFileFind}/> line {usageLoc[0][0]}</Code>
        </>)


    } else {
        if (fixType === 'Term') {
            after = (<>an instance of <Type sig={inferredType}/>
            </>)

        } else {
            after = (<Type sig={inferredType}/>)
        }
    }
    return (<Box width={'100%'}>
        Change
        {before}
        to {after}{condition}
    </Box>)

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

const DeclView = ({decl, changed}: { decl: Decl, changed: boolean }) => {
    let pushHighlights = useAppStore((state: AppStore) => state.pushHighlights)
    let popHighlights = useAppStore((state: AppStore) => state.popHighlights)

    let ref = React.useRef(null)
    let hasType = decl.type !== null
    let DeclType = <Text
        color={hasType ? 'black' : 'white'}
        bg={hasType ? 'green.300' : 'red.400'} ref={ref} h={7} px={2} flex={1}>{hasType ? decl.type : 'unknown'}</Text>
    React.useEffect(() => {
        let currentRef = ref.current as any
        currentRef.classList.remove('pulsing')
        setTimeout(() => {
            currentRef.classList.add('pulsing')
        }, 50)

    }, [decl.type])
    return (<Flex width={'100%'} bg={'purple.400'}>
        <Text bg={'gray.200'}
              h={7}
              px={2}
              _hover={{
                  background: 'yellow.300'
            }
              }
              onMouseEnter={() => {
                  pushHighlights(
                      [{span: decl.loc, marker: 'marker-secondary'}]
                  )
              }}
              onMouseLeave={() => {
                  popHighlights()
              }}
        >{decl.name.split('_')[0]}</Text><Flex flex={1}>{DeclType}</Flex>
    </Flex>)
}

const Decls = ({errorId}: {errorId: number}) => {

    let errors = useAppStore((state: AppStore) => state.errors)
    let activeErrorId = useAppStore((state: AppStore) => state.activeErrorId)
    let activeCauseId = useAppStore((state: AppStore) => state.activeCauseId)
    console.log(errorId)
    console.log(activeCauseId)
    let decls : Decl[] = []
    if (activeCauseId === null) {
        decls = (errors[activeErrorId as number].decls)
    } else {
        decls = (errors[activeErrorId as number].causes[activeCauseId].decls)
    }

    let [oldDecls, setOldDecls]  = React.useState<Decl[]>([])
    let [changed, setChanged]  = React.useState<boolean[]>([])

    console.log(decls.map(decl => decl.type))

    React.useEffect(() => {
        let _changed =
                decls.map((decl, i) => {
                    if (!oldDecls[i]) {
                        return true
                    } else if (oldDecls[i].type === decl.type) {
                        return false
                    }   else {
                        return true
                    }
                })
        setChanged(_changed)
        setOldDecls(decls)
        }, [decls]
    )

    return (<Box>
        <VStack>
            {decls.map(((decl, i) => <DeclView decl={decl} changed={changed[i]} key={i}/>))}
        </VStack>
    </Box>)
}

const Diagnosis = ({diagnosis, errorId}: { diagnosis: Diagnosis, errorId: number }) => {
    let chooseFix = useAppStore((state: AppStore) => state.chooseFix)
    let activeErrorId = useAppStore((state: AppStore) => state.activeErrorId)
    let activeCauseId = useAppStore((state: AppStore) => state.activeCauseId)
    let setHighlights = useAppStore((state: AppStore) => state.setHighlights)
    return (
        <TabPanel>
            <Text mb={2}>An error occurred when assigning types to the following expressions:</Text>
            <Decls errorId ={errorId}/>
            <Text mt={4} mb={2}>Possible places to fix this error:</Text>
            <VStack w={'100%'}>
                {diagnosis.causes.map((cause: Cause, i: number) => {
                    let active = activeCauseId === i && activeErrorId === errorId
                    return (
                        <VStack w={'100%'} key={i} spacing={0} boxShadow={'md'} border={1}>
                            <Box
                                onClick={_ => {
                                    if (active) {
                                        chooseFix(errorId, null);
                                        setHighlights()
                                    } else {
                                        chooseFix(errorId, i);
                                        setHighlights()
                                    }

                                }}
                                w={'100%'} h={8} bg={active ? 'blackAlpha.800' : 'blackAlpha.600'}>
                                <Flex justify={'start'} h={8} p={1} align={'middle'}>
                                    <HStack>
                                        {cause.suggestions.map((suggestion, i) => (
                                            <Text
                                                bg={active ? 'blue.500' : 'gray.100'}
                                                color={ active ? 'white' : 'black' }
                                                key={i} px={2}>
                                                {suggestion.original_text}
                                            </Text>))}
                                    </HStack>
                                    <Spacer/>
                                    {/*<Button colorScheme={'whiteAlpha'} size={"xs"}>*/}
                                    {/*    {active ? 'Ignore changes' :  'Preview changes'}*/}
                                    {/*</Button>*/}
                                </Flex>
                            </Box>

                            {
                                active ? <Box w={'100%'} p={1} bg={'blackAlpha.200'}>
                                    <Heading my={1} size={'sm'}>How to fix:</Heading>
                                    <Suggestions/>
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
    const isLoading = useAppStore((state: AppStore) => state.isLoading)
    if (isLoading) {
        return <Stack>
            <Skeleton height='45px'/>
            <Skeleton height='120px'/>
        </Stack>
    }
    return (
        <Box>
            <Tabs onChange={(index) => chooseError(index)} >
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
