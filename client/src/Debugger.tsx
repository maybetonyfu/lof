import React from "react"
import useAppStore from "./state";
import {AppStore, Rule, RuleSet, TypeError} from "./global";
import {Box, Tabs, TabList, TabPanels, Tab, TabPanel, VStack, Button, Text, Skeleton, Stack} from "@chakra-ui/react";

const pluralize = (word: string, n: number) => {
    if (n == 1) {
        return word
    } else {
        return word + 's'
    }
}

const TypeError = ({error}: any) => {
    let chooseFix = useAppStore((state: AppStore) => state.chooseFix)
    let suggestion = useAppStore((state: AppStore) => state.suggestion)
    let rules = useAppStore((state: AppStore) => state.rules)
    let currentError = useAppStore((state: AppStore) => state.currentError)
    let currentFix = useAppStore((state: AppStore) => state.fix)

    const removeDefs = (rs: RuleSet) => {
        return !rs.rules
            .flatMap(rid => rules.filter((rule: Rule) => rule.rid === rid))
            .some(rule => rule.type === "Def")
    }
    return (
        <TabPanel>
            <Text>Possible Fixes:</Text>
            <VStack w={'100%'}>
                {error.mcs_list.filter(removeDefs).map((fix: RuleSet, i: number) => {
                    let active = currentFix === fix.setId && currentError === error.error_id
                    return (
                        <Button key={i} w={'100%'} colorScheme={active ? 'blue' : 'gray'}
                                onClick={_ => chooseFix(error.error_id, fix.setId)}>
                            {`Fix ${i + 1} (${fix.rules.length} ${pluralize('place', fix.rules.length)})`}
                        </Button>)
                })}
            </VStack>
            {error.error_id === currentError ?
                (<Box>
                    <Text>How to fix:</Text>
                    <VStack>
                        {
                            suggestion.map((s, i) => <Text p={1} bg={'gray.50'} key={i}>{s}</Text>)
                        }
                    </VStack>
                </Box>) : <></>
            }

        </TabPanel>
    )
}
const Debugger = () => {
    const errors = useAppStore((state: AppStore) => state.errors)
    const isLoading = useAppStore((state: AppStore) => state.isLoading)
    if (isLoading) {
        return <Stack>
            <Skeleton height='45px'/>
            <Skeleton height='120px'/>
        </Stack>
    }

    return (
        <Box>
            <Tabs>
                <TabList>
                    {errors.map((error: TypeError) => <Tab key={error.error_id}> Error {error.error_id + 1} </Tab>)}
                </TabList>
                <TabPanels>
                    {errors.map((error: TypeError, i) => <TypeError
                        key={i}
                        error={error}
                    />)}
                </TabPanels>
            </Tabs>
        </Box>)
};

export default Debugger
