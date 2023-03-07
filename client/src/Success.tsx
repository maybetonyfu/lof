import React from "react"
import {Box, Text} from "@chakra-ui/react";
import useAppStore from "./state";
import {AppStore} from "./global";

function Success () {
    const dirty = useAppStore((state: AppStore) => state.dirty)
    const openedFile = useAppStore((state: AppStore) => state.openedFile)
    if (openedFile === null) {
        return (<Box>
            <Text>Open a file.</Text>
        </Box>)
    } else if (dirty) {
        return (<Box>
            <Text>Your code changed. Please save.</Text>
        </Box>)
    } else {
        return (<Box>
            <Text>Your code looks good.</Text>
        </Box>)
    }

}

export default Success
