import React, {useEffect} from "react"
import useAppStore from "./state";
import { WrenchIcon } from '@heroicons/react/24/outline'
import {RuleSet, TypeError} from "./global";

import {Typography, Box, List, Paper, Card,CardContent, ListItem, ListItemButton,
    IconButton, ListItemText} from "@mui/material";

import {VisibilityOutlined} from '@mui/icons-material';
const Hint = ({replacement} : any) => {
    if (replacement.kind == "Type hole") {
        return (<div>
            Change <span className={'mx-1'}>{replacement.original}</span> to an instance of <span className={'mx-1'}>{replacement.signature}</span>
        </div>)
    } else if (replacement.kind == "Type wildcard") {
        return (<div>
            Change <span className={'mx-1'}>{replacement.original}</span>  to type <span className={'mx-1'}>{replacement.signature}</span>
        </div>)
    } else {
        return (<div>
            Change <span className={'mx-1'}>{replacement.original}</span>  to a different expression
        </div>)
    }

}
const Fix = ({fix, slices, number}: any) => {
    let activeFix = useAppStore((state : any) => state.fix)
    let chooseFix = useAppStore((state : any) => state.chooseFix)
    let replacements = useAppStore((state : any) => state.replacements)
    let setHighlight = useAppStore((state:any) => state.setHighlight)
    let active = activeFix === fix.setId
    let rules = fix.rules
    let activeSlices = slices.filter((slice: any) => rules.includes(slice.slice_id))

    return (
        <div className={'flex flex-col rounded-sm py-0.5 px-1 cursor-pointer my-0.5 ' + (active ? 'bg-amber-100' : 'bg-stone-100')}>
            <div
                className={'flex items-center'}
                onClick={_ => {
                    chooseFix(fix.setId)
                }}>
                <WrenchIcon className="h-6 w-6 "/>
                <span className={"ml-2"}>Fix {number + 1}</span>
            </div>
            {active ?
                <div className={"text-gray-700"}>This location appears in {activeSlices[0].appears.length} conflicts</div>
                :<></>
            }
            <div>
                {
                    replacements.map((r: any) => activeFix == fix.setId ? <Hint replacement={r} key={r.hole_id}/> : null)
                }
            </div>

        </div>)


}
const TypeError = ({error} : any) => {
    // let setHighlight = useAppStore((state:any) => state.setHighlight)
    let chooseFix = useAppStore((state : any) => state.chooseFix)
    let suggestions =  useAppStore((state : any) => state.suggestions)
    return (<Card sx={{ }} >
        <CardContent>
        <Typography>Type error {error.error_id + 1}</Typography>
        <Typography>Possible Fixes:</Typography>



        <List dense disablePadding sx={{bgcolor : 'grey.200',}}>
            {error.mcs_list.map((fix:RuleSet, i:number) => {

                return (<ListItem  key={i}
                                   secondaryAction={
                                       <IconButton edge="end"  onClick={_ => chooseFix(fix.setId)}>
                                           <VisibilityOutlined />
                                       </IconButton>
                                   }
                                 >
                    <ListItemButton>
                        <ListItemText primary={`Fix ${i + 1}`}></ListItemText>
                    </ListItemButton>
            </ListItem>)})}
        </List>
            <Paper sx={{marginTop: '10px', marginBottom: '10px'}}>
                <Typography>Suggestion:</Typography>
                {suggestions.map((s:string,i:number) => <Typography key={i}>{s}</Typography>) }
            </Paper>
            </CardContent>
    </Card>)
}
const Debugger = () => {
    const errors = useAppStore((state: any) => state.errors)
    const isLoading = useAppStore((state: any) => state.isLoading)
    if (isLoading) {
        return <div>Type checking in progress</div>
    }
    return (
    <Box sx={{bgcolor : 'grey.100', height: '100%', padding: '10px'}}>
        {errors.map((error: TypeError) => <TypeError   key={error.error_id} error={error}/>)}
    </Box>)
};

export default Debugger
