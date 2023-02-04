import React from "react"
import useAppStore from "./state";
import List from '@mui/material/List';
import ListItem from '@mui/material/ListItem';
import ListItemIcon from "@mui/material/ListItemIcon"
import ListItemText from '@mui/material/ListItemText';
import ListItemButton from "@mui/material/ListItemButton"
import {InsertDriveFileOutlined} from '@mui/icons-material';
import Box from '@mui/material/Box';
function FileEntry({file, active}: any) {
    let readFile = useAppStore((state: any) => state.readFile)
    return (active ?
            <div
                className={'bg-amber-100  px-1 py-0.5 h-8 items-center flex'}>
                <span className={'ml-2'}>{file}</span></div> :
            <div onClick={() => readFile(file)}
                 className={'bg-stone-200  px-1 py-0.5 h-8 items-center flex'}>
                <span className={'ml-2'}>{file}</span></div>
    )
}

function Sidebar() {
    let fileList = useAppStore((state: any) => state.fileList)
    let openedFile = useAppStore((state: any) => state.openedFile)
    let readFile = useAppStore((state: any) => state.readFile)
    return (<Box sx={{height: '100%'}}>
        <List dense disablePadding  sx={{ bgcolor : 'grey.200',	 height: '100%'}}>
            {fileList.map((f: string, i: number) => (
                <ListItem disablePadding divider key={i}>
                    <ListItemButton onClick={_ => readFile(f)}>
                        <ListItemIcon>
                            <InsertDriveFileOutlined />
                        </ListItemIcon>
                        <ListItemText  primary={f} />

                        </ListItemButton>
                </ListItem>))}
        </List>
    </Box>)
}

export default Sidebar
