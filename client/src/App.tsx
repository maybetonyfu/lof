import React, {useEffect} from "react"
import Grid from '@mui/material/Unstable_Grid2';
import Layout from "./Layout"
import Editor from "./Editor"
import Sidebar from "./Sidebar"
import Debugger from "./Debugger";
import Paper from '@mui/material/Paper';
import useAppStore from "./state";
import Container from "@mui/material/Container"

const App = () => {
    const setFileList = useAppStore((state: any) => state.setFileList)
    useEffect(() => {
        setFileList();
    }, [])
    return (
        <Container maxWidth={false} disableGutters sx={{height: "100%"}}>
            <Grid container sx={{height: "100%"}}>
                <Grid xs={'auto'}>
                    <Sidebar/>
                </Grid>
                <Grid xs={6}>
                    <Editor/>
                </Grid>
                <Grid xs>
                    <Debugger />
                </Grid>
            </Grid>
        </Container>
    )
};

export default App
