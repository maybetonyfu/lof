import React, {useEffect} from "react"
import Layout from "./Layout"
import Editor from "./Editor"
import Sidebar from "./Sidebar"
import Debugger from "./Debugger";
import useAppStore from "./state";
const App = () => {
    const setFileList = useAppStore((state: any) => state.setFileList)
    useEffect(() => {
        setFileList();
    }, [])
    return (
        <div>
        <Layout
            left={<Sidebar/>}
            middle={(<div className="h-full flex flex-col">
                <Editor></Editor>
            </div>)}
            right={<Debugger/>}
        >
        </Layout>
    </div>)
};

export default App
