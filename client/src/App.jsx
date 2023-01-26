import React from "react"
import Layout from "./Layout"
import Editor from "./Editor"
import Sidebar from "./Sidebar"
import {

    QueryClient,

    QueryClientProvider,

} from 'react-query'

const queryClient = new QueryClient()

const App = () => {
    return (<QueryClientProvider client={queryClient}>


            <div className="h-full">
                <Layout
                    left={<Sidebar/>}
                    middle={(<div className="h-full flex flex-col">
                        <Editor> </Editor>
                    </div>)}
                    right={<div> Right </div>}
                >
                </Layout>
            </div>
        </QueryClientProvider>

    )
};

export default App