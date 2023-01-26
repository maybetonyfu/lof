import React from "react"
import {useQuery, useQueryClient} from 'react-query'
import {fetchData} from './query'


function FileEntry({file}) {
    return <div className={'bg-stone-200 px-1 py-0.5'}>{file}</div>
}

function Sidebar() {
    const queryClient = useQueryClient()
    const {isLoading, isError, data, error} = useQuery('data', fetchData)
    if (isLoading) {

        return <span>Loading...</span>

    }
    if (isError) {
        return <span>Error: {error.message}</span>
    }

    let files = data.parsing_result.contents.map(i => i.file)
    return (<div>
        {files.map(f => {
            return <FileEntry file={f} key={f}></FileEntry>
        })}
    </div>)
}

export default Sidebar